open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

let name = "active-day"

let description =
  "Multi-symbol busy market: several market makers and a higher-throughput \
   noise trader."
;;

(* The whole scenario is driven off this one table: each row is a symbol, its
   starting/mean price, and the RNG seed for that symbol's market maker.
   Every derived structure below (the symbol list, the oracle config, the
   per-symbol market makers) is a [List.map] over it, so adding a fourth
   symbol is a single new row -- it automatically gets an oracle entry, its
   own market maker, and market data for the shared noise trader. *)
let symbol_table =
  [ Symbol.of_string "AAPL", 15000, 2001
  ; Symbol.of_string "GOOG", 28000, 2002
  ; Symbol.of_string "MSFT", 41000, 2003
  ]
;;

let symbols = List.map symbol_table ~f:(fun (symbol, _, _) -> symbol)

(* Symbol ids are assigned by position in [symbols] -- the list handed to the
   engine via {!Scenario_config.symbols} -- so the [i]th symbol has id [i].
   Orders, market-data subscriptions, and the oracle are all keyed by id. *)
let symbol_ids =
  List.mapi symbols ~f:(fun index _ -> Symbol_id.Private.of_int index)
;;

(* Moderate volatility on every symbol so all three books stay lively at once
   -- an "active" day is one where there is always something happening
   somewhere. *)
let oracle_config : Fundamental_oracle.Config.t =
  List.mapi symbol_table ~f:(fun index (_symbol, initial_price_cents, _) ->
    ( Symbol_id.Private.of_int index
    , { Fundamental_oracle.Config.initial_price_cents
      ; volatility_cents_per_sec = 4.0
      ; mean_reversion_strength = 0.05
      ; tick_interval = Time_ns.Span.of_sec 1.0
      } ))
  |> Symbol_id.Map.of_alist_exn
;;

(* A time-in-force distribution: [day_pct]% resting [Day] orders, the balance
   [Ioc]. Written as a distribution (rather than a single Ioc probability) so
   a new order type is mixed in by adding an entry, not by changing a bot. *)
let day_ioc_mix ~day_pct =
  [ Time_in_force.Day, Percent.of_percentage day_pct
  ; Ioc, Percent.of_percentage (100. -. day_pct)
  ]
;;

(* One dedicated market maker per symbol, each quoting (and consuming market
   data for) only its own symbol. Distinct participant names and seeds keep
   them independent, and a self-trade-preventing engine means a maker never
   fills against itself -- the crossing flow comes from the noise trader. *)
let market_maker_specs =
  List.mapi symbol_table ~f:(fun index (symbol, _, seed) ->
    let symbol_id = Symbol_id.Private.of_int index in
    Bot_spec.T
      { bot = (module Market_maker_bot)
      ; config =
          Market_maker_bot.Config.create
            ~symbols:[ symbol_id ]
            ~size_per_level:10
            ~num_levels:5
            ~inventory_skew_cents_per_share:1
            ~half_spread_cents:8
            ~min_half_spread_cents:2
            ~max_spread_cents:500
      ; participant =
          Participant.of_string [%string "market-maker-%{symbol#Symbol}"]
      ; symbols = [ symbol_id ]
      ; rng_seed = seed
      ; tick_interval = Time_ns.Span.of_sec 1.0
      ; is_marketdata_consumer = true
      })
;;

(* A single high-throughput noise trader spanning every symbol: it ticks
   fast, trades larger sizes, and crosses the spread half the time, so all
   three books see heavy two-sided flow from one bot. Because it subscribes
   to every symbol's market data, its internal BBO cache stays fresh across
   the board. *)
let noise_trader_spec =
  Bot_spec.T
    { bot = (module Noise_trader)
    ; config =
        Noise_trader.Config.create
          ~symbols:symbol_ids
          ~mean_size:12
          ~tick_chance:(Percent.of_percentage 90.)
          ~aggressiveness:(Percent.of_percentage 55.)
          ~time_in_force_distribution:(day_ioc_mix ~day_pct:55.)
    ; participant = Participant.of_string "noise-trader"
    ; symbols = symbol_ids
    ; rng_seed = 3001
    ; tick_interval = Time_ns.Span.of_ms 100.0
    ; is_marketdata_consumer = true
    }
;;

let configure () : Scenario_config.t =
  { name
  ; symbols
  ; oracle_config
  ; news = []
  ; bots = market_maker_specs @ [ noise_trader_spec ]
  }
;;
