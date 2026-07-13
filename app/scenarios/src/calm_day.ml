open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

let name = "calm-day"

let description =
  "Quiet single-symbol market: one market maker, one noise trader, no news."
;;

(* The engine assigns ids by position, so AAPL (the only symbol) is id 0.
   [symbol] is the id used for orders and the oracle; [symbol_name] is the
   name handed to the engine via {!Scenario_config.symbols}. *)
let symbol = Symbol_id.Private.of_int 0
let symbol_name = Symbol.of_string "AAPL"
let initial_price_cents = 15000

(* Modest volatility and gentle mean reversion: the fundamental drifts
   quietly around its starting value with no dramatic moves. There are no
   news shocks in this scenario, so this OU noise is the *only* thing moving
   fair value -- keeping it small is what makes the day "calm". *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol_id.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 3.0
        ; mean_reversion_strength = 0.05
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

(* A time-in-force distribution: [day_pct]% resting [Day] orders, the balance
   [Ioc]. Written as a distribution (rather than a single Ioc probability) so
   a new order type is mixed in by adding an entry, not by changing a bot. *)
let day_ioc_mix ~day_pct =
  [ Time_in_force.Day, Percent.of_percentage day_pct
  ; Ioc, Percent.of_percentage (100. -. day_pct)
  ]
;;

(* A standard two-sided market maker. Its wide [max_spread_cents] tolerance
   means it keeps quoting through the whole (uneventful) day rather than ever
   standing aside -- there is no whale here to blow the spread out. *)
let market_maker_spec =
  Bot_spec.T
    { bot = (module Market_maker_bot)
    ; config =
        Market_maker_bot.Config.create
          ~symbols:[ symbol ]
          ~size_per_level:10
          ~num_levels:5
          ~inventory_skew_cents_per_share:1
          ~half_spread_cents:10
          ~min_half_spread_cents:2
          ~max_spread_cents:500
    ; participant = Participant.of_string "market-maker"
    ; symbols = [ symbol ]
    ; rng_seed = 2001
    ; tick_interval = Time_ns.Span.of_sec 1.0
    ; is_marketdata_consumer = true
    }
;;

(* One unhurried noise trader. Small sizes, a slow tick, and a coin-flip
   [aggressiveness] keep a steady but sparse trickle of orders crossing the
   maker's quotes -- enough to see a continuous stream of fills in the
   monitor without the book ever getting busy. A [day_pct] of 70 means most
   of its orders rest, so the book stays populated between crossings. *)
let noise_trader_spec =
  Bot_spec.T
    { bot = (module Noise_trader)
    ; config =
        Noise_trader.Config.create
          ~symbols:[ symbol ]
          ~mean_size:6
          ~tick_chance:(Percent.of_percentage 50.)
          ~aggressiveness:(Percent.of_percentage 45.)
          ~time_in_force_distribution:(day_ioc_mix ~day_pct:70.)
    ; participant = Participant.of_string "noise-trader"
    ; symbols = [ symbol ]
    ; rng_seed = 3001
    ; tick_interval = Time_ns.Span.of_ms 400.0
    ; is_marketdata_consumer = true
    }
;;

let configure () : Scenario_config.t =
  { name
  ; symbols = [ symbol_name ]
  ; oracle_config
  ; news = []
  ; bots = [ market_maker_spec; noise_trader_spec ]
  }
;;
