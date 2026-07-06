open! Core
open Jsip_types
open Jsip_scenario_runner

let name = "cancel-storm"

let description =
  "Cancel storm: several bots submit and immediately cancel on a tight \
   loop, hammering the cancel path, against a market maker and a noise \
   trader."
;;

(* A single liquid symbol at $150.00 keeps the pathology easy to watch. *)
let aapl = Symbol.of_string "AAPL"
let symbols = [ aapl ]

(* The market maker quotes with this half-spread when it has no BBO (its
   default), so the storm's marketable orders must reach past it to cross. *)
let market_maker_half_spread_cents = 50

let oracle_config : Jsip_fundamental.Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( aapl
      , { Jsip_fundamental.Fundamental_oracle.Config.initial_price_cents =
            15000
        ; volatility_cents_per_sec = 10.0
        ; mean_reversion_strength = 0.1
        ; tick_interval = Time_ns.Span.of_sec 0.2
        } )
    ]
;;

(* A market maker gives the storm a two-sided book to cancel against and to
   fill against for the marketable fraction. *)
let market_maker_spec =
  Bot_spec.T
    { bot = (module Jsip_bots.Robyn_market_maker_bot)
    ; config =
        Jsip_bots.Robyn_market_maker_bot.create_config
          ()
          ~size_per_level:200
          ~num_levels:5
          ~inventory_skew_cents_per_share:2
          ~symbols
    ; participant = Participant.of_string "Market Maker"
    ; symbols
    ; rng_seed = 100
    ; tick_interval = Time_ns.Span.of_sec 1.0
    ; is_marketdata_consumer = true
    }
;;

(* A noise trader adds organic churn so the storm is not the only activity. *)
let noise_trader_spec =
  Bot_spec.T
    { bot = (module Jsip_bots.Robyn_noise_trader)
    ; config =
        Jsip_bots.Robyn_noise_trader.create_config
          ~symbols
          ~avg_size:100
          ~tick_chance:0.7
          ~aggressiveness_pct:40
          ~ioc_pct:20
    ; participant = Participant.of_string "Noise Trader"
    ; symbols
    ; rng_seed = 200
    ; tick_interval = Time_ns.Span.of_sec 0.25
    ; is_marketdata_consumer = true
    }
;;

(* 25 cycles every 0.1s per bot -> ~250 submit+cancel pairs/sec each.
   Distinct names and seeds so the copies churn independently. *)
let cancel_storm_spec ~index =
  Bot_spec.T
    { bot = (module Jsip_bots.Cancel_storm)
    ; config =
        Jsip_bots.Cancel_storm.create_config
          ~symbols
          ~cycles_per_tick:25
          ~size:50
          ~pct_marketable:20
          ~price_offset_cents:(market_maker_half_spread_cents + 50)
    ; participant =
        Participant.of_string [%string "Cancel Storm %{index#Int}"]
    ; symbols
    ; rng_seed = 300 + index
    ; tick_interval = Time_ns.Span.of_sec 0.1
    ; is_marketdata_consumer = false
    }
;;

let num_storms = 3

let configure () : Scenario_config.t =
  { name
  ; symbols
  ; oracle_config
  ; news = []
  ; bots =
      market_maker_spec
      :: noise_trader_spec
      :: List.init num_storms ~f:(fun i -> cancel_storm_spec ~index:(i + 1))
  }
;;
