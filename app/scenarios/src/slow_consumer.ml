open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module Market_maker_bot = Jsip_bots.Market_maker_bot_lijia
module Slow_consumer_bot = Jsip_bots.Slow_consumer

let name = "slow-consumer"

let description =
  "One symbol, a fast-quoting market maker, and one or more subscribers \
   that read their market-data feed too slowly to keep up — so events pile \
   up in the exchange-side buffer. Demonstrates the unbounded-buffering \
   pathology."
;;

let aapl = Symbol.of_string "AAPL"

(* A gently drifting fundamental so the market maker's quotes keep moving,
   which keeps market-data events flowing for the slow consumers to fall
   behind on. Deterministic given the runner's seed. *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( aapl
      , { Fundamental_oracle.Config.initial_price_cents = 15000
        ; volatility_cents_per_sec = 10.0
        ; mean_reversion_strength = 0.05
        ; tick_interval = Time_ns.Span.of_sec 0.5
        } )
    ]
;;

(* Build one market-maker instance. Each instance needs its own [participant]
   name — the gateway keys sessions by participant, so reusing a name would
   evict the earlier session — and its own [rng_seed]. *)
let market_maker_spec ~participant ~rng_seed =
  Bot_spec.T
    { bot = (module Market_maker_bot)
    ; config =
        Market_maker_bot.Config.create
          ~symbol:aapl
          ~half_spread_cents:5
          ~size_per_level:100
          ~num_levels:3
    ; participant
    ; symbols = [ aapl ]
    ; rng_seed
    ; (* Re-quote four times a second: a brisk stream of BBO updates. *)
      tick_interval = Time_ns.Span.of_sec 0.25
    ; is_marketdata_consumer = false
    }
;;

(* Build one slow-consumer instance. [read_delay] tunes how far behind this
   particular consumer falls: the bigger it is relative to the market maker's
   event rate, the faster this subscriber's exchange-side buffer grows. *)
let slow_consumer_spec ~participant ~rng_seed ~read_delay =
  Bot_spec.T
    { bot = (module Slow_consumer_bot)
    ; config = Slow_consumer_bot.Config.create ~read_delay
    ; participant
    ; symbols = [ aapl ]
    ; rng_seed
    ; tick_interval = Time_ns.Span.of_sec 5.0
    ; is_marketdata_consumer = true
    }
;;

(* The roster of consumers to launch side by side. Each entry is
   [(participant name, rng seed, per-event read delay)]. Distinct names and
   seeds let every instance run and lag independently; varied delays let you
   compare a mildly-slow consumer against a hopelessly-slow one in a single
   run. Add or remove rows to change the cast. *)
let consumer_roster =
  [ "SlowConsumer-2s", 2, Time_ns.Span.of_sec 2.0
  ; "SlowConsumer-5s", 3, Time_ns.Span.of_sec 5.0
  ]
;;

let configure () : Scenario_config.t =
  let market_makers =
    [ market_maker_spec
        ~participant:(Participant.of_string "MarketMaker")
        ~rng_seed:1
    ]
  in
  let slow_consumers =
    List.map consumer_roster ~f:(fun (name, rng_seed, read_delay) ->
      slow_consumer_spec
        ~participant:(Participant.of_string name)
        ~rng_seed
        ~read_delay)
  in
  { name
  ; symbols = [ aapl ]
  ; oracle_config
  ; news = []
  ; bots = market_makers @ slow_consumers
  }
;;
