open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module Spammer = Jsip_bots.Spammer
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Momentum_trader = Jsip_bots.Momentum_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

let name = "spam-storm"

let description =
  "Order spammer flooding the request queue, dispatcher, and subscriber \
   pipes, with a market maker and noise trader for organic activity."
;;

let symbol = Symbol.of_string "AAPL"
let initial_price_cents = 15000

let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
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

(* A fleet of specialized spammers. Instead of one generic flood, each bot
   below is tuned to hammer ONE shared resource as hard as it can, so that
   run together they load every path through the exchange at once. The knobs
   are not independent attacks -- [marketable_chance] picks *which*
   downstream work an order generates, and [time_in_force_distribution] picks
   whether it *rests* (memory) or *churns* (CPU). The four archetypes, and
   the resource each targets:

   - [fan-out-storm]: 100% marketable, Ioc -- subscriber pipes / dispatcher
   - [deep-sweep]: 100% marketable, Ioc -- matching-engine CPU per order
   - [book-bloat]: 0% marketable, Day -- order-book memory + scan cost
   - [queue-flood]: 0% marketable, Ioc -- bounded request-queue intake

   The spammers are also symbiotic: [book-bloat] rests thousands of orders
   across a wide price band, which is exactly the deep resting liquidity the
   two marketable spammers then sweep through. Because each spammer is a
   distinct participant (and the matching engine now prevents self-trades),
   their marketable orders cross each other's -- and the market maker's and
   noise trader's -- resting liquidity, so a single spammer never just fills
   itself. *)

(* Attacks the subscriber pipes and dispatcher fan-out (the unbounded
   [Pipe.write_without_pushback_if_open] path). Every order is marketable, so
   each one amplifies into Fill + Trade_report events plus a moving BBO --
   the most events-per-order the exchange can emit. [mean_size] is tiny so an
   order crosses ~one level and the burst is a stream of *many cheap crosses*
   (max event count, not deep sweeps); [Ioc] means nothing rests, so every
   tick is a fresh wave and the leftover cancels (one more event). Small
   [price_jitter] keeps orders near the touch so they reliably cross. *)
let fan_out_storm_params : Spammer.Config.resource_exhaustion_params =
  { orders_per_burst = 60
  ; buy_chance = Percent.of_percentage 50.
  ; marketable_chance = Percent.of_percentage 100.
  ; time_in_force_distribution = day_ioc_mix ~day_pct:0.
  ; mean_size = 3
  ; price_jitter_cents = 5
  }
;;

(* Attacks matching-engine CPU. Same marketable/[Ioc] shape as fan-out-storm,
   but [mean_size] is large and [price_jitter] crosses *deep* past the touch,
   so each order walks many resting levels in a single [match_loop] recursion
   -- long sweeps and many book removals per order. Fewer orders per burst
   because each one is individually expensive to match. *)
let deep_sweep_params : Spammer.Config.resource_exhaustion_params =
  { orders_per_burst = 30
  ; buy_chance = Percent.of_percentage 50.
  ; marketable_chance = Percent.of_percentage 100.
  ; time_in_force_distribution = day_ioc_mix ~day_pct:0.
  ; mean_size = 60
  ; price_jitter_cents = 30
  }
;;

(* Attacks order-book memory and per-match scan cost. Nothing is marketable,
   so every order rests; [Day] means it rests until end-of-day and never
   leaves. A wide [price_jitter] spreads the burst across ~400 distinct price
   levels, fattening the price->orders maps so every future [find_match] /
   [best_level] scans more. Balanced [buy_chance] bloats both sides. Large
   burst because the whole point is unbounded accumulation. *)
let book_bloat_params : Spammer.Config.resource_exhaustion_params =
  { orders_per_burst = 150
  ; buy_chance = Percent.of_percentage 50.
  ; marketable_chance = Percent.of_percentage 0.
  ; time_in_force_distribution = day_ioc_mix ~day_pct:100.
  ; mean_size = 5
  ; price_jitter_cents = 200
  }
;;

(* Attacks the bounded request queue itself -- raw intake rate. Everything is
   unmarketable *and* [Ioc], so each order does the least possible downstream
   work: it crosses nothing (no fills, no fan-out) and rests nothing (no
   memory), just Order_accept + Order_cancel and out. That isolates the
   attack onto submission throughput. Huge [orders_per_burst] paired with the
   tightest [tick_interval] below pins the queue faster than the single
   matching loop can drain it. *)
let queue_flood_params : Spammer.Config.resource_exhaustion_params =
  { orders_per_burst = 400
  ; buy_chance = Percent.of_percentage 50.
  ; marketable_chance = Percent.of_percentage 0.
  ; time_in_force_distribution = day_ioc_mix ~day_pct:0.
  ; mean_size = 1
  ; price_jitter_cents = 50
  }
;;

(* One [Bot_spec] per archetype. Participant name, RNG seed, and
   [tick_interval] live on the spec (the per-instance tuning point);
   queue-flood ticks fastest because its attack is purely about rate. Add
   more archetypes by adding a params record above and a row here. *)
let spammer_specs =
  List.map
    [ "fan-out-storm", 1001, Time_ns.Span.of_ms 10.0, fan_out_storm_params
    ; "deep-sweep", 1002, Time_ns.Span.of_ms 10.0, deep_sweep_params
    ; "book-bloat", 1003, Time_ns.Span.of_ms 10.0, book_bloat_params
    ; "queue-flood", 1004, Time_ns.Span.of_ms 2.0, queue_flood_params
    ]
    ~f:(fun (participant, seed, tick_interval, params) ->
      let behavior : Spammer.Config.behavior = Resource_exhaustion params in
      Bot_spec.T
        { bot = (module Spammer)
        ; config = Spammer.Config.create ~symbols:[ symbol ] ~behavior
        ; participant = Participant.of_string participant
        ; symbols = [ symbol ]
        ; rng_seed = seed
        ; tick_interval
        ; is_marketdata_consumer = true
        })
;;

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

(* A noise trader supplies organic two-sided activity so the book has real
   liquidity for the spammers and market maker to churn against. *)
let noise_trader_spec =
  Bot_spec.T
    { bot = (module Noise_trader)
    ; config =
        Noise_trader.Config.create
          ~symbols:[ symbol ]
          ~mean_size:8
          ~tick_chance:(Percent.of_percentage 80.)
          ~aggressiveness:(Percent.of_percentage 50.)
          ~time_in_force_distribution:(day_ioc_mix ~day_pct:60.)
    ; participant = Participant.of_string "noise-trader"
    ; symbols = [ symbol ]
    ; rng_seed = 3001
    ; tick_interval = Time_ns.Span.of_ms 200.0
    ; is_marketdata_consumer = true
    }
;;

(* A momentum trader adds trend-following organic flow: it reads the public
   trade tape and chases sustained moves. Under the spammers' flood its
   window fills with their prints, so it reacts to the artificial volatility
   they inject -- letting us watch a strategy bot behave against a market
   being actively attacked, not just the market maker and noise trader. *)
let momentum_trader_spec =
  Bot_spec.T
    { bot = (module Momentum_trader)
    ; config =
        Momentum_trader.Config.create_exn
          ~symbol
          ~window_capacity:5
          ~threshold_cents:15
          ~max_order_size:25
          ~max_position:200
          ~cooldown_ticks:1
          ()
    ; participant = Participant.of_string "momentum-trader"
    ; symbols = [ symbol ]
    ; rng_seed = 4001
    ; tick_interval = Time_ns.Span.of_ms 500.0
    ; is_marketdata_consumer = true
    }
;;

let configure () : Scenario_config.t =
  { name
  ; symbols = [ symbol ]
  ; oracle_config
  ; news = []
  ; bots =
      spammer_specs
      @ [ market_maker_spec; noise_trader_spec; momentum_trader_spec ]
  }
;;
