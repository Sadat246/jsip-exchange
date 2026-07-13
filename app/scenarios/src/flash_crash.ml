open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module News_injector = Jsip_news_injector.News_injector
module Spammer = Jsip_bots.Spammer
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Momentum_trader = Jsip_bots.Momentum_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

let name = "flash-crash"

let description =
  "Tight sequence of large negative shocks plus a sell-heavy whale; market \
   makers pull quotes and liquidity collapses."
;;

(* The engine assigns ids by position, so AAPL (the only symbol) is id 0.
   [symbol] is the id used for orders, the oracle, and news; [symbol_name] is
   the name handed to the engine via {!Scenario_config.symbols}. *)
let symbol = Symbol_id.Private.of_int 0
let symbol_name = Symbol.of_string "AAPL"
let initial_price_cents = 15000

(* Higher volatility and weak mean reversion: we want the market jumpy and,
   once the crash starts, we do *not* want the fundamental snapping back to
   its starting level -- the point is a sustained collapse, not a dip. *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol_id.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 5.0
        ; mean_reversion_strength = 0.02
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

(* The trigger: a tight burst of large *negative* shocks a couple of seconds
   apart, modelling the accelerating bad news of a real cascade. The three
   descriptions cycle through the phases the exercise suggests -- a fund
   dumping, other holders stop-loss selling, then dealers giving up -- so the
   [News_injector]'s stdout narration reads like a crash unfolding. *)
let crash_news : News_injector.Event.t list =
  let step_cents = -300 in
  let first_at = Time_ns.Span.of_sec 8.0 in
  let step_every = Time_ns.Span.of_sec 2.0 in
  let steps = 5 in
  let descriptions =
    [| "large fund liquidation"
     ; "stop-loss cascade"
     ; "dealers pull quotes"
    |]
  in
  List.init steps ~f:(fun i ->
    { News_injector.Event.at =
        Time_ns.Span.( + )
          first_at
          (Time_ns.Span.scale step_every (Float.of_int i))
    ; symbol
    ; delta_cents = step_cents
    ; description = descriptions.(i % Array.length descriptions)
    })
;;

(* A time-in-force distribution: [day_pct]% resting [Day] orders, the balance
   [Ioc]. Written as a distribution (rather than a single Ioc probability) so
   a new order type is mixed in by adding an entry, not by changing a bot. *)
let day_ioc_mix ~day_pct =
  [ Time_in_force.Day, Percent.of_percentage day_pct
  ; Ioc, Percent.of_percentage (100. -. day_pct)
  ]
;;

(* The whale. There is no dedicated whale bot in the project, so we realize
   one with the [Spammer]'s resource-exhaustion behavior biased hard to the
   sell side: a [buy_chance] of 10% makes ~90% of its orders sells, every
   order is marketable and [Ioc], and a large [mean_size] with deep
   [price_jitter] means each one sweeps several resting levels before
   cancelling the remainder. Ticking a few times a second, it dumps size
   continuously through the crash. Each swept side leaves the book one-sided,
   which is what blows the spread out past the makers' tolerance below. *)
let whale_spec =
  let params : Spammer.Config.resource_exhaustion_params =
    { orders_per_burst = 12
    ; buy_chance = Percent.of_percentage 10.
    ; marketable_chance = Percent.of_percentage 100.
    ; time_in_force_distribution = day_ioc_mix ~day_pct:0.
    ; mean_size = 40
    ; price_jitter_cents = 30
    }
  in
  Bot_spec.T
    { bot = (module Spammer)
    ; config =
        Spammer.Config.create
          ~symbols:[ symbol ]
          ~behavior:(Resource_exhaustion params)
    ; participant = Participant.of_string "whale"
    ; symbols = [ symbol ]
    ; rng_seed = 5001
    ; tick_interval = Time_ns.Span.of_ms 250.0
    ; is_marketdata_consumer = true
    }
;;

(* Two market makers on the one symbol, so "makers pull quotes" is visible as
   a collective withdrawal rather than a single bot. The key knob is
   a *tight* [max_spread_cents]: once the whale sweeps a side and the
   observed spread blows past 250 cents, both makers stand aside instead of
   quoting into the dislocation -- and with the makers gone, liquidity
   collapses and the crash accelerates. *)
let market_maker_specs =
  List.map
    [ "market-maker-a", 2001; "market-maker-b", 2002 ]
    ~f:(fun (participant, seed) ->
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
              ~max_spread_cents:250
        ; participant = Participant.of_string participant
        ; symbols = [ symbol ]
        ; rng_seed = seed
        ; tick_interval = Time_ns.Span.of_sec 1.0
        ; is_marketdata_consumer = true
        })
;;

(* Background flow so there is genuine two-sided liquidity for the whale to
   eat into (and so the book isn't empty before the crash even starts). *)
let noise_trader_spec =
  Bot_spec.T
    { bot = (module Noise_trader)
    ; config =
        Noise_trader.Config.create
          ~symbols:[ symbol ]
          ~mean_size:8
          ~tick_chance:(Percent.of_percentage 80.)
          ~aggressiveness:(Percent.of_percentage 50.)
          ~time_in_force_distribution:(day_ioc_mix ~day_pct:50.)
    ; participant = Participant.of_string "noise-trader"
    ; symbols = [ symbol ]
    ; rng_seed = 3001
    ; tick_interval = Time_ns.Span.of_ms 200.0
    ; is_marketdata_consumer = true
    }
;;

(* A momentum trader turns the crash into a cascade. Reading the public tape
   of ever-lower prints, its signal points down and it sells into the fall --
   the trend-follower amplifying the move, exactly the "others see it falling
   and sell too" dynamic the exercise describes. *)
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
  ; symbols = [ symbol_name ]
  ; oracle_config
  ; news = crash_news
  ; bots =
      (whale_spec :: market_maker_specs)
      @ [ noise_trader_spec; momentum_trader_spec ]
  }
;;
