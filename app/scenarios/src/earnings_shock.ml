open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module News_injector = Jsip_news_injector.News_injector
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Momentum_trader = Jsip_bots.Momentum_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

let name = "earnings-shock"

let description =
  "Single positive news shock partway through; market maker gets run over, \
   momentum trader chases the move."
;;

let symbol = Symbol.of_string "AAPL"
let initial_price_cents = 15000

(* Moderate volatility, and deliberately *weak* mean reversion: after the
   earnings jump we want the fundamental to stay near its new, higher level
   rather than being dragged straight back to [initial_price_cents]. That
   leaves a clean, persistent step-change for the momentum trader to chase
   and for the market maker to (belatedly) re-quote around. *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 3.0
        ; mean_reversion_strength = 0.02
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

(* The earnings report: a single large positive shock to the fundamental,
   fifteen seconds in. The market maker's quotes are driven off the
   fundamental but only re-seed once per tick, so for a beat after the jump
   its resting asks sit below the new fair value and get swept -- that's the
   maker "getting run over". *)
let earnings_news : News_injector.Event.t list =
  [ { at = Time_ns.Span.of_sec 15.0
    ; symbol
    ; delta_cents = 500
    ; description = "AAPL earnings report -- stock spikes"
    }
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

(* The victim. A wide [max_spread_cents] means it keeps quoting straight
   through the shock instead of standing aside, and a one-second tick means
   its ladder lags the instantaneous jump in fair value -- exactly the
   flat-footed maker the scenario is about. Its resting asks (priced off the
   pre-jump fundamental) get lifted before it can re-quote. *)
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

(* Background two-sided flow so the trade tape has prints before and after
   the shock -- the momentum trader reads [Trade_report]s, so this keeps its
   price window fed and gives the post-jump trend something to build on. *)
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

(* The winner of this scenario. A short window and modest threshold let it
   react within a couple of ticks once the post-jump trades start printing
   higher; its [Ioc] entries chase the move up, taking the maker's now-cheap
   asks. This is the trend-follower "loving" the jump that Exercise 7
   describes. *)
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
  ; news = earnings_news
  ; bots = [ market_maker_spec; noise_trader_spec; momentum_trader_spec ]
  }
;;
