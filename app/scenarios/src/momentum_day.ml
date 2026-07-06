open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module News_injector = Jsip_news_injector.News_injector
module Momentum_trader = Jsip_bots.Momentum_trader_hansel
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

let name = "momentum-day"

let description =
  "Scripted up-then-down trend that the momentum trader chases, with a \
   market maker quoting and a noise trader adding organic activity."
;;

let symbol = Symbol.of_string "AAPL"
let initial_price_cents = 15000

(* Low intrinsic volatility and gentle mean reversion, so the fundamental
   drifts quietly on its own. The visible price *moves* in this scenario come
   from the scripted news shocks below, not from the oracle's own noise --
   that keeps the momentum trader's signal driven by the trend we scripted
   rather than by random wiggles. *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 1.0
        ; mean_reversion_strength = 0.02
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

(* A staircase of shocks: a run of positive steps builds a sustained uptrend,
   then a run of negative steps reverses it into a downtrend. Each step is
   larger than the momentum trader's [threshold_cents] and they arrive a few
   seconds apart, so the trader's price window fills with a clear one-way
   move and its signal crosses the threshold in each direction in turn -- it
   should buy into the rise and sell into the fall. Priced in a single place
   so the trend's shape is easy to retune. *)
let trend_news =
  let step_cents = 40 in
  let step_every = Time_ns.Span.of_sec 3.0 in
  let up_steps = 6 in
  let down_steps = 6 in
  let event ~index ~delta ~label : News_injector.Event.t =
    { at = Time_ns.Span.scale step_every (Float.of_int index)
    ; symbol
    ; delta_cents = delta
    ; description = label
    }
  in
  let ups =
    List.init up_steps ~f:(fun i ->
      event ~index:(i + 1) ~delta:step_cents ~label:"uptrend step (+)")
  in
  let downs =
    List.init down_steps ~f:(fun i ->
      event
        ~index:(up_steps + i + 1)
        ~delta:(-step_cents)
        ~label:"downtrend step (-)")
  in
  ups @ downs
;;

(* The star of the scenario. A short window and a modest threshold make it
   react within a couple of steps of a trend; [cooldown_ticks] keeps one
   sustained move from firing on literally every tick. [Ioc] entries take
   whatever the market maker and noise trader are resting near the touch and
   cancel the rest, so the trader's position tracks the trend without leaving
   stale orders behind. *)
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

(* Quotes both sides so the momentum trader's marketable [Ioc] entries always
   have resting liquidity to hit, and so the book has a sensible spread as
   the fundamental walks with the news. *)
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

(* Organic two-sided flow so the trade tape has prints between shocks -- the
   momentum trader reads [Trade_report]s, so this keeps its price window fed
   even during the quiet stretches between news steps. *)
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

let configure () : Scenario_config.t =
  { name
  ; symbols = [ symbol ]
  ; oracle_config
  ; news = trend_news
  ; bots = [ momentum_trader_spec; market_maker_spec; noise_trader_spec ]
  }
;;
