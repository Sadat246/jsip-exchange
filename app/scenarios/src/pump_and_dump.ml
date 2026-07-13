open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module Spammer = Jsip_bots.Spammer
module Noise_trader = Jsip_bots.Noise_trader_hansel
module Momentum_trader = Jsip_bots.Momentum_trader_hansel
module Market_maker_bot = Jsip_bots.Market_maker_bot_hansel

(* A single-symbol market with four participants, arranged to show a
   pump-and-dump extracting money from a price-chaser:

   - the [pump-and-dumper] ({!Spammer} in its [Pump_and_dump] behavior) walks
     AAPL up on marketable buys, then dumps its inventory once the mid has
     risen;
   - the [momentum-trader] reads the public tape and chases the rising price
     -- it is the intended victim, buying near the top and left holding as
     the price falls back;
   - the [market-maker] quotes a ladder anchored on the *fundamental*, so it
     is NOT fooled into repricing upward: it supplies the offers the pump
     lifts on the way up and mostly refuses to chase. This is the control --
     against it alone the scheme is barely profitable;
   - the [noise-trader] supplies organic two-sided flow, so the book has real
     resting bids for the dump to land in and the manipulation hides in
     noise.

   The contrast between the momentum trader (chases, gets hurt) and the
   market maker (value-anchored, mostly doesn't) is the whole point: the
   scheme only extracts money from participants who trade on price rather
   than value. *)

let name = "pump-and-dump"

let description =
  "A pump-and-dump bot walks a price up and dumps into a momentum trader \
   that chased the move, while a fundamental-anchored market maker mostly \
   refuses the bait."
;;

(* AAPL is the only symbol, so the engine assigns it id 0. [symbol] is the id
   used for orders, the oracle, and news; [symbol_name] is the name handed to
   the engine via {!Scenario_config.symbols}. *)
let symbol = Symbol_id.Private.of_int 0
let symbol_name = Symbol.of_string "AAPL"
let initial_price_cents = 15000

(* Low volatility and weak mean reversion so the pump's signal stands out and
   the manipulated price is allowed to drift rather than snapping straight
   back to fundamental against the scheme. *)
let oracle_config : Fundamental_oracle.Config.t =
  Symbol_id.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 1.5
        ; mean_reversion_strength = 0.02
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

let day_ioc_mix ~day_pct =
  [ Time_in_force.Day, Percent.of_percentage day_pct
  ; Ioc, Percent.of_percentage (100. -. day_pct)
  ]
;;

(* The manipulator. Every knob is chosen relative to the other bots below:
   [clip_size] is 3x the market maker's [size_per_level] so each buy walks
   several offer levels; [pump_target_pct] of 1% (a 150-cent move off 150.00)
   is well above the momentum trader's 10-cent trigger, so the pump trips the
   victim long before the target is hit; [max_inventory] of 150 is a long the
   momentum trader (max_position 250) plus the noise-trader bids can
   plausibly absorb on the dump; [give_up_ticks] of 40 (~10s at this tick
   rate) gives the pump time to work but guarantees an unwind if it stalls. *)
let pump_and_dump_spec =
  let params =
    Spammer.Config.pump_and_dump_params
      ~target_symbol:symbol
      ~pump_target_pct:(Percent.of_percentage 1.0)
      ~clip_size:30
      ~max_inventory:150
      ~give_up_ticks:40
      ~aggression_offset_cents:3
      ~entry_time_in_force:Ioc
  in
  Bot_spec.T
    { bot = (module Spammer)
    ; config =
        Spammer.Config.create
          ~symbols:[ symbol ]
          ~behavior:(Pump_and_dump params)
    ; participant = Participant.of_string "pump-and-dumper"
    ; symbols = [ symbol ]
    ; rng_seed = 5001
    ; tick_interval = Time_ns.Span.of_ms 250.0
    ; is_marketdata_consumer = true
    }
;;

(* The victim: a short window and a low threshold make it eager to chase the
   pump's rising prints, and a large [max_position] lets it keep buying into
   the top -- exactly the counterparty the dump needs. *)
let momentum_trader_spec =
  Bot_spec.T
    { bot = (module Momentum_trader)
    ; config =
        Momentum_trader.Config.create_exn
          ~symbol
          ~window_capacity:4
          ~threshold_cents:10
          ~max_order_size:30
          ~max_position:250
          ~cooldown_ticks:0
          ()
    ; participant = Participant.of_string "momentum-trader"
    ; symbols = [ symbol ]
    ; rng_seed = 4001
    ; tick_interval = Time_ns.Span.of_ms 300.0
    ; is_marketdata_consumer = true
    }
;;

(* The control: a fundamental-anchored ladder. It supplies the offers the
   pump lifts and the bids the dump hits, but reprices off the oracle, not
   the last trade, so it does not chase the manipulated price. *)
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

(* Organic two-sided flow so the book has real resting liquidity beyond the
   market maker -- bids for the dump to land in, and cover for the pump. *)
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
  ; symbols = [ symbol_name ]
  ; oracle_config
  ; news = []
  ; bots =
      [ pump_and_dump_spec
      ; momentum_trader_spec
      ; market_maker_spec
      ; noise_trader_spec
      ]
  }
;;
