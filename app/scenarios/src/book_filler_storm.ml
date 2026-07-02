open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

let name = "book-filler"

let description =
  "One Book_filler bot floods a single book with resting Day orders it never \
   intends to trade, growing order-book memory and snapshot/match latency."
;;

let symbol = Symbol.of_string "AAPL"

(* A deliberately aggressive configuration: every 100ms the bot adds 50 resting
   orders, each on a fresh price level ([level_spacing_cents = 1]), sitting at
   least $5 off the fundamental so nothing ever fills. Dial [orders_per_tick]
   or the [tick_interval] down for a gentler run. *)
let configure () : Scenario_config.t =
  let oracle_config =
    Symbol.Map.of_alist_exn
      [ ( symbol
        , { Fundamental_oracle.Config.initial_price_cents = 15000
          ; volatility_cents_per_sec = 5.0
          ; mean_reversion_strength = 0.1
          ; tick_interval = Time_ns.Span.of_sec 1.0
          } )
      ]
  in
  let book_filler_config : Jsip_bots.Book_filler.Config.t =
    { symbols = [ symbol ]
    ; orders_per_tick = 50
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 1
    ; next_client_order_id = ref 0
    }
  in
  { name
  ; symbols = [ symbol ]
  ; oracle_config
  ; news = []
  ; bots =
      [ Bot_spec.T
          { bot = (module Jsip_bots.Book_filler)
          ; config = book_filler_config
          ; participant = Participant.of_string "BookFiller"
          ; symbols = [ symbol ]
          ; rng_seed = 0
          ; tick_interval = Time_ns.Span.of_ms 100.0
          ; is_marketdata_consumer = false
          }
      ]
  }
;;
