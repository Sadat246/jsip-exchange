(** Scaffolding for bot tests. *)

open! Core
open! Async
open Jsip_types
open Jsip_fundamental
open Jsip_order_book
open Jsip_bot_runtime
open! Jsip_bots

let aapl = Symbol.of_string "AAPL"
let alice = Participant.of_string "Alice"

let oracle_config ~initial_price_cents =
  Symbol.Map.of_alist_exn
    [ ( aapl
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 0.0
        ; mean_reversion_strength = 0.0
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

(* Build a runtime around a bot module with a mock submit/cancel that records
   what the bot does. *)
let make_recording_bot
  (type cfg)
  (bot_module : (module Bot_runtime.Bot with type Config.t = cfg))
  (config : cfg)
  ?(initial_price_cents = 15000)
  ()
  =
  let submitted = ref [] in
  let cancelled = ref [] in
  let submit request =
    submitted := request :: !submitted;
    return (Ok ())
  in
  let cancel order_id =
    cancelled := order_id :: !cancelled;
    return (Ok ())
  in
  let oracle =
    Fundamental_oracle.create (oracle_config ~initial_price_cents) ~seed:42
  in
  let bot =
    Bot_runtime.create
      bot_module
      config
      ~participant:alice
      ~oracle
      ~rng:(Splittable_random.of_int 7)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.0)
  in
  bot, submitted, cancelled
;;

let print_submitted (submitted : Order.Request.t list ref) =
  let recent = List.rev !submitted in
  List.iter recent ~f:(fun req ->
    printf
      !"%{Side} %{Symbol} %d@%{Price#dollar} %{Time_in_force}\n"
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price
      req.time_in_force)
;;

(* Smoke test: drive the do-nothing reference bot through one event so the
   runtest target exercises the helpers above. Replace or extend with
   bot-specific tests as concrete strategies are added to [Jsip_bots]. *)
module Inert_bot = struct
  module Config = struct
    type t = unit
  end

  let name = "inert"
  let on_start () _ctx = return ()
  let on_tick () _ctx = return ()
  let on_event () _ctx _event = return ()
end

let%expect_test "make_recording_bot wires up a runnable bot" =
  let bot, submitted, _cancelled =
    make_recording_bot (module Inert_bot) () ()
  in
  let%bind () =
    Bot_runtime.feed_event
      bot
      (Order_accept
         { order_id = Order_id.For_testing.of_int 1
         ; request =
             { symbol = aapl
             ; participant = alice
             ; side = Buy
             ; price = Price.of_int_cents 15000
             ; size = Size.of_int 10
             ; time_in_force = Day
             ; client_order_id = Client_order_id.of_int 0
             }
         })
  in
  print_submitted submitted;
  [%expect {| |}];
  return ()
;;

(* One [on_tick] with the fundamental pinned at $150.00. Every order must rest
   ($5.00+ away from fair value, so non-marketable), be [Day], and carry a
   distinct client order id. With [level_spacing_cents = 10] successive pairs
   march onto new price levels; sides alternate so both halves of the book
   grow. *)
let%expect_test "book_filler floods non-marketable resting Day orders" =
  let config : Book_filler.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 6
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 10
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Book_filler) config ~initial_price_cents:15000 ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler.on_tick config context in
  List.iter (List.rev !submitted) ~f:(fun (req : Order.Request.t) ->
    printf
      !"cid=%{Client_order_id} %{Side} %{Symbol} %d@%{Price#dollar} \
        %{Time_in_force}\n"
      req.client_order_id
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price
      req.time_in_force);
  [%expect {|
    cid=0 BUY AAPL 1@$145.00 DAY
    cid=1 SELL AAPL 1@$155.00 DAY
    cid=2 BUY AAPL 1@$144.90 DAY
    cid=3 SELL AAPL 1@$155.10 DAY
    cid=4 BUY AAPL 1@$144.80 DAY
    cid=5 SELL AAPL 1@$155.20 DAY
    |}];
  return ()
;;
