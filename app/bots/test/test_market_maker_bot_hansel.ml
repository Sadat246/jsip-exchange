(** Expect tests for {!Jsip_bots.Market_maker_bot_hansel}.

    This maker adapts its half-spread to the market and stands aside when the
    book dislocates, so most tests feed a [Best_bid_offer_update] to set the
    market state before checking how it re-quotes. It also tracks its resting
    orders from [Order_accept] events (not at submit time), so [accept_all]
    replays accepts for the seeded ladder before a re-quote is expected to
    cancel it. Ladders are printed with [print_ladder]: the per-level
    buy/sell submits race, so submission order is unspecified. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
open! Jsip_bots
open Bot_harness

let bob = Participant.of_string "Bob"

let make_config
  ?(size_per_level = 10)
  ?(num_levels = 3)
  ?(inventory_skew_cents_per_share = 0)
  ?(half_spread_cents = 30)
  ?(min_half_spread_cents = 5)
  ?(max_spread_cents = 100)
  ()
  =
  Market_maker_bot_hansel.Config.create
    ~symbols:[ aapl ]
    ~size_per_level
    ~num_levels
    ~inventory_skew_cents_per_share
    ~half_spread_cents
    ~min_half_spread_cents
    ~max_spread_cents
;;

(* Replay an [Order_accept] for every order submitted so far, so the maker's
   internal book of resting orders matches what it quoted. It records resting
   orders from accepts (not at submit time) and only cancels ids it believes
   are resting, so without this a re-quote would have nothing to cancel. *)
let accept_all bot submitted =
  feed_events
    bot
    (List.rev_map !submitted ~f:(fun (request : Order.Request.t) ->
       Exchange_event.Order_accept
         { order_id =
             Order_id.For_testing.of_int
               (Client_order_id.to_int request.client_order_id)
         ; participant = alice
         ; request
         }))
;;

(* A book [half_width] cents to each side of $150.00, i.e. a spread of
   [2 * half_width] cents. *)
let bbo_event ~half_width =
  Exchange_event.Best_bid_offer_update
    { symbol = aapl
    ; bbo =
        { bid =
            Some
              { price = Price.of_int_cents (15000 - half_width)
              ; size = Size.of_int 100
              }
        ; ask =
            Some
              { price = Price.of_int_cents (15000 + half_width)
              ; size = Size.of_int 100
              }
        }
    }
;;

(* [alice] trades [size] shares on [alice_side] as the aggressor. The
   [aggressor_client_order_id] is one the maker isn't resting, so the fill
   only moves inventory and triggers a re-quote -- it doesn't retire a
   tracked order. *)
let fill ~alice_side ~size =
  Exchange_event.Fill
    { fill_id = 1
    ; symbol = aapl
    ; price = Price.of_int_cents 15000
    ; size = Size.of_int size
    ; aggressor_order_id = Order_id.For_testing.of_int 1
    ; aggressor_client_order_id = Client_order_id.of_int 999
    ; aggressor_participant = alice
    ; aggressor_side = alice_side
    ; resting_order_id = Order_id.For_testing.of_int 2
    ; resting_client_order_id = Client_order_id.of_int 998
    ; resting_participant = bob
    }
;;

let%expect_test "on_start seeds a symmetric ladder at the initial \
                 half-spread"
  =
  let config = make_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Market_maker_bot_hansel) config ()
  in
  let%bind () = Bot_runtime.For_testing.manual_start bot in
  print_ladder submitted;
  [%expect
    {|
    BUY 0 10@$149.68 DAY
    BUY 0 10@$149.69 DAY
    BUY 0 10@$149.70 DAY
    SELL 0 10@$150.30 DAY
    SELL 0 10@$150.31 DAY
    SELL 0 10@$150.32 DAY
    |}];
  return ()
;;

let%expect_test "the half-spread adapts to the market and re-quotes tighter" =
  let config = make_config ~num_levels:2 () in
  let bot, submitted, cancelled =
    make_recording_bot (module Market_maker_bot_hansel) config ()
  in
  let%bind () = Bot_runtime.For_testing.manual_start bot in
  let%bind () = accept_all bot submitted in
  submitted := [];
  (* A 20c-wide market (half-spread 10c) is tighter than our starting 30c, so
     we re-quote at 10c and cancel the old, wider ladder. *)
  let%bind () = Bot_runtime.feed_event bot (bbo_event ~half_width:10) in
  printf "cancelled %d, re-quoted:\n" (List.length !cancelled);
  print_ladder submitted;
  [%expect
    {|
    cancelled 4, re-quoted:
    BUY 0 10@$149.89 DAY
    BUY 0 10@$149.90 DAY
    SELL 0 10@$150.10 DAY
    SELL 0 10@$150.11 DAY
    |}];
  return ()
;;

let%expect_test "a fill skews the re-quoted ladder by inventory" =
  let config =
    make_config
      ~num_levels:1
      ~inventory_skew_cents_per_share:3
      ~half_spread_cents:10
      ~min_half_spread_cents:10
      ()
  in
  let bot, submitted, cancelled =
    make_recording_bot (module Market_maker_bot_hansel) config ()
  in
  let%bind () = Bot_runtime.For_testing.manual_start bot in
  let%bind () = accept_all bot submitted in
  (* Make the market healthy so a fill will re-quote. A 20c book holds the
     half-spread at its 10c floor and matches our current quote, so this by
     itself triggers no re-quote. *)
  let%bind () = Bot_runtime.feed_event bot (bbo_event ~half_width:10) in
  submitted := [];
  (* Buy 10 -> long 10 -> center skews down 30c (10 * 3c). *)
  let%bind () = Bot_runtime.feed_event bot (fill ~alice_side:Buy ~size:10) in
  printf
    "after buying 10, re-quoted (cancelled %d):\n"
    (List.length !cancelled);
  print_ladder submitted;
  [%expect
    {|
    after buying 10, re-quoted (cancelled 2):
    BUY 0 10@$149.60 DAY
    SELL 0 10@$149.80 DAY
    |}];
  return ()
;;

let%expect_test "the maker stands aside on a dislocated book, then recovers" =
  let config =
    make_config ~num_levels:2 ~half_spread_cents:20 ~max_spread_cents:40 ()
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Market_maker_bot_hansel) config ()
  in
  let%bind () = Bot_runtime.For_testing.manual_start bot in
  let%bind () = accept_all bot submitted in
  submitted := [];
  (* A 60c-wide book exceeds our 40c tolerance -- a whale swept a side. Stop
     quoting. *)
  let%bind () = Bot_runtime.feed_event bot (bbo_event ~half_width:30) in
  printf "on a 60c-wide book: submitted %d\n" (List.length !submitted);
  (* Stay aside even when a fill arrives. *)
  let%bind () = Bot_runtime.feed_event bot (fill ~alice_side:Buy ~size:10) in
  printf
    "after a fill while dislocated: submitted %d\n"
    (List.length !submitted);
  (* Once the book tightens back inside tolerance, re-quote. *)
  let%bind () = Bot_runtime.feed_event bot (bbo_event ~half_width:10) in
  printf
    "after the book recovers to 20c wide: re-quoted %d orders\n"
    (List.length !submitted);
  print_ladder submitted;
  [%expect
    {|
    on a 60c-wide book: submitted 0
    after a fill while dislocated: submitted 0
    after the book recovers to 20c wide: re-quoted 4 orders
    BUY 0 10@$149.89 DAY
    BUY 0 10@$149.90 DAY
    SELL 0 10@$150.10 DAY
    SELL 0 10@$150.11 DAY
    |}];
  return ()
;;
