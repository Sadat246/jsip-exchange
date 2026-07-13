(** Expect tests for {!Jsip_bots.Book_filler_sadat}. *)

open! Core
open! Async
open Jsip_bot_runtime
open! Jsip_bots
open Bot_harness

(* One [on_tick] with the fundamental pinned at $150.00. Every order must
   rest ($5.00+ away from fair value, so non-marketable), be [Day], and carry
   a distinct client order id. With [level_spacing_cents = 10] successive
   pairs march onto new price levels; sides alternate so both halves of the
   book grow. *)
let%expect_test "book_filler floods non-marketable resting Day orders" =
  let config : Book_filler_sadat.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 6
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 10
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot
      (module Book_filler_sadat)
      config
      ~initial_price_cents:15000
      ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler_sadat.on_tick config context in
  print_orders submitted;
  [%expect
    {|
    cid=0 BUY 0 1@$145.00 DAY
    cid=1 SELL 0 1@$155.00 DAY
    cid=2 BUY 0 1@$144.90 DAY
    cid=3 SELL 0 1@$155.10 DAY
    cid=4 BUY 0 1@$144.80 DAY
    cid=5 SELL 0 1@$155.20 DAY
    |}];
  return ()
;;

(* The client-order-id counter lives in [config] as a [ref] and must keep
   advancing across ticks: the exchange rejects duplicate ids, so two ticks
   in a row must never mint the same id. The fundamental is pinned, so prices
   repeat tick to tick while the ids do not. *)
let%expect_test "book_filler advances client_order_id across ticks" =
  let config : Book_filler_sadat.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 3
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 10
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Book_filler_sadat) config ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler_sadat.on_tick config context in
  let%bind () = Book_filler_sadat.on_tick config context in
  print_orders submitted;
  [%expect
    {|
    cid=0 BUY 0 1@$145.00 DAY
    cid=1 SELL 0 1@$155.00 DAY
    cid=2 BUY 0 1@$144.90 DAY
    cid=3 BUY 0 1@$145.00 DAY
    cid=4 SELL 0 1@$155.00 DAY
    cid=5 BUY 0 1@$144.90 DAY
    |}];
  return ()
;;

(* [level_spacing_cents = 0] stacks every order this tick onto a single price
   level per side (growing depth, not the number of levels): all buys sit at
   [fundamental - offset] and all sells at [fundamental + offset]. *)
let%expect_test "book_filler with zero level spacing stacks one level per \
                 side"
  =
  let config : Book_filler_sadat.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 6
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 0
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Book_filler_sadat) config ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler_sadat.on_tick config context in
  print_orders submitted;
  [%expect
    {|
    cid=0 BUY 0 1@$145.00 DAY
    cid=1 SELL 0 1@$155.00 DAY
    cid=2 BUY 0 1@$145.00 DAY
    cid=3 SELL 0 1@$155.00 DAY
    cid=4 BUY 0 1@$145.00 DAY
    cid=5 SELL 0 1@$155.00 DAY
    |}];
  return ()
;;

(* When the offset would drive a buy price to zero or below, it is floored at
   one cent so it stays a valid positive price; sells are unaffected and keep
   climbing. A $3.00 fundamental with a $5.00 offset forces the clamp, and
   the $1.00 level spacing shows it holding across successive levels. *)
let%expect_test "book_filler floors buy prices at one cent" =
  let config : Book_filler_sadat.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 4
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 100
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot
      (module Book_filler_sadat)
      config
      ~initial_price_cents:300
      ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler_sadat.on_tick config context in
  print_orders submitted;
  [%expect
    {|
    cid=0 BUY 0 1@$0.01 DAY
    cid=1 SELL 0 1@$8.00 DAY
    cid=2 BUY 0 1@$0.01 DAY
    cid=3 SELL 0 1@$9.00 DAY
    |}];
  return ()
;;

(* With several symbols the per-tick orders round-robin across them
   ([index mod num_symbols]) and each is priced off its own symbol's
   fundamental — AAPL near $150, MSFT near $200. (With exactly two symbols
   the even/odd side split lines up with the round-robin, so AAPL takes all
   the buys and MSFT all the sells; that is a consequence of these particular
   counts, not a guarantee of the bot.) *)
let%expect_test "book_filler round-robins orders across symbols" =
  let config : Book_filler_sadat.Config.t =
    { symbols = [ aapl; msft ]
    ; orders_per_tick = 4
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 10
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot
      (module Book_filler_sadat)
      config
      ~initial_price_cents:15000
      ~extra_symbol_prices:[ msft, 20000 ]
      ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler_sadat.on_tick config context in
  print_orders submitted;
  [%expect
    {|
    cid=0 BUY 0 1@$145.00 DAY
    cid=1 SELL 1 1@$205.00 DAY
    cid=2 BUY 0 1@$144.90 DAY
    cid=3 SELL 1 1@$205.10 DAY
    |}];
  return ()
;;

(* [order_size] is applied verbatim to every order the bot submits,
   regardless of side or price level: with [order_size = 10] each request
   carries size 10, confirming the bot reads the config rather than
   hard-coding a size. The orders still rest ($5.00 off the pinned $150.00
   fundamental) — size affects only the notional resting on the book, never
   whether an order trades. *)
let%expect_test "book_filler has order_size greater than 1" =
  let config : Book_filler_sadat.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 2
    ; order_size = 10
    ; price_offset_cents = 500
    ; level_spacing_cents = 100
    ; next_client_order_id = ref 0
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot
      (module Book_filler_sadat)
      config
      ~initial_price_cents:30000
      ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let%bind () = Book_filler_sadat.on_tick config context in
  print_orders submitted;
  [%expect
    {|
    cid=0 BUY 0 10@$295.00 DAY
    cid=1 SELL 0 10@$305.00 DAY
    |}];
  return ()
;;

(* [on_start] validates the config once, before the first tick, so a bad
   scenario fails loudly at boot rather than as an obscure crash mid-run.
   Each guard — empty symbols, non-positive orders-per-tick, non-positive
   order size — should raise. *)
let%expect_test "book_filler on_start rejects invalid configs" =
  let base : Book_filler_sadat.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 5
    ; order_size = 1
    ; price_offset_cents = 500
    ; level_spacing_cents = 1
    ; next_client_order_id = ref 0
    }
  in
  let bot, _submitted, _cancelled =
    make_recording_bot (module Book_filler_sadat) base ()
  in
  let context = Bot_runtime.For_testing.context_of bot in
  let require_raises (config : Book_filler_sadat.Config.t) =
    Expect_test_helpers_core.require_does_raise (fun () ->
      Book_filler_sadat.on_start config context)
  in
  require_raises { base with symbols = [] };
  [%expect {| "Book_filler: symbols must be non-empty" |}];
  require_raises { base with orders_per_tick = 0 };
  [%expect
    {| ("Book_filler: orders_per_tick must be positive" (orders_per_tick 0)) |}];
  require_raises { base with order_size = 0 };
  [%expect {| ("Book_filler: order_size must be positive" (order_size 0)) |}];
  require_raises { base with order_size = -1 };
  [%expect
    {| ("Book_filler: order_size must be positive" (order_size -1)) |}];
  return ()
;;
