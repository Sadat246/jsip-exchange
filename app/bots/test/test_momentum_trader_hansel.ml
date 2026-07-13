(** Expect tests for {!Jsip_bots.Momentum_trader_hansel}.

    Each test builds a fresh config (fresh ring and position), feeds events
    via [feed_events] / [feed_event], and drives ticks via [drive_ticks] --
    [Bot_runtime.start]'s clock loop never returns, so it is unusable here. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
open! Jsip_bots
open Bot_harness

let bob = Participant.of_string "Bob"
let goog = Symbol_id.Private.of_int 2

let momentum_config ?cooldown_ticks ?(max_order_size = 100) ?max_position () =
  Momentum_trader_hansel.Config.create_exn
    ?cooldown_ticks
    ~symbol:aapl
    ~window_capacity:3
    ~threshold_cents:5
    ~max_order_size
    ~max_position:(Option.value max_position ~default:1000)
    ()
;;

let trade ?(symbol = aapl) price_cents =
  Exchange_event.Trade_report
    { symbol; price = Price.of_int_cents price_cents; size = Size.of_int 10 }
;;

let feed_trades bot price_cents_list =
  feed_events bot (List.map price_cents_list ~f:trade)
;;

(* A fill between alice (the bot under test) and bob. [alice_is_aggressor]
   picks alice's role; [aggressor_side] is the side the aggressor traded, so
   e.g. alice resting against a [Buy] aggressor means alice sold. *)
let fill_event ~aggressor_side ~alice_is_aggressor ~size =
  let aggressor, resting =
    if alice_is_aggressor then alice, bob else bob, alice
  in
  Exchange_event.Fill
    { fill_id = 1
    ; symbol = aapl
    ; price = Price.of_int_cents 15000
    ; size = Size.of_int size
    ; aggressor_order_id = Order_id.For_testing.of_int 1
    ; aggressor_client_order_id = Client_order_id.of_int 1
    ; aggressor_participant = aggressor
    ; aggressor_side
    ; resting_order_id = Order_id.For_testing.of_int 2
    ; resting_client_order_id = Client_order_id.of_int 2
    ; resting_participant = resting
    }
;;

let%expect_test "momentum: does nothing until the window is full" =
  let config = momentum_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () = feed_trades bot [ 15000; 15010 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| |}];
  return ()
;;

let%expect_test "momentum: rising prices trigger a buy" =
  let config = momentum_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () = feed_trades bot [ 15000; 15004; 15010 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| BUY 0 10@$150.11 IOC |}];
  return ()
;;

let%expect_test "momentum: falling prices trigger a sell" =
  let config = momentum_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () = feed_trades bot [ 15010; 15004; 15000 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| SELL 0 10@$149.99 IOC |}];
  return ()
;;

let%expect_test "momentum: a signal below the threshold does nothing" =
  let config = momentum_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () = feed_trades bot [ 15000; 15001; 15003 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| |}];
  return ()
;;

let%expect_test "momentum: the signal decays as the ring rolls over" =
  let config = momentum_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () = feed_trades bot [ 15000; 15004; 15010 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  (* Two flat prints roll the early prices out; the window is now all 15010,
     so the signal is zero and the second tick stays quiet. *)
  let%bind () = feed_trades bot [ 15010; 15010 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| BUY 0 10@$150.11 IOC |}];
  return ()
;;

let%expect_test "momentum: order size is capped by max_order_size" =
  let config = momentum_config ~max_order_size:5 () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () = feed_trades bot [ 15000; 15010; 15050 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| BUY 0 5@$150.51 IOC |}];
  return ()
;;

let%expect_test "momentum: the position limit clamps and then blocks" =
  let config = momentum_config ~max_position:10 () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  (* Alice takes 8 shares as the aggressor: 2 shares of room left. *)
  let%bind () =
    Bot_runtime.feed_event
      bot
      (fill_event ~aggressor_side:Buy ~alice_is_aggressor:true ~size:8)
  in
  let%bind () = feed_trades bot [ 15000; 15004; 15010 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  (* That entry fills too; the limit is now exhausted, so the same strong
     signal submits nothing more. *)
  let%bind () =
    Bot_runtime.feed_event
      bot
      (fill_event ~aggressor_side:Buy ~alice_is_aggressor:true ~size:2)
  in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| BUY 0 2@$150.11 IOC |}];
  return ()
;;

let%expect_test "momentum: a fill where we rested also updates position" =
  let config = momentum_config ~max_position:10 () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  (* Bob's buy lifts alice's resting offer, so alice sold 8: short 8, with 2
     shares of room left on the sell side. *)
  let%bind () =
    Bot_runtime.feed_event
      bot
      (fill_event ~aggressor_side:Buy ~alice_is_aggressor:false ~size:8)
  in
  let%bind () = feed_trades bot [ 15010; 15004; 15000 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| SELL 0 2@$149.99 IOC |}];
  return ()
;;

let%expect_test "momentum: the cooldown skips ticks after an entry" =
  let config = momentum_config ~cooldown_ticks:2 () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  (* No further trades arrive, so the window (and signal) stay the same
     across all four ticks: entry, two cooldown skips, entry again. *)
  let%bind () = feed_trades bot [ 15000; 15004; 15010 ] in
  let%bind () = drive_ticks bot ~ticks:4 in
  print_submitted submitted;
  [%expect {|
    BUY 0 10@$150.11 IOC
    BUY 0 10@$150.11 IOC
    |}];
  return ()
;;

let%expect_test "momentum: trades in other symbols never enter the window" =
  let config = momentum_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Momentum_trader_hansel) config ()
  in
  let%bind () =
    feed_events
      bot
      (List.map [ 20000; 20010; 20020 ] ~f:(trade ~symbol:goog))
  in
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_trades bot [ 15000; 15004 ] in
  let%bind () = Bot_runtime.feed_event bot (trade ~symbol:goog 20030) in
  let%bind () = drive_ticks bot ~ticks:1 in
  (* Only now does a third AAPL print complete the window. *)
  let%bind () = feed_trades bot [ 15010 ] in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  [%expect {| BUY 0 10@$150.11 IOC |}];
  return ()
;;
