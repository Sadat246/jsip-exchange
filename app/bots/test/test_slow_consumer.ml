open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
open! Jsip_bots
open Bot_harness

(* A synthetic BBO event for AAPL. [nonce] just nudges the prices so
   successive events differ, mimicking a market maker re-quoting. *)
let bbo_event nonce : Exchange_event.t =
  Best_bid_offer_update
    { symbol = aapl
    ; bbo =
        { bid =
            Some
              { price = Price.of_int_cents (15000 - nonce)
              ; size = Size.of_int 100
              }
        ; ask =
            Some
              { price = Price.of_int_cents (15100 + nonce)
              ; size = Size.of_int 100
              }
        }
    }
;;

(* The pathology, in miniature: model the exchange -> consumer feed as a
   pipe, exactly like a gateway [Session]. The exchange pushes with
   [write_without_pushback_if_open] (see [Session.push]), so the producer
   never blocks even when nobody is reading. A well-behaved consumer would
   drain the pipe promptly; the slow consumer does not, and the events sit in
   the buffer. [Pipe.read_now'] snapshots that backlog without blocking. *)
let%expect_test "events pile up in the feed pipe while the consumer is busy" =
  let reader, writer = Pipe.create () in
  let burst = 8 in
  for nonce = 1 to burst do
    Pipe.write_without_pushback_if_open writer (bbo_event nonce)
  done;
  (* A consumer so slow that, on the timescale of this test, it never
     finishes even the first event: [read_delay] is a full day, so
     [on_event]'s [Clock_ns.after] stays pending and [consumed] stays 0. *)
  let config =
    Slow_consumer.Config.create ~read_delay:(Time_ns.Span.of_day 1.0)
  in
  let bot, _submitted, _cancelled =
    make_recording_bot (module Slow_consumer) config ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  (* The runner drains the feed with [Pipe.iter feed ~f:on_event], which
     pulls one event and waits for [on_event] before pulling the next. We do
     that first step by hand: pull exactly one event and start handling it.
     The returned deferred will not resolve within this test, so the
     remaining [burst - 1] events stay parked in the pipe. *)
  (match Pipe.read_now reader with
   | `Ok event -> don't_wait_for (Slow_consumer.on_event config ctx event)
   | `Eof | `Nothing_available -> ());
  (* TODO(human): use [Pipe.read_now'] to snapshot how many events are still
     buffered in [reader], and [printf] that count. This is the backlog that
     would grow without bound in a real run. *)
  (match Pipe.read_now' reader with
   | `Ok q -> printf "%d" (Queue.length q)
   | `Eof | `Nothing_available -> printf "0");
  [%expect {| 7 |}];
  return ()
;;

(* Complement to the backlog test: once [on_event] actually finishes, the
   consumer's count advances. Here [read_delay] is zero, so each [on_event]
   resolves promptly, and [on_tick] prints the running total. *)
let%expect_test "on_event advances the consumed count once it finishes" =
  let config = Slow_consumer.Config.create ~read_delay:Time_ns.Span.zero in
  let bot, _submitted, _cancelled =
    make_recording_bot (module Slow_consumer) config ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  let%bind () = Slow_consumer.on_tick config ctx in
  let%bind () = Slow_consumer.on_event config ctx (bbo_event 1) in
  let%bind () = Slow_consumer.on_event config ctx (bbo_event 2) in
  let%bind () = Slow_consumer.on_tick config ctx in
  [%expect
    {|
    [slow-consumer] finished handling 0 events so far
    [slow-consumer] finished handling 2 events so far
    |}];
  return ()
;;
