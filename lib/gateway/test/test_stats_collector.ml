open! Core
open Jsip_gateway

(* Percentiles use nearest-rank: for [n] sorted samples the [p]th value is at
   1-based rank [ceil (p/100 * n)]. The expected outputs below are
   hand-computed from that rule (samples are in ms). *)

let take_submit samples =
  let t = Stats_collector.create () in
  List.iter samples ~f:(fun ms ->
    Stats_collector.record_submit t (Time_ns.Span.of_ms (Float.of_int ms)));
  let submit, _cancel = Stats_collector.take t in
  print_s [%sexp (submit : Exchange_stats.Latency_summary.t)]
;;

let%expect_test "1..10 -> p50=5 p90=9 p99=10" =
  take_submit (List.range 1 11);
  [%expect {| ((count 10) (p50 5ms) (p90 9ms) (p99 10ms)) |}]
;;

let%expect_test "1..100 -> p50=50 p90=90 p99=99" =
  take_submit (List.range 1 101);
  [%expect {| ((count 100) (p50 50ms) (p90 90ms) (p99 99ms)) |}]
;;

let%expect_test "small batch -> ranks round up" =
  take_submit [ 10; 20; 30 ];
  [%expect {| ((count 3) (p50 20ms) (p90 30ms) (p99 30ms)) |}]
;;

let%expect_test "single sample -> all percentiles equal" =
  take_submit [ 42 ];
  [%expect {| ((count 1) (p50 42ms) (p90 42ms) (p99 42ms)) |}]
;;

let%expect_test "empty batch -> empty summary" =
  take_submit [];
  [%expect {| ((count 0) (p50 0s) (p90 0s) (p99 0s)) |}]
;;

let%expect_test "take clears: second take is empty; streams are independent" =
  let t = Stats_collector.create () in
  Stats_collector.record_submit t (Time_ns.Span.of_ms 5.);
  Stats_collector.record_cancel t (Time_ns.Span.of_ms 9.);
  let submit1, cancel1 = Stats_collector.take t in
  let submit2, cancel2 = Stats_collector.take t in
  print_s
    [%message
      "first take"
        (submit1 : Exchange_stats.Latency_summary.t)
        (cancel1 : Exchange_stats.Latency_summary.t)];
  print_s
    [%message
      "second take"
        (submit2 : Exchange_stats.Latency_summary.t)
        (cancel2 : Exchange_stats.Latency_summary.t)];
  [%expect
    {|
    ("first take" (submit1 ((count 1) (p50 5ms) (p90 5ms) (p99 5ms)))
     (cancel1 ((count 1) (p50 9ms) (p90 9ms) (p99 9ms))))
    ("second take" (submit2 ((count 0) (p50 0s) (p90 0s) (p99 0s)))
     (cancel2 ((count 0) (p50 0s) (p90 0s) (p99 0s))))
    |}]
;;
