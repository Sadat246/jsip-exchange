open! Core
open Jsip_dashboard_protocol
open Jsip_dashboard_controller

(* Build snapshots inline: there's no [Exchange_stats.t] builder in the test
   harness, and only a few fields matter to the controller. *)

let latency ?(count = 0) ?(p50 = 0) ?(p90 = 0) ?(p99 = 0) () =
  { Exchange_stats.Latency_summary.count
  ; p50 = Time_ns.Span.of_ms (Float.of_int p50)
  ; p90 = Time_ns.Span.of_ms (Float.of_int p90)
  ; p99 = Time_ns.Span.of_ms (Float.of_int p99)
  }
;;

let snapshot
  ?(live_words = 1000)
  ?(minor = 0)
  ?(major = 0)
  ?(submit = latency ())
  ?(cancel = latency ())
  ()
  =
  { Exchange_stats.live_words
  ; heap_words = live_words
  ; top_heap_words = live_words
  ; minor_words = 0.
  ; promoted_words = 0.
  ; minor_collections = minor
  ; major_collections = major
  ; submit_latency = submit
  ; cancel_latency = cancel
  }
;;

let feed snapshots =
  List.fold snapshots ~init:Controller.empty ~f:Controller.apply_snapshot
;;

let show t = print_s [%sexp (Controller.display t : Controller.Display.t)]

let%expect_test "empty window" =
  show Controller.empty;
  [%expect
    {|
    ((memory_series ()) (memory_now ()) (memory_min ()) (memory_max ())
     (submit_latency ((count 0) (p50 0s) (p90 0s) (p99 0s)))
     (cancel_latency ((count 0) (p50 0s) (p90 0s) (p99 0s))) (submit_rate 0)
     (cancel_rate 0) (minor_collections 0) (major_collections 0)
     (sample_count 0))
    |}]
;;

let%expect_test "series, latest latency, and per-second rates" =
  let t =
    feed
      [ snapshot ~live_words:1000 ~submit:(latency ~count:3 ()) ()
      ; snapshot
          ~live_words:1500
          ~minor:7
          ~major:1
          ~submit:(latency ~count:5 ~p50:2 ~p90:8 ~p99:20 ())
          ~cancel:(latency ~count:2 ~p50:1 ())
          ()
      ]
  in
  show t;
  [%expect
    {|
    ((memory_series (1000 1500)) (memory_now (1500)) (memory_min (1000))
     (memory_max (1500))
     (submit_latency ((count 5) (p50 2ms) (p90 8ms) (p99 20ms)))
     (cancel_latency ((count 2) (p50 1ms) (p90 0s) (p99 0s))) (submit_rate 5)
     (cancel_rate 2) (minor_collections 7) (major_collections 1)
     (sample_count 2))
    |}]
;;

let%expect_test "consecutive-identical snapshots are deduped" =
  let s = snapshot ~live_words:2000 () in
  let t = feed [ s; s; s ] in
  show t;
  [%expect
    {|
    ((memory_series (2000)) (memory_now (2000)) (memory_min (2000))
     (memory_max (2000)) (submit_latency ((count 0) (p50 0s) (p90 0s) (p99 0s)))
     (cancel_latency ((count 0) (p50 0s) (p90 0s) (p99 0s))) (submit_rate 0)
     (cancel_rate 0) (minor_collections 0) (major_collections 0)
     (sample_count 1))
    |}]
;;

let%expect_test "an equal value after a change is kept, not deduped" =
  (* Dedup only collapses *consecutive* equals: 1000, 2000, 1000 keeps all
     three, so a value returning to an earlier level still registers. *)
  let t =
    feed
      [ snapshot ~live_words:1000 ()
      ; snapshot ~live_words:2000 ()
      ; snapshot ~live_words:1000 ()
      ]
  in
  print_s [%sexp ((Controller.display t).memory_series : int list)];
  [%expect {| (1000 2000 1000) |}]
;;

let%expect_test "window is capped at window_size, oldest dropped first" =
  let t =
    feed (List.init 65 ~f:(fun i -> snapshot ~live_words:(i + 1) ()))
  in
  let d = Controller.display t in
  print_s
    [%message
      ""
        ~sample_count:(d.sample_count : int)
        ~window_size:(Controller.window_size : int)
        ~memory_now:(d.memory_now : int option)
        ~memory_min:(d.memory_min : int option)
        ~memory_max:(d.memory_max : int option)];
  [%expect
    {|
    ((sample_count 60) (window_size 60) (memory_now (65)) (memory_min (6))
     (memory_max (65)))
    |}]
;;
