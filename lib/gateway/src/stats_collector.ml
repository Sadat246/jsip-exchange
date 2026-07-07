open! Core

(* Samples cons on as they arrive; [summarize] sorts before ranking, so arrival
   order is irrelevant. *)
type t =
  { mutable submit_samples : Time_ns.Span.t list
  ; mutable cancel_samples : Time_ns.Span.t list
  }

let create () = { submit_samples = []; cancel_samples = [] }
let record_submit t span = t.submit_samples <- span :: t.submit_samples
let record_cancel t span = t.cancel_samples <- span :: t.cancel_samples

(* Nearest-rank percentile of a non-empty ascending array. *)
let percentile sorted p =
  let n = Array.length sorted in
  let rank = Float.iround_up_exn (Float.of_int n *. p /. 100.) in
  sorted.(Int.clamp_exn rank ~min:1 ~max:n - 1)
;;

let summarize samples : Exchange_stats.Latency_summary.t =
  match samples with
  | [] -> Exchange_stats.Latency_summary.empty
  | _ ->
    let sorted = Array.of_list samples in
    Array.sort sorted ~compare:Time_ns.Span.compare;
    { count = Array.length sorted
    ; p50 = percentile sorted 50.
    ; p90 = percentile sorted 90.
    ; p99 = percentile sorted 99.
    }
;;

let take t =
  let submit = summarize t.submit_samples in
  let cancel = summarize t.cancel_samples in
  t.submit_samples <- [];
  t.cancel_samples <- [];
  submit, cancel
;;
