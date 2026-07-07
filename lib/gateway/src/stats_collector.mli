(** Accumulates request-latency samples and computes percentile summaries for
    {!Exchange_stats}.

    The pure half of the stats pipeline: it never reads the clock or
    [Gc.stat] (the server measures each latency and passes in a
    [Time_ns.Span.t]), so the percentile math is deterministically
    unit-testable. The matching loop calls [record_*] per request; a 1 Hz
    timer calls [take] to snapshot and reset. *)

open! Core

type t

val create : unit -> t
val record_submit : t -> Time_ns.Span.t -> unit
val record_cancel : t -> Time_ns.Span.t -> unit

(** Summarize samples recorded since the last [take] (submit and cancel
    separately) as [(submit, cancel)], then clear both buffers. Percentiles
    use nearest-rank: sort ascending, [p]th value is at 1-based rank
    [ceil (p/100 * n)]. No samples yields
    {!Exchange_stats.Latency_summary.empty}. *)
val take
  :  t
  -> Exchange_stats.Latency_summary.t * Exchange_stats.Latency_summary.t
