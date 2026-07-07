(** A per-second health snapshot of the running exchange process, streamed by
    {!Rpc_protocol.exchange_stats_rpc} to operator tooling.

    Operational metrics (memory + latency), not domain vocabulary. It lives
    in its own [Core]-only library — rather than [Jsip_gateway] — so the
    browser dashboard can link it: [Jsip_gateway] pulls in [async_unix],
    which [js_of_ocaml] cannot compile. Each snapshot is raw; building a
    rolling ~60s view from the stream is the dashboard's job. *)

open! Core

(** Percentiles of one interval's request latencies. [p99] is the slowest
    1-in-100 request — the tail that climbs first under load. *)
module Latency_summary : sig
  type t =
    { count : int
    ; p50 : Time_ns.Span.t
    ; p90 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, compare, equal]

  (** No samples this interval: [count = 0], spans [Time_ns.Span.zero]. *)
  val empty : t
end

(** GC fields mirror OCaml's [Gc.stat] record; [live_words] (words reachable
    now) is the primary leak signal. Latencies are end-to-end from RPC
    handler to matching-engine completion, queue wait included. *)
type t =
  { live_words : int
  ; heap_words : int
  ; top_heap_words : int
  ; minor_words : float
  ; promoted_words : float
  ; minor_collections : int
  ; major_collections : int
  ; submit_latency : Latency_summary.t
  ; cancel_latency : Latency_summary.t
  }
[@@deriving sexp, bin_io, compare, equal]
