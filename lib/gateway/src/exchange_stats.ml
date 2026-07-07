open! Core

module Latency_summary = struct
  type t =
    { count : int
    ; p50 : Time_ns.Span.t
    ; p90 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, compare, equal]

  let empty =
    { count = 0
    ; p50 = Time_ns.Span.zero
    ; p90 = Time_ns.Span.zero
    ; p99 = Time_ns.Span.zero
    }
  ;;
end

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
