(** The dashboard's pure state machine: folds the stream of per-second
    {!Exchange_stats.t} snapshots into a bounded rolling window, and projects
    that window into a {!Display.t} the view renders.

    Pure and free of Bonsai/Async, exactly like {!Jsip_monitor.Controller},
    so it can be exercised with plain expect tests. The Bonsai layer
    ({!Jsip_dashboard.View}) only feeds it snapshots and renders its display. *)

open! Core
open Jsip_dashboard_protocol

(** Everything the panes need, pre-computed from the window so the view stays
    dumb. Series run oldest-first; [now]/latency/rate fields describe the
    most recent snapshot. *)
module Display : sig
  type t =
    { memory_series : int list
    (** [live_words] over the window, oldest first *)
    ; memory_now : int option
    ; memory_min : int option
    ; memory_max : int option
    ; memory_delta : int option
    (** [live_words] now minus at the oldest retained snapshot — the
        unambiguous flat-vs-climbing signal the auto-scaled sparkline can't
        give. Near zero = flat; large positive = growing. *)
    ; submit_latency : Exchange_stats.Latency_summary.t
    ; cancel_latency : Exchange_stats.Latency_summary.t
    ; submit_latency_series : Exchange_stats.Latency_summary.t list
    (** per-snapshot submit latency over the window, oldest first, for the
        p50/p90/p99 trend chart *)
    ; cancel_latency_series : Exchange_stats.Latency_summary.t list
    ; submit_rate : int
    (** submitted orders in the last snapshot (≈ orders/sec) *)
    ; cancel_rate : int
    ; submit_rate_series : int list
    (** submit count per snapshot, oldest first *)
    ; cancel_rate_series : int list
    ; minor_collections : int
    ; major_collections : int
    ; sample_count : int (** snapshots currently in the window *)
    }
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of, equal]

(** Longest window we retain (seconds, since snapshots arrive at ~1 Hz). *)
val window_size : int

(** Empty window — what the dashboard shows before the first snapshot. *)
val empty : t

(** Append a snapshot, dropping it if it equals the newest already held (the
    browser polls faster than the exchange emits, so duplicates are common)
    and trimming the window to {!window_size}. *)
val apply_snapshot : t -> Exchange_stats.t -> t

val display : t -> Display.t
