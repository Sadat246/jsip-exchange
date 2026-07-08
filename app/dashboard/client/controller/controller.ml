open! Core
open Jsip_dashboard_protocol

let window_size = 60

module Display = struct
  type t =
    { memory_series : int list
    ; memory_now : int option
    ; memory_min : int option
    ; memory_max : int option
    ; memory_delta : int option
    ; submit_latency : Exchange_stats.Latency_summary.t
    ; cancel_latency : Exchange_stats.Latency_summary.t
    ; submit_latency_series : Exchange_stats.Latency_summary.t list
    ; cancel_latency_series : Exchange_stats.Latency_summary.t list
    ; submit_rate : int
    ; cancel_rate : int
    ; submit_rate_series : int list
    ; cancel_rate_series : int list
    ; minor_collections : int
    ; major_collections : int
    ; sample_count : int
    }
  [@@deriving sexp_of, equal]
end

(* Snapshots oldest-first; length is capped at [window_size]. A list (not a
   deque) is fine: n <= 60, so the O(n) append and [List.last] are trivial. *)
type t = { snapshots : Exchange_stats.t list } [@@deriving sexp_of, equal]

let empty = { snapshots = [] }

let apply_snapshot t (snapshot : Exchange_stats.t) =
  match List.last t.snapshots with
  | Some prev when Exchange_stats.equal prev snapshot -> t
  | _ ->
    let snapshots = t.snapshots @ [ snapshot ] in
    let overflow = List.length snapshots - window_size in
    let snapshots =
      if overflow > 0 then List.drop snapshots overflow else snapshots
    in
    { snapshots }
;;

let display t =
  let memory_series =
    List.map t.snapshots ~f:(fun (s : Exchange_stats.t) -> s.live_words)
  in
  let latest = List.last t.snapshots in
  let of_latest ~default ~f = Option.value_map latest ~default ~f in
  let series f =
    List.map t.snapshots ~f:(fun (s : Exchange_stats.t) -> f s)
  in
  { Display.memory_series
  ; memory_now =
      Option.map latest ~f:(fun (s : Exchange_stats.t) -> s.live_words)
  ; memory_min = List.min_elt memory_series ~compare:Int.compare
  ; memory_max = List.max_elt memory_series ~compare:Int.compare
  ; memory_delta =
      (* now minus oldest retained: an absolute-magnitude read the
         auto-scaled chart can't give. [memory_series] is oldest-first, so
         [List.hd] is the oldest and [latest] the newest. *)
      (match memory_series with
       | [] -> None
       | oldest :: _ ->
         Option.map latest ~f:(fun (s : Exchange_stats.t) ->
           s.live_words - oldest))
  ; submit_latency =
      of_latest
        ~default:Exchange_stats.Latency_summary.empty
        ~f:(fun (s : Exchange_stats.t) -> s.submit_latency)
  ; cancel_latency =
      of_latest
        ~default:Exchange_stats.Latency_summary.empty
        ~f:(fun (s : Exchange_stats.t) -> s.cancel_latency)
  ; submit_latency_series = series (fun s -> s.submit_latency)
  ; cancel_latency_series = series (fun s -> s.cancel_latency)
  ; submit_rate =
      of_latest ~default:0 ~f:(fun (s : Exchange_stats.t) ->
        s.submit_latency.count)
  ; cancel_rate =
      of_latest ~default:0 ~f:(fun (s : Exchange_stats.t) ->
        s.cancel_latency.count)
  ; submit_rate_series = series (fun s -> s.submit_latency.count)
  ; cancel_rate_series = series (fun s -> s.cancel_latency.count)
  ; minor_collections =
      of_latest ~default:0 ~f:(fun (s : Exchange_stats.t) ->
        s.minor_collections)
  ; major_collections =
      of_latest ~default:0 ~f:(fun (s : Exchange_stats.t) ->
        s.major_collections)
  ; sample_count = List.length t.snapshots
  }
;;
