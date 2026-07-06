(** A deliberately pathological market-data subscriber.

    A well-behaved subscriber drains its feed promptly: the runner calls
    [Pipe.iter feed ~f:on_event], and each [on_event] returns quickly so the
    next event can be pulled. This bot does the opposite — it takes
    [read_delay] to handle every event. Because the runner won't pull the
    next event until [on_event] resolves, a slow [on_event] stalls the whole
    drain.

    That stall is the point. The exchange pushes into each subscriber's pipe
    with [Pipe.write_without_pushback_if_open] (see [Dispatcher]/[Session]),
    which never blocks the producer. So when this consumer falls behind, the
    events it hasn't read pile up in the exchange-side buffer without bound —
    the memory-growth pathology this bot exists to demonstrate.

    It never trades; it only subscribes and lags. Pair it with
    {!Market_maker_bot} (via the [slow-consumer] scenario) to give it a
    firehose of market-data events to fall behind on. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime

module Config : sig
  type t

  (** [create ~read_delay] builds a consumer that waits [read_delay] before
      finishing with each event. Larger delays make it fall behind faster. *)
  val create : read_delay:Time_ns.Span.t -> t
end

val name : string
val on_start : Config.t -> Bot_runtime.Context.t -> unit Deferred.t
val on_tick : Config.t -> Bot_runtime.Context.t -> unit Deferred.t

val on_event
  :  Config.t
  -> Bot_runtime.Context.t
  -> Exchange_event.t
  -> unit Deferred.t
