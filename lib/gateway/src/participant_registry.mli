(** Server-local registry mapping participant names to small integer ids and
    back.

    Names ([Participant.t]) cross the wire; ids ([Participant_id.t]) are a
    server-internal optimization that never does. The registry mints an id
    the first time it sees a name and reuses that id forever after — ids are
    stable for the process's lifetime and are never reclaimed, so they stay
    dense and monotonic. All the name<->id statefulness lives here, which is
    what keeps {!Jsip_types.Participant} a pure value. *)

open! Core
open Jsip_types

type t

(** An empty registry, ready to intern its first name. *)
val create : unit -> t

(** [name]'s id: mint a fresh one the first time [name] is seen, and reuse
    the existing id on every later call (e.g. when a participant reconnects).
    Ids are assigned in increasing order and never reused across distinct
    names. *)
val intern : t -> Participant.t -> Participant_id.t

(** The name [id] was minted for. Raises if [id] did not come from this
    registry — which cannot happen for ids obtained through {!intern}. *)
val to_name : t -> Participant_id.t -> Participant.t
