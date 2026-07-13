open! Core
open Jsip_types

(* Mutable, server-global, long-lived: interning happens in place on login,
   so [intern] mutates rather than returning a fresh registry the caller must
   thread. [next_id] only ever increases; nothing is removed, so ids stay
   dense and monotonic. *)
type t =
  { name_to_id : Participant_id.t Participant.Table.t
  ; id_to_name : Participant.t Participant_id.Table.t
  ; mutable next_id : int
  }

let create () =
  { name_to_id = Participant.Table.create ()
  ; id_to_name = Participant_id.Table.create ()
  ; next_id = 0
  }
;;

let intern t name =
  match Hashtbl.find t.name_to_id name with
  | Some id -> id (* reuse: seen before (e.g. reconnect) *)
  | None ->
    (* assign: mint the next id, record both directions, never reclaim it *)
    let id = Participant_id.Private.of_int t.next_id in
    t.next_id <- t.next_id + 1;
    Hashtbl.set t.name_to_id ~key:name ~data:id;
    Hashtbl.set t.id_to_name ~key:id ~data:name;
    id
;;

let to_name t id = Hashtbl.find_exn t.id_to_name id
