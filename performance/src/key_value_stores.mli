open! Core

(** A deliberately-naive [int -> int] key-value store: correct, but O(n) per
    operation (it holds an association list and rebuilds it on writes). It's
    a baseline to compare faster stores against later. *)
module Silly_store : sig
  type t

  val create : unit -> t
  val set : t -> key:int -> data:int -> unit
  val get : t -> int -> int option
  val remove : t -> int -> unit
end
