open! Core

(** Sequential (positional) [int] containers for the "sequential access"
    pre-exercise. In all of these [key] is a 0-based index into the sequence.

    [set ~key ~data] updates the element at index [key]; if [key] equals the
    current length it appends (growing by one). It raises if [key] is out of
    range (negative, or greater than the length). [get] returns [None] out of
    range. [remove] drops the element at [key] (a no-op if out of range),
    shifting later elements down.

    The exercise compares the three backings on the same operations. *)

module List_seq : sig
  (** A list indexed by position. A poor fit for positional access:
      [get]/[set]/[remove] are all O(n). *)
  type t

  val create : unit -> t
  val set : t -> key:int -> data:int -> unit
  val get : t -> int -> int option
end

module Dynarray_seq : sig
  (** Stdlib growable array (amortized O(1) append via doubling). O(1)
      get/set; O(n) remove-at-index (shift the tail down, then drop the
      last). *)
  type t

  val create : unit -> t
  val set : t -> key:int -> data:int -> unit
  val get : t -> int -> int option
end
