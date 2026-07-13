open! Core

type t = private int [@@deriving sexp_of, compare, equal, hash]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

module Private : sig
  (** Mint an id. For {!Participant_registry} only. *)
  val of_int : int -> t
end
