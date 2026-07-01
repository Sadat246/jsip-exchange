(** A unique identifier for a client order on the exchange.

    Order IDs are assigned sequentially by the matching engine when an order
    is accepted. They are never reused within a trading session. *)

open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash]

val to_string : t -> string
val of_string : string -> t

include Comparable.S with type t := t
include Hashable.S with type t := t

val of_int : int -> t
val to_int : t -> int
