(** A compact integer id for a {!Symbol.t}, an interning of the symbol
    string.

    Unlike {!Participant_id} (which is server-local and lives in the
    gateway), a symbol id {b crosses the wire}: the exchange can send the
    small int in place of the symbol string on hot feeds, so it belongs here
    beside the other wire types and derives [bin_io].

    [private int]: readable as an array index via [(id :> int)] but not
    forgeable — ids are minted by the matching engine's symbol registry
    (fixed at startup), not chosen by clients. Contrast {!Client_order_id},
    whose ids the client picks and so exposes [of_int] freely. *)

open! Core

type t = private int [@@deriving sexp, bin_io, compare, equal, hash]

(** Prints the raw integer id. [Symbol_id] carries no name — that mapping
    lives in the server's registry, not in this pure data type — so an int is
    all it can show. *)
val to_string : t -> string

include Comparable.S with type t := t
include Hashable.S with type t := t

module Private : sig
  (** Mint an id. For the symbol registry that owns the name<->id mapping;
      everyone else obtains ids from it. *)
  val of_int : int -> t
end
