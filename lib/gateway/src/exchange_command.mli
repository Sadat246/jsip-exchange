open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t
[@@deriving sexp_of]

val parse : ?default_participant:Participant.t -> string -> t Or_error.t
val to_string : t -> string
