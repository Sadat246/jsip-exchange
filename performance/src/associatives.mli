open! Core

(** Associative (by-key) stores for the map-vs-hashtbl and key-representation
    pre-exercises. Values are always [int]; the key type varies (int vs
    string) to isolate the cost of comparing/hashing different keys. *)

module Map_int : sig
  type t

  val create : unit -> t
  val set : t -> key:int -> data:int -> unit
  val get : t -> int -> int option
end

module Hashtable_int : sig
  type t

  val create : unit -> t
  val set : t -> key:int -> data:int -> unit
  val get : t -> int -> int option
end

module Map_string : sig
  type t

  val create : unit -> t
  val set : t -> key:string -> data:int -> unit
  val get : t -> string -> int option
end

module Hashtable_string : sig
  type t

  val create : unit -> t
  val set : t -> key:string -> data:int -> unit
  val get : t -> string -> int option
end

(** A deliberately fat, mixed-field record key -- to show how a large
    structural key inflates compare/hash cost relative to an int. *)
module Fat_record : sig
  type t

  (** Build a distinct key from an index, so benchmarks can generate keys. *)
  val of_index : int -> t
end

module Map_record : sig
  type t

  val create : unit -> t
  val set : t -> key:Fat_record.t -> data:int -> unit
  val get : t -> Fat_record.t -> int option
end

module Hashtable_record : sig
  type t

  val create : unit -> t
  val set : t -> key:Fat_record.t -> data:int -> unit
  val get : t -> Fat_record.t -> int option
end
