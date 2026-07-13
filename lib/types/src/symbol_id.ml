open! Core

module T = struct
  (* Concrete [int] here (privacy is added only in the .mli), so the module
     can construct values; the full [sexp] derivation is what lets
     [Comparable.Make] and [Hashable.Make] apply. *)
  type t = int [@@deriving sexp, bin_io, compare, equal, hash]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let to_string t = Int.to_string t

module Private = struct
  let of_int = Fn.id
end
