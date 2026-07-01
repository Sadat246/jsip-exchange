open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]

  let to_string = Int.to_string
  let of_string = Int.of_string
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let of_int x = x
let to_int x = x
