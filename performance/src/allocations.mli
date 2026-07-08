open! Core

(** "Silly vs non-silly" allocation contests: each task is done two ways that
    return the same result but allocate very differently -- the point is
    visible in the [mWd/Run] column. *)

(** Build a copy of a list. [silly] appends with [@] (rebuilding the spine on
    every step -- O(n^2) allocation); [non_silly] prepends then reverses once
    (O(n)). *)
module Build_list : sig
  val silly : int list -> int list
  val non_silly : int list -> int list
end

(** The first element satisfying [f]. [silly] filters the whole list
    (allocating a fresh list of every match) then takes the head; [non_silly]
    uses [find], which stops at the first match and allocates nothing. *)
module First_match : sig
  val silly : int list -> f:(int -> bool) -> int option
  val non_silly : int list -> f:(int -> bool) -> int option
end
