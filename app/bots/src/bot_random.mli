open! Core

(** Random-sampling helpers shared by the trading bots.

    Every draw is threaded through the caller's [Splittable_random.t]
    (obtained from [Jsip_bot_runtime.Bot_runtime.Context.random]) so a
    scenario stays reproducible from its seed. Probabilities are expressed as
    {!Core.Percent.t} rather than bare floats, so a value is always clearly a
    proportion at the call site (e.g. [Percent.of_percentage 70.]). *)

(** A categorical distribution: each value paired with a relative weight. The
    weights need not sum to 100% — {!categorically_weighted_exn} normalizes
    them — and any entry whose weight is non-positive is never drawn. This is
    the extensible way to choose among a variant type: adding a new variant
    (say a new {!Jsip_types.Time_in_force.t}) is just another entry in the
    list, with no change to the sampling code. *)
type 'a distribution = ('a * Percent.t) list

(** [does_occur rng chance] is [true] with probability [chance]. *)
val does_occur : Splittable_random.t -> Percent.t -> bool

(** [int_inclusive rng ~lo ~hi] draws an integer uniformly from the closed
    interval [[lo, hi]] -- both endpoints included. *)
val int_inclusive : Splittable_random.t -> lo:int -> hi:int -> int

(** Pick one element of [choices] uniformly at random. Raises if [choices] is
    empty. *)
val uniform_exn : Splittable_random.t -> 'a list -> 'a

(** Sample one value from a categorical [distribution]. Raises if the
    distribution is empty or every weight is non-positive. *)
val categorically_weighted_exn : Splittable_random.t -> 'a distribution -> 'a
