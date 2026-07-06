open! Core

type 'a distribution = ('a * Percent.t) list

let does_occur rng chance =
  Float.( < )
    (Splittable_random.float rng ~lo:0. ~hi:1.)
    (Percent.to_mult chance)
;;

let int_inclusive rng ~lo ~hi = Splittable_random.int rng ~lo ~hi

let uniform_exn rng choices =
  List.nth_exn
    choices
    (Splittable_random.int rng ~lo:0 ~hi:(List.length choices - 1))
;;

let categorically_weighted_exn rng distribution =
  let weights =
    List.map distribution ~f:(fun (value, weight) ->
      value, Float.max 0. (Percent.to_mult weight))
  in
  let total = List.sum (module Float) weights ~f:snd in
  if Float.( <= ) total 0.
  then
    raise_s
      [%message
        "Bot_random.weighted_exn: distribution has no positive weight"];
  let target = Splittable_random.float rng ~lo:0. ~hi:total in
  (* Walk the cumulative weight; the last entry also catches any
     floating-point slop that would otherwise leave [target] unmatched. *)
  let rec walk cumulative = function
    | [ (value, _) ] -> value
    | (value, weight) :: rest ->
      let cumulative = cumulative +. weight in
      if Float.( < ) target cumulative then value else walk cumulative rest
    | [] -> assert false
  in
  walk 0. weights
;;
