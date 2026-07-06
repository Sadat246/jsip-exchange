(** Expect tests for {!Jsip_bots.Bot_random}, the shared randomness helpers
    the bots draw from. *)

open! Core
open! Async
open! Jsip_bots

let%expect_test "categorically_weighted_exn honors the weights" =
  let rng = Splittable_random.of_int 7 in
  let distribution =
    [ "day", Percent.of_percentage 60.
    ; "ioc", Percent.of_percentage 40.
    ; "never", Percent.of_percentage 0.
    ]
  in
  let counts = String.Table.create () in
  for _ = 1 to 10_000 do
    let drawn = Bot_random.categorically_weighted_exn rng distribution in
    Hashtbl.incr counts drawn
  done;
  List.iter [ "day"; "ioc"; "never" ] ~f:(fun key ->
    let count = Option.value (Hashtbl.find counts key) ~default:0 in
    printf "%s: %.2f\n" key (Float.of_int count /. 10_000.));
  [%expect {|
    day: 0.60
    ioc: 0.40
    never: 0.00
    |}];
  return ()
;;
