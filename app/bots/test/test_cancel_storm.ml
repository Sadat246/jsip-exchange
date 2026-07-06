(** Expect tests for {!Jsip_bots.Cancel_storm}.

    The storm holds no book and reacts to no events, so every test just
    drives [on_tick] and inspects what it submitted and cancelled. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
open! Jsip_bots
open Bot_harness

(* Is [req] priced to cross [fundamental_cents] -- a buy above it or a sell
   below it? An independent read of the price that does not reimplement
   [choose_price]; the storm prices off the fundamental, so this classifies
   marketable (crossing) vs. resting orders without needing a book. *)
let crosses_fundamental ~fundamental_cents (req : Order.Request.t) =
  match req.side with
  | Buy -> Price.to_int_cents req.price > fundamental_cents
  | Sell -> Price.to_int_cents req.price < fundamental_cents
;;

let%expect_test "each tick fires cycles_per_tick submit->cancel cycles \
                 under fresh ids"
  =
  let cycles_per_tick = 5 in
  let config =
    Cancel_storm.create_config
      ~symbols:[ aapl ]
      ~cycles_per_tick
      ~size:10
      ~pct_marketable:50
      ~price_offset_cents:20
  in
  let bot, submitted, cancelled =
    make_recording_bot (module Cancel_storm) config ()
  in
  let ticks = 3 in
  let%bind () = drive_ticks bot ~ticks in
  (* Both lists are newest-first; [rev_map] reads them back in the order the
     storm fired them. *)
  let submitted_ids =
    List.rev_map !submitted ~f:(fun (r : Order.Request.t) ->
      Client_order_id.to_int r.client_order_id)
  in
  let cancelled_ids = List.rev_map !cancelled ~f:Client_order_id.to_int in
  let total = cycles_per_tick * ticks in
  printf
    "submitted: %d, cancelled: %d\n"
    (List.length !submitted)
    (List.length !cancelled);
  printf
    "each submit cancelled under its own id: %b\n"
    (List.equal Int.equal submitted_ids cancelled_ids);
  printf
    "ids are 1..%d in order: %b\n"
    total
    (List.equal
       Int.equal
       submitted_ids
       (List.init total ~f:(fun i -> i + 1)));
  [%expect
    {|
    submitted: 15, cancelled: 15
    each submit cancelled under its own id: true
    ids are 1..15 in order: true
    |}];
  return ()
;;

let%expect_test "pct_marketable decides whether every order crosses the \
                 fundamental"
  =
  let report label ~pct_marketable =
    let config =
      Cancel_storm.create_config
        ~symbols:[ aapl ]
        ~cycles_per_tick:20
        ~size:10
        ~pct_marketable
        ~price_offset_cents:20
    in
    let bot, submitted, _cancelled =
      make_recording_bot (module Cancel_storm) config ()
    in
    let ctx = Bot_runtime.For_testing.context_of bot in
    let fundamental_cents =
      Price.to_int_cents (Bot_runtime.Context.fundamental ctx aapl)
    in
    let%bind () = drive_ticks bot ~ticks:1 in
    let crossing =
      List.count !submitted ~f:(crosses_fundamental ~fundamental_cents)
    in
    printf
      "%s: %d/%d orders cross the fundamental\n"
      label
      crossing
      (List.length !submitted);
    return ()
  in
  let%bind () = report "pct_marketable=100" ~pct_marketable:100 in
  let%bind () = report "pct_marketable=0" ~pct_marketable:0 in
  [%expect
    {|
    pct_marketable=100: 20/20 orders cross the fundamental
    pct_marketable=0: 0/20 orders cross the fundamental
    |}];
  return ()
;;

let%expect_test "a short storm: submit then cancel, over and over" =
  let config =
    Cancel_storm.create_config
      ~symbols:[ aapl ]
      ~cycles_per_tick:3
      ~size:7
      ~pct_marketable:100
      ~price_offset_cents:20
  in
  let bot, submitted, cancelled =
    make_recording_bot (module Cancel_storm) config ()
  in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_submitted submitted;
  printf
    "cancelled ids: %s\n"
    (List.rev !cancelled
     |> List.map ~f:(fun id -> Int.to_string (Client_order_id.to_int id))
     |> String.concat ~sep:", ");
  [%expect
    {|
    SELL AAPL 7@$149.80 DAY
    SELL AAPL 7@$149.80 DAY
    SELL AAPL 7@$149.80 DAY
    cancelled ids: 1, 2, 3
    |}];
  return ()
;;
