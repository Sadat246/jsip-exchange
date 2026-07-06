(** Expect tests for {!Jsip_bots.Robyn_market_maker_bot}.

    The maker seeds a two-sided ladder in [on_start] and re-quotes on every
    fill, so the tests drive [manual_start] and then feed [Fill] events.
    Prices are deterministic: the oracle sits at a constant $150.00 (zero
    volatility) and, with no cached book, the maker defaults to a 50c
    half-spread. Ladders are printed with [print_ladder] because the buy/sell
    submits within a level race, so submission order is unspecified. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
open! Jsip_bots
open Bot_harness

let bob = Participant.of_string "Bob"

let make_config
  ?(size_per_level = 10)
  ?(num_levels = 3)
  ?(inventory_skew_cents_per_share = 2)
  ()
  =
  Robyn_market_maker_bot.create_config
    ()
    ~size_per_level
    ~num_levels
    ~inventory_skew_cents_per_share
    ~symbols:[ aapl ]
;;

(* A fill in which [alice] (the bot) trades [size] shares on [alice_side], as
   the aggressor. This maker re-quotes the whole book on any fill and keys
   only off its net inventory, so which party rested is irrelevant here. *)
let fill ~alice_side ~size =
  Exchange_event.Fill
    { fill_id = 1
    ; symbol = aapl
    ; price = Price.of_int_cents 15000
    ; size = Size.of_int size
    ; aggressor_order_id = Order_id.For_testing.of_int 1
    ; aggressor_client_order_id = Client_order_id.of_int 1
    ; aggressor_participant = alice
    ; aggressor_side = alice_side
    ; resting_order_id = Order_id.For_testing.of_int 2
    ; resting_client_order_id = Client_order_id.of_int 2
    ; resting_participant = bob
    }
;;

let%expect_test "on_start seeds a symmetric two-sided ladder around fair \
                 value"
  =
  let config = make_config () in
  let bot, submitted, _cancelled =
    make_recording_bot (module Robyn_market_maker_bot) config ()
  in
  let%bind () = Bot_runtime.For_testing.manual_start bot in
  print_ladder submitted;
  [%expect
    {|
    BUY AAPL 10@$149.48 DAY
    BUY AAPL 10@$149.49 DAY
    BUY AAPL 10@$149.50 DAY
    SELL AAPL 10@$150.50 DAY
    SELL AAPL 10@$150.51 DAY
    SELL AAPL 10@$150.52 DAY
    |}];
  return ()
;;

let%expect_test "a fill re-quotes the whole book, skewed by new inventory" =
  let report label ~alice_side ~size =
    let config = make_config ~num_levels:2 () in
    let bot, submitted, cancelled =
      make_recording_bot (module Robyn_market_maker_bot) config ()
    in
    let%bind () = Bot_runtime.For_testing.manual_start bot in
    (* Drop the initial ladder; we want to see only the re-quote. *)
    submitted := [];
    let%bind () = Bot_runtime.feed_event bot (fill ~alice_side ~size) in
    printf "%s (cancelled %d resting):\n" label (List.length !cancelled);
    print_ladder submitted;
    return ()
  in
  (* Buying leaves us long, so the re-quote skews down (2c/share * 20 = 40c)
     to lean on the sell side; selling leaves us short and skews up. *)
  let%bind () = report "bought 20" ~alice_side:Buy ~size:20 in
  let%bind () = report "sold 20" ~alice_side:Sell ~size:20 in
  [%expect
    {|
    bought 20 (cancelled 4 resting):
    BUY AAPL 10@$149.09 DAY
    BUY AAPL 10@$149.10 DAY
    SELL AAPL 10@$150.10 DAY
    SELL AAPL 10@$150.11 DAY
    sold 20 (cancelled 4 resting):
    BUY AAPL 10@$149.89 DAY
    BUY AAPL 10@$149.90 DAY
    SELL AAPL 10@$150.90 DAY
    SELL AAPL 10@$150.91 DAY
    |}];
  return ()
;;

let%expect_test "a cached BBO tightens the spread on the next re-quote" =
  let config =
    make_config ~num_levels:1 ~inventory_skew_cents_per_share:0 ()
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Robyn_market_maker_bot) config ()
  in
  let%bind () = Bot_runtime.For_testing.manual_start bot in
  printf "seeded with no book (half-spread defaults to 50c):\n";
  print_ladder submitted;
  submitted := [];
  (* The fixed BBO is 20c wide (14990 / 15010); the maker caches it but only
     re-quotes on a fill. Skew is 0, so the fill triggers the re-quote
     without moving the center. *)
  let%bind () = feed_fixed_bbo bot in
  let%bind () = Bot_runtime.feed_event bot (fill ~alice_side:Buy ~size:10) in
  printf
    "after caching a 20c-wide book, re-quote hugs it (half-spread 10c):\n";
  print_ladder submitted;
  [%expect
    {|
    seeded with no book (half-spread defaults to 50c):
    BUY AAPL 10@$149.50 DAY
    SELL AAPL 10@$150.50 DAY
    after caching a 20c-wide book, re-quote hugs it (half-spread 10c):
    BUY AAPL 10@$149.90 DAY
    SELL AAPL 10@$150.10 DAY
    |}];
  return ()
;;
