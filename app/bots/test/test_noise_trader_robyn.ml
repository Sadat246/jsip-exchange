(** Expect tests for {!Jsip_bots.Robyn_noise_trader}. *)

open! Core
open! Async
open Jsip_types
open! Jsip_bots
open Bot_harness

let%expect_test "noise trader submits a randomized stream of orders" =
  let config =
    Robyn_noise_trader.create_config
      ~symbols:[ aapl ]
      ~avg_size:10
      ~tick_chance:1.0
      ~aggressiveness_pct:50
      ~ioc_pct:50
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Robyn_noise_trader) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let%bind () = drive_ticks bot ~ticks:15 in
  print_submitted submitted;
  [%expect
    {|
    BUY AAPL 8@$149.85 DAY
    SELL AAPL 10@$149.89 DAY
    SELL AAPL 10@$149.86 IOC
    SELL AAPL 11@$150.12 DAY
    SELL AAPL 8@$149.86 DAY
    BUY AAPL 11@$149.89 DAY
    BUY AAPL 12@$149.89 IOC
    SELL AAPL 11@$150.13 IOC
    BUY AAPL 10@$150.11 DAY
    SELL AAPL 9@$149.88 DAY
    BUY AAPL 11@$150.15 IOC
    BUY AAPL 11@$150.12 IOC
    SELL AAPL 8@$149.89 IOC
    BUY AAPL 9@$149.86 DAY
    BUY AAPL 11@$149.88 DAY
    |}];
  return ()
;;

let%expect_test "distributions look right over many ticks" =
  let aggressiveness_pct = 70 in
  let ioc_pct = 40 in
  let avg_size = 10 in
  let config =
    Robyn_noise_trader.create_config
      ~symbols:[ aapl ]
      ~avg_size
      ~tick_chance:1.0
      ~aggressiveness_pct
      ~ioc_pct
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Robyn_noise_trader) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let ticks = 400 in
  let%bind () = drive_ticks bot ~ticks in
  let requests = List.rev !submitted in
  let total = List.length requests in
  let buys = List.count requests ~f:(fun r -> Side.equal r.side Buy) in
  let marketable = List.count requests ~f:is_marketable in
  let ioc =
    List.count requests ~f:(fun r -> Time_in_force.equal r.time_in_force Ioc)
  in
  let mean_of counts = Float.of_int counts /. Float.of_int total in
  let avg_of_size =
    Float.of_int
      (List.sum (module Int) requests ~f:(fun r -> Size.to_int r.size))
    /. Float.of_int total
  in
  printf "orders: %d\n" total;
  printf "buy fraction: %.2f (target 0.50)\n" (mean_of buys);
  printf "avg size: %.2f (target %d)\n" avg_of_size avg_size;
  printf
    "marketable fraction: %.2f (target %.2f)\n"
    (mean_of marketable)
    (Float.of_int aggressiveness_pct /. 100.);
  printf
    "ioc fraction: %.2f (target %.2f)\n"
    (mean_of ioc)
    (Float.of_int ioc_pct /. 100.);
  [%expect
    {|
    orders: 400
    buy fraction: 0.47 (target 0.50)
    avg size: 10.03 (target 10)
    marketable fraction: 0.73 (target 0.70)
    ioc fraction: 0.40 (target 0.40)
    |}];
  return ()
;;

let%expect_test "tick_chance gates whether any order is sent" =
  let config =
    Robyn_noise_trader.create_config
      ~symbols:[ aapl ]
      ~avg_size:10
      ~tick_chance:0.0
      ~aggressiveness_pct:50
      ~ioc_pct:50
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Robyn_noise_trader) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let%bind () = drive_ticks bot ~ticks:50 in
  printf "orders sent with tick_chance 0.0: %d\n" (List.length !submitted);
  [%expect {| orders sent with tick_chance 0.0: 0 |}];
  return ()
;;
