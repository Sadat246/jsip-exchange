(** Expect tests for {!Jsip_bots.Noise_trader_hansel}. *)

open! Core
open! Async
open Jsip_types
open! Jsip_bots
open Bot_harness

let%expect_test "noise trader submits a randomized ladder" =
  let config =
    Noise_trader_hansel.Config.create
      ~symbols:[ aapl ]
      ~mean_size:10
      ~tick_chance:(Percent.of_percentage 100.)
      ~aggressiveness:(Percent.of_percentage 50.)
      ~time_in_force_distribution:(day_ioc_mix ~day_pct:50.)
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Noise_trader_hansel) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let%bind () = drive_ticks bot ~ticks:15 in
  print_submitted submitted;
  [%expect
    {|
    BUY 0 7@$150.12 IOC
    SELL 0 13@$150.11 IOC
    SELL 0 12@$149.87 DAY
    SELL 0 9@$150.13 IOC
    SELL 0 12@$149.88 IOC
    BUY 0 13@$150.13 IOC
    BUY 0 12@$149.89 DAY
    SELL 0 10@$150.13 DAY
    BUY 0 10@$150.11 IOC
    SELL 0 7@$150.11 IOC
    BUY 0 10@$150.13 DAY
    BUY 0 7@$150.11 DAY
    SELL 0 9@$149.89 DAY
    BUY 0 13@$149.88 IOC
    BUY 0 12@$149.88 IOC
    |}];
  return ()
;;

let%expect_test "distributions look right over many ticks" =
  let aggressiveness_pct = 0.7 in
  let ioc_pct = 0.4 in
  let mean_size = 10 in
  let config =
    Noise_trader_hansel.Config.create
      ~symbols:[ aapl ]
      ~mean_size
      ~tick_chance:(Percent.of_percentage 100.)
      ~aggressiveness:(Percent.of_percentage (aggressiveness_pct *. 100.))
      ~time_in_force_distribution:
        (day_ioc_mix ~day_pct:((1. -. ioc_pct) *. 100.))
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Noise_trader_hansel) config ()
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
  let avg_size =
    Float.of_int
      (List.sum (module Int) requests ~f:(fun r -> Size.to_int r.size))
    /. Float.of_int total
  in
  printf "orders: %d\n" total;
  printf "buy fraction: %.2f (target 0.50)\n" (mean_of buys);
  printf "avg size: %.2f (target %d)\n" avg_size mean_size;
  printf
    "marketable fraction: %.2f (target %.2f)\n"
    (mean_of marketable)
    aggressiveness_pct;
  printf "ioc fraction: %.2f (target %.2f)\n" (mean_of ioc) ioc_pct;
  [%expect
    {|
    orders: 400
    buy fraction: 0.47 (target 0.50)
    avg size: 10.04 (target 10)
    marketable fraction: 0.66 (target 0.70)
    ioc fraction: 0.40 (target 0.40)
    |}];
  return ()
;;

let%expect_test "tick_chance gates whether any order is sent" =
  let config =
    Noise_trader_hansel.Config.create
      ~symbols:[ aapl ]
      ~mean_size:10
      ~tick_chance:(Percent.of_percentage 0.)
      ~aggressiveness:(Percent.of_percentage 50.)
      ~time_in_force_distribution:(day_ioc_mix ~day_pct:50.)
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Noise_trader_hansel) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let%bind () = drive_ticks bot ~ticks:50 in
  printf "orders sent with tick_chance 0.0: %d\n" (List.length !submitted);
  [%expect {| orders sent with tick_chance 0.0: 0 |}];
  return ()
;;
