(** Expect tests for {!Jsip_bots.Spammer}. *)

open! Core
open! Async
open Jsip_types
open! Jsip_bots
open Bot_harness

(* Build a [Resource_exhaustion] spammer config. The knob percentages are
   passed as plain floats; [price_jitter_cents] is fixed since it spreads
   orders across levels without changing whether they cross. *)
let spammer_config
  ~orders_per_burst
  ~buy_pct
  ~marketable_pct
  ~day_pct
  ~mean_size
  =
  let params : Spammer.Config.resource_exhaustion_params =
    { orders_per_burst
    ; buy_chance = Percent.of_percentage buy_pct
    ; marketable_chance = Percent.of_percentage marketable_pct
    ; time_in_force_distribution = day_ioc_mix ~day_pct
    ; mean_size
    ; price_jitter_cents = 20
    }
  in
  Spammer.Config.create
    ~symbols:[ aapl ]
    ~behavior:(Spammer.Config.Resource_exhaustion params)
;;

(* The core stress lever: each tick fires exactly [orders_per_burst] orders,
   so the count grows deterministically regardless of the random knobs. *)
let%expect_test "spammer fires a full burst of orders every tick" =
  let orders_per_burst = 25 in
  let config =
    spammer_config
      ~orders_per_burst
      ~buy_pct:50.
      ~marketable_pct:50.
      ~day_pct:50.
      ~mean_size:5
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Spammer) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let ticks = 4 in
  let%bind () = drive_ticks bot ~ticks in
  printf
    "orders after %d ticks of %d: %d\n"
    ticks
    orders_per_burst
    (List.length !submitted);
  [%expect {| orders after 4 ticks of 25: 100 |}];
  return ()
;;

(* Each knob at 100%/0% should fully determine its dimension of every order:
   side (buy_chance), whether it crosses the primed BBO (marketable_chance),
   and time-in-force (the distribution). This pins each knob to one property
   without reimplementing the price/side/tif picking. *)
let%expect_test "spammer knobs at their extremes fully determine each order" =
  let report label ~buy_pct ~marketable_pct ~day_pct =
    let config =
      spammer_config
        ~orders_per_burst:40
        ~buy_pct
        ~marketable_pct
        ~day_pct
        ~mean_size:6
    in
    let bot, submitted, _cancelled =
      make_recording_bot (module Spammer) config ()
    in
    let%bind () = feed_fixed_bbo bot in
    let%bind () = drive_ticks bot ~ticks:1 in
    let requests = !submitted in
    let total = List.length requests in
    let buys = List.count requests ~f:(fun r -> Side.equal r.side Buy) in
    let marketable = List.count requests ~f:is_marketable in
    let day =
      List.count requests ~f:(fun r ->
        Time_in_force.equal r.time_in_force Day)
    in
    printf
      "%s: total=%d buys=%d marketable=%d day=%d\n"
      label
      total
      buys
      marketable
      day;
    return ()
  in
  let%bind () =
    report
      "all buy / marketable / day"
      ~buy_pct:100.
      ~marketable_pct:100.
      ~day_pct:100.
  in
  let%bind () =
    report
      "all sell / resting / ioc"
      ~buy_pct:0.
      ~marketable_pct:0.
      ~day_pct:0.
  in
  [%expect
    {|
    all buy / marketable / day: total=40 buys=40 marketable=40 day=40
    all sell / resting / ioc: total=40 buys=0 marketable=0 day=0
    |}];
  return ()
;;

(* At intermediate settings the knobs should hold as frequencies across a
   large sample, and sizes should center on [mean_size]. Same shape as the
   noise-trader distribution test in [Test_noise_trader_hansel]. *)
let%expect_test "spammer knob distributions match their targets" =
  let buy_pct = 0.5 in
  let marketable_pct = 0.3 in
  let ioc_pct = 0.5 in
  let mean_size = 8 in
  let config =
    spammer_config
      ~orders_per_burst:40
      ~buy_pct:(buy_pct *. 100.)
      ~marketable_pct:(marketable_pct *. 100.)
      ~day_pct:((1. -. ioc_pct) *. 100.)
      ~mean_size
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Spammer) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  let%bind () = drive_ticks bot ~ticks:25 in
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
  printf "buy fraction: %.2f (target %.2f)\n" (mean_of buys) buy_pct;
  printf "avg size: %.2f (target %d)\n" avg_size mean_size;
  printf
    "marketable fraction: %.2f (target %.2f)\n"
    (mean_of marketable)
    marketable_pct;
  printf "ioc fraction: %.2f (target %.2f)\n" (mean_of ioc) ioc_pct;
  [%expect
    {|
    orders: 1000
    buy fraction: 0.49 (target 0.50)
    avg size: 7.94 (target 8)
    marketable fraction: 0.30 (target 0.30)
    ioc fraction: 0.51 (target 0.50)
    |}];
  return ()
;;

let print_state (params : Spammer.Config.pump_and_dump_params) =
  printf
    !"phase=%{sexp:Spammer.Config.pump_and_dump_phase} position=%d \
      realized_pnl_cents=%d\n"
    params.phase
    params.position
    (params.proceeds_cents - params.cost_cents)
;;

(* The whole scheme end to end: anchor at the fixed BBO's mid, accumulate a
   long on marketable buys, flip to distribute once the observed mid has run
   up past [pump_target_pct], unwind into the raised market, and land [Done]
   and flat. Fills are fed back so the position and P&L track a real run; the
   realized P&L is positive because we buy near 150.10 and sell near 152.00. *)
let%expect_test "pump-and-dump: accumulate, flip on price rise, then dump" =
  let params =
    Spammer.Config.pump_and_dump_params
      ~target_symbol:aapl
      ~pump_target_pct:(Percent.of_percentage 1.0)
      ~clip_size:20
      ~max_inventory:100
      ~give_up_ticks:100
      ~aggression_offset_cents:2
      ~entry_time_in_force:Ioc
  in
  let config =
    Spammer.Config.create ~symbols:[ aapl ] ~behavior:(Pump_and_dump params)
  in
  let bot, submitted, _cancelled =
    make_recording_bot (module Spammer) config ()
  in
  (* Anchor at mid 150.00 from the fixed BBO (bid 149.90 / ask 150.10). *)
  let%bind () = feed_fixed_bbo bot in
  (* Two accumulate ticks; confirm each buy clip with a fill so the long
     grows. *)
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Buy ~price_cents:15011 ~size:20 in
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Buy ~price_cents:15012 ~size:20 in
  print_state params;
  (* The market has now run up past the 1% target (mid 152.00 >= 151.50); the
     next tick flips to Distribute and submits nothing that tick. *)
  let%bind () = feed_bbo bot ~bid_cents:15190 ~ask_cents:15210 in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_state params;
  (* Distribute: sell clips until flat, feeding each sell back. *)
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Sell ~price_cents:15200 ~size:20 in
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Sell ~price_cents:15200 ~size:20 in
  print_state params;
  (* Flat now, so the next tick lands [Done]. *)
  let%bind () = drive_ticks bot ~ticks:1 in
  print_state params;
  print_submitted submitted;
  [%expect
    {|
    phase=Accumulate position=40 realized_pnl_cents=-600460
    phase=Distribute position=40 realized_pnl_cents=-600460
    phase=Distribute position=0 realized_pnl_cents=7540
    phase=Done position=0 realized_pnl_cents=7540
    BUY 0 20@$150.12 IOC
    BUY 0 20@$150.13 IOC
    SELL 0 20@$151.88 IOC
    SELL 0 20@$151.87 IOC
    |}];
  return ()
;;

(* When the price never rises to the target, the [give_up_ticks] budget still
   flips the scheme to Distribute so it unwinds rather than holding forever
   -- the honest "scheme failed" path. Here it buys into a flat market and
   dumps back into it at a loss. *)
let%expect_test "pump-and-dump: give_up_ticks unwinds a failed pump" =
  let params =
    Spammer.Config.pump_and_dump_params
      ~target_symbol:aapl
      ~pump_target_pct:(Percent.of_percentage 50.0)
      ~clip_size:10
      ~max_inventory:100
      ~give_up_ticks:3
      ~aggression_offset_cents:2
      ~entry_time_in_force:Ioc
  in
  let config =
    Spammer.Config.create ~symbols:[ aapl ] ~behavior:(Pump_and_dump params)
  in
  let bot, _submitted, _cancelled =
    make_recording_bot (module Spammer) config ()
  in
  let%bind () = feed_fixed_bbo bot in
  (* Two buys into a flat market, then the third tick hits the give-up budget
     and flips to Distribute without buying. *)
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Buy ~price_cents:15011 ~size:10 in
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Buy ~price_cents:15011 ~size:10 in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_state params;
  (* Unwind the 20-share long at the (unchanged, weak) bid. *)
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Sell ~price_cents:14989 ~size:10 in
  let%bind () = drive_ticks bot ~ticks:1 in
  let%bind () = feed_self_fill bot ~side:Sell ~price_cents:14989 ~size:10 in
  let%bind () = drive_ticks bot ~ticks:1 in
  print_state params;
  [%expect
    {|
    phase=Distribute position=20 realized_pnl_cents=-300220
    phase=Done position=0 realized_pnl_cents=-440
    |}];
  return ()
;;
