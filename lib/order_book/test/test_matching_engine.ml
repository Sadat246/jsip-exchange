open! Core
open Jsip_types
open Jsip_order_book
open Jsip_test_harness

(** Helper: submit and print, filtering out market data events for cleaner
    matching-logic tests. *)
let submit t request =
  let events = Matching_engine.submit (Harness.engine t) request in
  Harness.print_events ~show:Harness.Show.no_market_data events;
  events
;;

let submit_ t request = ignore (submit t request : Exchange_event.t list)

let show_bbo =
  Harness.Show.only (function
    | Exchange_event.Best_bid_offer_update _ -> true
    | _ -> false)
;;

(* ================================================================ *)
(* Basic matching tests *)
(* ================================================================ *)

let%expect_test "single buy order, nothing to match" =
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ());
  [%expect {| ACCEPTED id=1 AAPL BUY 100@$150.00 DAY |}]
;;

let%expect_test "two orders that don't cross" =
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ~participant:Harness.alice ());
  submit_ t (Harness.sell ~price_cents:15100 ~participant:Harness.bob ());
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$150.00 DAY
    ACCEPTED id=2 AAPL SELL 100@$151.00 DAY
    |}]
;;

let%expect_test "exact cross at same price" =
  let t = Harness.create () in
  submit_ t (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  submit_ t (Harness.buy ~price_cents:15000 ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    |}]
;;

let%expect_test "buy crosses at resting price, not aggressor price" =
  let t = Harness.create () in
  submit_ t (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  submit_ t (Harness.buy ~price_cents:15100 ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    ACCEPTED id=2 AAPL BUY 100@$151.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    |}]
;;

let%expect_test "partial fill: buy is larger than resting sell" =
  let t = Harness.create () in
  submit_
    t
    (Harness.sell ~price_cents:15000 ~size:60 ~participant:Harness.bob ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 60@$150.00 DAY
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x60 aggressor=2(Alice) BUY resting=1(Bob)
    |}];
  (* Remainder rests on the book *)
  Harness.print_book t Harness.aapl;
  [%expect
    {|
    === AAPL ===
      BIDS:
        $150.00 x40
      ASKS: (empty)
      BBO: $150.00 x40 / -
    |}]
;;

let%expect_test "aggressor sweeps multiple resting orders" =
  let t = Harness.create () in
  submit_
    t
    (Harness.sell ~price_cents:15000 ~size:50 ~participant:Harness.bob ());
  submit_
    t
    (Harness.sell
       ~price_cents:15000
       ~size:80
       ~participant:Harness.charlie
       ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 50@$150.00 DAY
    ACCEPTED id=2 AAPL SELL 80@$150.00 DAY
    ACCEPTED id=3 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x80 aggressor=3(Alice) BUY resting=2(Charlie)
    FILL fill_id=2 AAPL $150.00 x20 aggressor=3(Alice) BUY resting=1(Bob)
    |}]
;;

(* ================================================================ *)
(* IOC (Immediate-or-Cancel) orders *)
(* ================================================================ *)

let%expect_test "IOC: no match means immediate cancel" =
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ~time_in_force:Ioc ());
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$150.00 IOC
    CANCELLED id=1 AAPL remaining=100 reason=IOC_REMAINDER
    |}]
;;

let%expect_test "IOC: partial fill then cancel remainder" =
  let t = Harness.create () in
  submit_
    t
    (Harness.sell ~price_cents:15000 ~size:40 ~participant:Harness.bob ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ~time_in_force:Ioc ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 40@$150.00 DAY
    ACCEPTED id=2 AAPL BUY 100@$150.00 IOC
    FILL fill_id=1 AAPL $150.00 x40 aggressor=2(Alice) BUY resting=1(Bob)
    CANCELLED id=2 AAPL remaining=60 reason=IOC_REMAINDER
    |}]
;;

let%expect_test "IOC: full fill means no cancel event" =
  let t = Harness.create () in
  submit_
    t
    (Harness.sell ~price_cents:15000 ~size:100 ~participant:Harness.bob ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ~time_in_force:Ioc ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    ACCEPTED id=2 AAPL BUY 100@$150.00 IOC
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    |}]
;;

let%expect_test "IOC: does not rest on book" =
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ~time_in_force:Ioc ());
  Harness.print_book t Harness.aapl;
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$150.00 IOC
    CANCELLED id=1 AAPL remaining=100 reason=IOC_REMAINDER
    === AAPL ===
      BIDS: (empty)
      ASKS: (empty)
      BBO: - / -
    |}]
;;

(* ================================================================ *)
(* Rejections *)
(* ================================================================ *)

let%expect_test "rejected: unknown symbol" =
  let t = Harness.create () in
  submit_
    t
    (Harness.buy ~price_cents:15000 ~symbol:(Symbol.of_string "NOPE") ());
  [%expect {| REJECTED NOPE BUY 100@$150.00 reason=unknown symbol |}]
;;

(* ================================================================ *)
(* Multi-symbol support *)
(* ================================================================ *)

let%expect_test "orders for different symbols don't cross" =
  let t = Harness.create () in
  submit_
    t
    (Harness.sell
       ~price_cents:15000
       ~symbol:Harness.aapl
       ~participant:Harness.bob
       ());
  submit_ t (Harness.buy ~price_cents:15000 ~symbol:Harness.tsla ());
  (* Buy for TSLA should not match the AAPL sell *)
  Harness.print_book t Harness.aapl;
  Harness.print_book t Harness.tsla;
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    ACCEPTED id=2 TSLA BUY 100@$150.00 DAY
    === AAPL ===
      BIDS: (empty)
      ASKS:
        $150.00 x100
      BBO: - / $150.00 x100
    === TSLA ===
      BIDS:
        $150.00 x100
      ASKS: (empty)
      BBO: $150.00 x100 / -
    |}]
;;

(* ================================================================ *)
(* Engine queries *)
(* ================================================================ *)

let%expect_test "book: returns book for known symbol, None for unknown" =
  let t = Harness.create () in
  let engine = Harness.engine t in
  [%test_result: bool]
    (Option.is_some (Matching_engine.book engine Harness.aapl))
    ~expect:true;
  [%test_result: _ option]
    (Matching_engine.book engine (Symbol.of_string "NOPE"))
    ~expect:None
;;

(* ================================================================ *)
(* Price priority (known naive bug) *)
(* ================================================================ *)

let%expect_test "price priority: naive impl matches first-found, not best" =
  let t = Harness.create () in
  (* Charlie sells at $10.00, then Bob at $10.05. A correct engine should
     match the buy against Charlie's $10.00 (best ask). The naive
     list-prepend means Bob's $10.05 is at the front. *)
  submit_ t (Harness.sell ~price_cents:1000 ~participant:Harness.charlie ());
  submit_ t (Harness.sell ~price_cents:1005 ~participant:Harness.bob ());
  submit_ t (Harness.buy ~price_cents:1005 ());
  (* NOTE: The buyer pays $10.05 instead of $10.00 — $0.05/share of
     unnecessary cost! *)
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$10.00 DAY
    ACCEPTED id=2 AAPL SELL 100@$10.05 DAY
    ACCEPTED id=3 AAPL BUY 100@$10.05 DAY
    FILL fill_id=1 AAPL $10.00 x100 aggressor=3(Alice) BUY resting=1(Charlie)
    |}]
;;

(* ================================================================ *)
(* Market data events *)
(* ================================================================ *)

let%expect_test "BBO update emitted when order rests on book" =
  let t = Harness.create () in
  let events = Harness.submit_quiet t (Harness.buy ~price_cents:15000 ()) in
  Harness.print_events ~show:show_bbo events;
  [%expect {| BBO AAPL bid=$150.00 x100 ask=- |}];
  let events = Harness.submit_quiet t (Harness.sell ~price_cents:15100 ()) in
  Harness.print_events ~show:show_bbo events;
  [%expect {| BBO AAPL bid=$150.00 x100 ask=$151.00 x100 |}]
;;

let%expect_test "BBO update: reflects new best after fill" =
  let t = Harness.create () in
  let events = Harness.submit_quiet t (Harness.sell ~price_cents:15000 ()) in
  Harness.print_events ~show:show_bbo events;
  [%expect {| BBO AAPL bid=- ask=$150.00 x100 |}];
  let events = Harness.submit_quiet t (Harness.buy ~price_cents:15000 ()) in
  Harness.print_events ~show:show_bbo events;
  (* Both sides empty after the cross *)
  [%expect {| BBO AAPL bid=- ask=- |}]
;;

let%expect_test "BBO update: not emitted when BBO unchanged" =
  let t = Harness.create () in
  (* Add a sell at $151, then another at $152. The BBO doesn't change on the
     second add (best ask is still $151). *)
  Harness.submit_quiet_
    t
    (Harness.sell ~price_cents:15100 ~participant:Harness.bob ());
  let events =
    Harness.submit_quiet
      t
      (Harness.sell ~price_cents:15200 ~participant:Harness.charlie ())
  in
  let bbo_count =
    List.count events ~f:(function
      | Exchange_event.Best_bid_offer_update _ -> true
      | _ -> false)
  in
  [%test_result: int] bbo_count ~expect:0
;;

let%expect_test "trade report emitted for each fill" =
  let t = Harness.create () in
  Harness.submit_quiet_
    t
    (Harness.sell ~price_cents:15000 ~size:50 ~participant:Harness.bob ());
  Harness.submit_quiet_
    t
    (Harness.sell
       ~price_cents:15000
       ~size:80
       ~participant:Harness.charlie
       ());
  let events =
    Harness.submit_quiet t (Harness.buy ~price_cents:15000 ~size:100 ())
  in
  Harness.print_events
    ~show:
      (Harness.Show.only (function
        | Exchange_event.Trade_report _ -> true
        | _ -> false))
    events;
  [%expect {|
    TRADE AAPL $150.00 x80
    TRADE AAPL $150.00 x20
    |}]
;;

let%expect_test "no market data events on rejection" =
  let t = Harness.create () in
  let events =
    Harness.submit_quiet
      t
      (Harness.buy ~price_cents:15000 ~symbol:(Symbol.of_string "NOPE") ())
  in
  let md_count =
    List.count events ~f:(function
      | Exchange_event.Best_bid_offer_update _ | Trade_report _ -> true
      | _ -> false)
  in
  [%test_result: int] md_count ~expect:0
;;

(* ================================================================ *)
(* End-to-end scenarios *)
(* ================================================================ *)

let%expect_test "scenario: two participants trade, book reflects state" =
  let t = Harness.create () in
  (* Alice posts bids, Bob posts asks *)
  submit_ t (Harness.buy ~price_cents:14990 ~size:100 ());
  submit_ t (Harness.buy ~price_cents:14980 ~size:200 ());
  submit_
    t
    (Harness.sell ~price_cents:15010 ~size:100 ~participant:Harness.bob ());
  submit_
    t
    (Harness.sell ~price_cents:15020 ~size:150 ~participant:Harness.bob ());
  (* Charlie crosses the spread: buys at $150.10 *)
  submit_
    t
    (Harness.buy ~price_cents:15010 ~size:50 ~participant:Harness.charlie ());
  Harness.print_book t Harness.aapl;
  Harness.print_bbo t Harness.aapl;
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$149.90 DAY
    ACCEPTED id=2 AAPL BUY 200@$149.80 DAY
    ACCEPTED id=3 AAPL SELL 100@$150.10 DAY
    ACCEPTED id=4 AAPL SELL 150@$150.20 DAY
    ACCEPTED id=5 AAPL BUY 50@$150.10 DAY
    FILL fill_id=1 AAPL $150.10 x50 aggressor=5(Charlie) BUY resting=3(Bob)
    === AAPL ===
      BIDS:
        $149.90 x100
        $149.80 x200
      ASKS:
        $150.10 x50
        $150.20 x150
      BBO: $149.90 x100 / $150.10 x50
    BBO AAPL: $149.90 x100 / $150.10 x50
    |}]
;;

let%expect_test "scenario: aggressive IOC sweeps entire book" =
  let t = Harness.create () in
  submit_
    t
    (Harness.sell ~price_cents:15000 ~size:50 ~participant:Harness.bob ());
  submit_
    t
    (Harness.sell
       ~price_cents:15010
       ~size:50
       ~participant:Harness.charlie
       ());
  submit_
    t
    (Harness.sell ~price_cents:15020 ~size:50 ~participant:Harness.bob ());
  (* IOC buy for 200 at $150.20 — sweeps all 150 shares, cancels 50 *)
  submit_ t (Harness.buy ~price_cents:15020 ~size:200 ~time_in_force:Ioc ());
  Harness.print_book t Harness.aapl;
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 50@$150.00 DAY
    ACCEPTED id=2 AAPL SELL 50@$150.10 DAY
    ACCEPTED id=3 AAPL SELL 50@$150.20 DAY
    ACCEPTED id=4 AAPL BUY 200@$150.20 IOC
    FILL fill_id=1 AAPL $150.20 x50 aggressor=4(Alice) BUY resting=3(Bob)
    FILL fill_id=2 AAPL $150.10 x50 aggressor=4(Alice) BUY resting=2(Charlie)
    FILL fill_id=3 AAPL $150.00 x50 aggressor=4(Alice) BUY resting=1(Bob)
    CANCELLED id=4 AAPL remaining=50 reason=IOC_REMAINDER
    === AAPL ===
      BIDS: (empty)
      ASKS: (empty)
      BBO: - / -
    |}]
;;

let%expect_test "scenario: order IDs are globally sequential" =
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ~symbol:Harness.aapl ());
  submit_
    t
    (Harness.sell
       ~price_cents:20000
       ~symbol:Harness.tsla
       ~participant:Harness.bob
       ());
  submit_
    t
    (Harness.buy
       ~price_cents:28000
       ~symbol:Harness.goog
       ~participant:Harness.charlie
       ());
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$150.00 DAY
    ACCEPTED id=2 TSLA SELL 100@$200.00 DAY
    ACCEPTED id=3 GOOG BUY 100@$280.00 DAY
    |}]
;;

let%expect_test "scenario: fill IDs are globally sequential" =
  let t = Harness.create () in
  (* Set up two separate crosses *)
  submit_ t (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  submit_
    t
    (Harness.sell
       ~price_cents:20000
       ~symbol:Harness.tsla
       ~participant:Harness.charlie
       ());
  submit_ t (Harness.buy ~price_cents:15000 ());
  submit_ t (Harness.buy ~price_cents:20000 ~symbol:Harness.tsla ());
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    ACCEPTED id=2 TSLA SELL 100@$200.00 DAY
    ACCEPTED id=3 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=3(Alice) BUY resting=1(Bob)
    ACCEPTED id=4 TSLA BUY 100@$200.00 DAY
    FILL fill_id=2 TSLA $200.00 x100 aggressor=4(Alice) BUY resting=2(Charlie)
    |}]
;;
