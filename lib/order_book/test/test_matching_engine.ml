open! Core
open Jsip_types
open Jsip_order_book
open Jsip_test_harness

(** Helpers: submit / cancel and print the resulting events.

    [?show] controls which events get printed. The default is
    [Harness.Show.no_market_data], which hides BBO updates and trade reports
    for cleaner matching-logic output. Tests that want to verify market-data
    events were emitted can pass [~show:Harness.Show.all] (see the "Market
    data events" section for examples). *)

let submit
  ?(participant = Harness.alice)
  ?(show = Harness.Show.no_market_data)
  t
  request
  =
  let events =
    Matching_engine.submit (Harness.engine t) ~participant request
  in
  Harness.print_events ~show events;
  events
;;

let submit_ ?participant ?show t request =
  ignore (submit ?participant ?show t request : Exchange_event.t list)
;;

let cancel
  ?(participant = Harness.alice)
  ?(show = Harness.Show.no_market_data)
  t
  client_order_id
  =
  let events =
    Matching_engine.cancel (Harness.engine t) ~participant ~client_order_id
  in
  Harness.print_events ~show events;
  events
;;

let cancel_ ?participant ?show t client_order_id =
  ignore
    (cancel ?participant ?show t client_order_id : Exchange_event.t list)
;;

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
  [%expect {| ACCEPTED server_id=1 client_id=101 0 BUY 100@$150.00 DAY |}]
;;

let%expect_test "two orders that don't cross" =
  let t = Harness.create () in
  submit_ t ~participant:Harness.alice (Harness.buy ~price_cents:15000 ());
  submit_ t ~participant:Harness.bob (Harness.sell ~price_cents:15100 ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 BUY 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 SELL 100@$151.00 DAY
    |}]
;;

let%expect_test "exact cross at same price" =
  let t = Harness.create () in
  submit_ t ~participant:Harness.bob (Harness.sell ~price_cents:15000 ());
  submit_ t (Harness.buy ~price_cents:15000 ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x100 aggressor=[server_id=2 client_id=102 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    |}]
;;

let%expect_test "buy crosses at resting price, not aggressor price" =
  let t = Harness.create () in
  submit_ t ~participant:Harness.bob (Harness.sell ~price_cents:15000 ());
  submit_ t (Harness.buy ~price_cents:15100 ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 BUY 100@$151.00 DAY
    FILL fill_id=1 0 $150.00 x100 aggressor=[server_id=2 client_id=102 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    |}]
;;

let%expect_test "partial fill: buy is larger than resting sell" =
  let t = Harness.create () in
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ~size:60 ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 60@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x60 aggressor=[server_id=2 client_id=102 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    |}];
  (* Remainder rests on the book *)
  Harness.print_book t Harness.aapl;
  [%expect
    {|
    === 0 ===
      BIDS:
        $150.00 x40
      ASKS: (empty)
      BBO: $150.00 x40 / -
    |}]
;;

let%expect_test "aggressor sweeps multiple resting orders" =
  let t = Harness.create () in
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ~size:50 ());
  submit_
    t
    ~participant:Harness.charlie
    (Harness.sell ~price_cents:15000 ~size:80 ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 50@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 SELL 80@$150.00 DAY
    ACCEPTED server_id=3 client_id=103 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x50 aggressor=[server_id=3 client_id=103 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    FILL fill_id=2 0 $150.00 x50 aggressor=[server_id=3 client_id=103 Alice] BUY resting=[server_id=2 client_id=102 Charlie]
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
    ACCEPTED server_id=1 client_id=101 0 BUY 100@$150.00 IOC
    CANCELLED server_id=1 client_id=101 0 remaining=100 reason=IOC_REMAINDER
    |}]
;;

let%expect_test "IOC: partial fill then cancel remainder" =
  let t = Harness.create () in
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ~size:40 ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ~time_in_force:Ioc ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 40@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 BUY 100@$150.00 IOC
    FILL fill_id=1 0 $150.00 x40 aggressor=[server_id=2 client_id=102 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    CANCELLED server_id=2 client_id=102 0 remaining=60 reason=IOC_REMAINDER
    |}]
;;

let%expect_test "IOC: full fill means no cancel event" =
  let t = Harness.create () in
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ~size:100 ());
  submit_ t (Harness.buy ~price_cents:15000 ~size:100 ~time_in_force:Ioc ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 BUY 100@$150.00 IOC
    FILL fill_id=1 0 $150.00 x100 aggressor=[server_id=2 client_id=102 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    |}]
;;

let%expect_test "IOC: does not rest on book" =
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ~time_in_force:Ioc ());
  Harness.print_book t Harness.aapl;
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 BUY 100@$150.00 IOC
    CANCELLED server_id=1 client_id=101 0 remaining=100 reason=IOC_REMAINDER
    === 0 ===
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
    (Harness.buy ~price_cents:15000 ~symbol:(Symbol_id.Private.of_int 99) ());
  [%expect
    {| REJECTED client_id=101 99 BUY 100@$150.00 reason=unknown symbol |}]
;;

let%expect_test "id validation: id one past the last symbol is rejected" =
  (* The default harness engine trades 3 symbols, so valid ids are 0..2. [3]
     is the first out-of-range index — the exact boundary the bounds check
     guards ([i < Array.length books]). *)
  let t = Harness.create () in
  submit_
    t
    (Harness.buy ~price_cents:15000 ~symbol:(Symbol_id.Private.of_int 3) ());
  [%expect
    {| REJECTED client_id=101 3 BUY 100@$150.00 reason=unknown symbol |}]
;;

let%expect_test "id validation: a negative id is rejected" =
  (* [bin_io] will happily deserialize any int off the wire, including a
     negative one; the [i >= 0] half of the bounds check is what stops it. *)
  let t = Harness.create () in
  submit_
    t
    (Harness.buy
       ~price_cents:15000
       ~symbol:(Symbol_id.Private.of_int (-1))
       ());
  [%expect
    {| REJECTED client_id=101 -1 BUY 100@$150.00 reason=unknown symbol |}]
;;

let%expect_test "id validation: the last valid id (num_symbols - 1) is accepted" =
  (* GOOG is id 2 — the highest in-range id — so the order rests rather than
     rejecting. Confirms the check accepts the top of the range, not just
     strictly-less. *)
  let t = Harness.create () in
  submit_ t (Harness.buy ~price_cents:15000 ~symbol:Harness.goog ());
  [%expect {| ACCEPTED server_id=1 client_id=101 2 BUY 100@$150.00 DAY |}]
;;

let%expect_test "id validation: book lookup returns None for an out-of-range id" =
  let t = Harness.create () in
  Harness.print_book t (Symbol_id.Private.of_int 99);
  [%expect {| unknown symbol 99 |}]
;;

(* ================================================================ *)
(* Duplicate client order ID detection *)
(* ================================================================ *)

let%expect_test "duplicate client order id from same participant is rejected"
  =
  let t = Harness.create () in
  let cid = Client_order_id.of_int 42 in
  submit_ t (Harness.buy ~price_cents:15000 ~client_order_id:cid ());
  submit_ t (Harness.buy ~price_cents:15100 ~client_order_id:cid ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=42 0 BUY 100@$150.00 DAY
    REJECTED client_id=42 0 BUY 100@$151.00 reason=duplicate client order id
    |}]
;;

let%expect_test "same client order id from different participants is fine" =
  let t = Harness.create () in
  let cid = Client_order_id.of_int 42 in
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy ~price_cents:15000 ~client_order_id:cid ());
  submit_
    t
    ~participant:Harness.bob
    (Harness.buy ~price_cents:15100 ~client_order_id:cid ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=42 0 BUY 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=42 0 BUY 100@$151.00 DAY
    |}]
;;

let%expect_test "client order id stays reserved after a full fill" =
  let t = Harness.create () in
  let cid = Client_order_id.of_int 42 in
  (* Bob places a resting sell, Alice fills it entirely — Alice's buy is
     fully filled and leaves no remainder on the book. *)
  submit_ t ~participant:Harness.bob (Harness.sell ~price_cents:15000 ());
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy ~price_cents:15000 ~client_order_id:cid ());
  (* Alice tries to reuse cid — the ID is still reserved. *)
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy ~price_cents:15100 ~client_order_id:cid ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=42 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x100 aggressor=[server_id=2 client_id=42 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    REJECTED client_id=42 0 BUY 100@$151.00 reason=duplicate client order id
    |}]
;;

(* The following three tests cover the IOC order-lifecycle paths. Reservation
   is supposed to happen at acceptance, above the matching loop, so it should
   be independent of whether the order matches, partially matches, or fully
   matches. Even so, it's worth pinning each path down so a future refactor
   that moves reservation into a fill-dependent branch is caught immediately. *)

let%expect_test "client order id stays reserved after IOC with no match" =
  let t = Harness.create () in
  let cid = Client_order_id.of_int 42 in
  (* IOC with nothing to match — accepted, then cancelled with Ioc_remainder. *)
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy
       ~price_cents:15000
       ~client_order_id:cid
       ~time_in_force:Ioc
       ());
  (* Alice tries to reuse cid — rejected. The retry uses [Harness.buy]'s
     default [time_in_force] (Day), which also confirms the reservation is
     keyed on [(participant, client_order_id)] alone, independent of the
     request's TIF. *)
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy ~price_cents:15000 ~client_order_id:cid ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=42 0 BUY 100@$150.00 IOC
    CANCELLED server_id=1 client_id=42 0 remaining=100 reason=IOC_REMAINDER
    REJECTED client_id=42 0 BUY 100@$150.00 reason=duplicate client order id
    |}]
;;

let%expect_test "client order id stays reserved after IOC with partial fill" =
  let t = Harness.create () in
  let cid = Client_order_id.of_int 42 in
  (* Bob's resting sell has less size than Alice's incoming IOC buy — some of
     Alice's order fills, the rest is cancelled with Ioc_remainder. *)
  submit_
    t
    ~participant:Harness.bob
    (Harness.sell ~price_cents:15000 ~size:30 ());
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy
       ~price_cents:15000
       ~size:100
       ~client_order_id:cid
       ~time_in_force:Ioc
       ());
  (* Alice tries to reuse cid — rejected. *)
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy ~price_cents:15000 ~client_order_id:cid ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 30@$150.00 DAY
    ACCEPTED server_id=2 client_id=42 0 BUY 100@$150.00 IOC
    FILL fill_id=1 0 $150.00 x30 aggressor=[server_id=2 client_id=42 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    CANCELLED server_id=2 client_id=42 0 remaining=70 reason=IOC_REMAINDER
    REJECTED client_id=42 0 BUY 100@$150.00 reason=duplicate client order id
    |}]
;;

let%expect_test "client order id stays reserved after IOC with full fill" =
  let t = Harness.create () in
  let cid = Client_order_id.of_int 42 in
  (* Bob's resting sell exactly matches Alice's IOC buy — full fill, no
     Ioc_remainder cancel event. *)
  submit_
    t
    ~participant:Harness.bob
    (Harness.sell ~price_cents:15000 ~size:100 ());
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy
       ~price_cents:15000
       ~size:100
       ~client_order_id:cid
       ~time_in_force:Ioc
       ());
  (* Alice tries to reuse cid — rejected. *)
  submit_
    t
    ~participant:Harness.alice
    (Harness.buy ~price_cents:15000 ~client_order_id:cid ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=42 0 BUY 100@$150.00 IOC
    FILL fill_id=1 0 $150.00 x100 aggressor=[server_id=2 client_id=42 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    REJECTED client_id=42 0 BUY 100@$150.00 reason=duplicate client order id
    |}]
;;

(* ================================================================ *)
(* Multi-symbol support *)
(* ================================================================ *)

let%expect_test "orders for different symbols don't cross" =
  let t = Harness.create () in
  submit_
    t
    ~participant:Harness.bob
    (Harness.sell ~price_cents:15000 ~symbol:Harness.aapl ());
  submit_ t (Harness.buy ~price_cents:15000 ~symbol:Harness.tsla ());
  (* Buy for TSLA should not match the AAPL sell *)
  Harness.print_book t Harness.aapl;
  Harness.print_book t Harness.tsla;
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 1 BUY 100@$150.00 DAY
    === 0 ===
      BIDS: (empty)
      ASKS:
        $150.00 x100
      BBO: - / $150.00 x100
    === 1 ===
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
    (Matching_engine.book engine (Symbol_id.Private.of_int 99))
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
  submit_ t ~participant:Harness.charlie (Harness.sell ~price_cents:1000 ());
  submit_ t ~participant:Harness.bob (Harness.sell ~price_cents:1005 ());
  submit_ t (Harness.buy ~price_cents:1005 ());
  (* NOTE: The buyer pays $10.05 instead of $10.00 — $0.05/share of
     unnecessary cost! *)
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$10.00 DAY
    ACCEPTED server_id=2 client_id=102 0 SELL 100@$10.05 DAY
    ACCEPTED server_id=3 client_id=103 0 BUY 100@$10.05 DAY
    FILL fill_id=1 0 $10.00 x100 aggressor=[server_id=3 client_id=103 Alice] BUY resting=[server_id=1 client_id=101 Charlie]
    |}]
;;

(* ================================================================ *)
(* Market data events *)
(* ================================================================ *)

let%expect_test "BBO update emitted when order rests on book" =
  let t = Harness.create () in
  let events = Harness.submit_quiet t (Harness.buy ~price_cents:15000 ()) in
  Harness.print_events ~show:show_bbo events;
  [%expect {| BBO 0 bid=$150.00 x100 ask=- |}];
  let events = Harness.submit_quiet t (Harness.sell ~price_cents:15100 ()) in
  Harness.print_events ~show:show_bbo events;
  [%expect {| BBO 0 bid=$150.00 x100 ask=$151.00 x100 |}]
;;

let%expect_test "BBO update: reflects new best after fill" =
  let t = Harness.create () in
  (* Resting order is Bob's so Alice's incoming buy can cross it -- a
     participant never trades against its own resting order. *)
  let events =
    Harness.submit_quiet
      ~participant:Harness.bob
      t
      (Harness.sell ~price_cents:15000 ())
  in
  Harness.print_events ~show:show_bbo events;
  [%expect {| BBO 0 bid=- ask=$150.00 x100 |}];
  let events = Harness.submit_quiet t (Harness.buy ~price_cents:15000 ()) in
  Harness.print_events ~show:show_bbo events;
  (* Both sides empty after the cross *)
  [%expect {| BBO 0 bid=- ask=- |}]
;;

let%expect_test "BBO update: not emitted when BBO unchanged" =
  let t = Harness.create () in
  (* Add a sell at $151, then another at $152. The BBO doesn't change on the
     second add (best ask is still $151). *)
  Harness.submit_quiet_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15100 ());
  let events =
    Harness.submit_quiet
      ~participant:Harness.charlie
      t
      (Harness.sell ~price_cents:15200 ())
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
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ~size:50 ());
  Harness.submit_quiet_
    t
    ~participant:Harness.charlie
    (Harness.sell ~price_cents:15000 ~size:80 ());
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
    TRADE 0 $150.00 x50
    TRADE 0 $150.00 x50
    |}]
;;

let%expect_test "no market data events on rejection" =
  let t = Harness.create () in
  let events =
    Harness.submit_quiet
      t
      (Harness.buy
         ~price_cents:15000
         ~symbol:(Symbol_id.Private.of_int 99)
         ())
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
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15010 ~size:100 ());
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15020 ~size:150 ());
  (* Charlie crosses the spread: buys at $150.10 *)
  submit_
    ~participant:Harness.charlie
    t
    (Harness.buy ~price_cents:15010 ~size:50 ());
  Harness.print_book t Harness.aapl;
  Harness.print_bbo t Harness.aapl;
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 BUY 100@$149.90 DAY
    ACCEPTED server_id=2 client_id=102 0 BUY 200@$149.80 DAY
    ACCEPTED server_id=3 client_id=103 0 SELL 100@$150.10 DAY
    ACCEPTED server_id=4 client_id=104 0 SELL 150@$150.20 DAY
    ACCEPTED server_id=5 client_id=105 0 BUY 50@$150.10 DAY
    FILL fill_id=1 0 $150.10 x50 aggressor=[server_id=5 client_id=105 Charlie] BUY resting=[server_id=3 client_id=103 Bob]
    === 0 ===
      BIDS:
        $149.90 x100
        $149.80 x200
      ASKS:
        $150.10 x50
        $150.20 x150
      BBO: $149.90 x100 / $150.10 x50
    BBO 0: $149.90 x100 / $150.10 x50
    |}]
;;

let%expect_test "scenario: aggressive IOC sweeps entire book" =
  let t = Harness.create () in
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ~size:50 ());
  submit_
    t
    ~participant:Harness.charlie
    (Harness.sell ~price_cents:15010 ~size:50 ());
  submit_
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15020 ~size:50 ());
  (* IOC buy for 200 at $150.20 — sweeps all 150 shares, cancels 50 *)
  submit_ t (Harness.buy ~price_cents:15020 ~size:200 ~time_in_force:Ioc ());
  Harness.print_book t Harness.aapl;
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 50@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 0 SELL 50@$150.10 DAY
    ACCEPTED server_id=3 client_id=103 0 SELL 50@$150.20 DAY
    ACCEPTED server_id=4 client_id=104 0 BUY 200@$150.20 IOC
    FILL fill_id=1 0 $150.00 x50 aggressor=[server_id=4 client_id=104 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    FILL fill_id=2 0 $150.10 x50 aggressor=[server_id=4 client_id=104 Alice] BUY resting=[server_id=2 client_id=102 Charlie]
    FILL fill_id=3 0 $150.20 x50 aggressor=[server_id=4 client_id=104 Alice] BUY resting=[server_id=3 client_id=103 Bob]
    CANCELLED server_id=4 client_id=104 0 remaining=50 reason=IOC_REMAINDER
    === 0 ===
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
    ~participant:Harness.bob
    (Harness.sell ~price_cents:20000 ~symbol:Harness.tsla ());
  submit_
    t
    ~participant:Harness.charlie
    (Harness.buy ~price_cents:28000 ~symbol:Harness.goog ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 BUY 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 1 SELL 100@$200.00 DAY
    ACCEPTED server_id=3 client_id=103 2 BUY 100@$280.00 DAY
    |}]
;;

let%expect_test "scenario: fill IDs are globally sequential" =
  let t = Harness.create () in
  (* Set up two separate crosses *)
  submit_ t ~participant:Harness.bob (Harness.sell ~price_cents:15000 ());
  submit_
    t
    ~participant:Harness.charlie
    (Harness.sell ~price_cents:20000 ~symbol:Harness.tsla ());
  submit_ t (Harness.buy ~price_cents:15000 ());
  submit_ t (Harness.buy ~price_cents:20000 ~symbol:Harness.tsla ());
  [%expect
    {|
    ACCEPTED server_id=1 client_id=101 0 SELL 100@$150.00 DAY
    ACCEPTED server_id=2 client_id=102 1 SELL 100@$200.00 DAY
    ACCEPTED server_id=3 client_id=103 0 BUY 100@$150.00 DAY
    FILL fill_id=1 0 $150.00 x100 aggressor=[server_id=3 client_id=103 Alice] BUY resting=[server_id=1 client_id=101 Bob]
    ACCEPTED server_id=4 client_id=104 1 BUY 100@$200.00 DAY
    FILL fill_id=2 1 $200.00 x100 aggressor=[server_id=4 client_id=104 Alice] BUY resting=[server_id=2 client_id=102 Charlie]
    |}]
;;
