open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness
open Harness

(* Hand-build a fill directly rather than driving the matching engine — we
   want to exercise {!Pnl} in isolation with controlled prices and sizes. *)
let fill ~aggressor ~aggressor_side ~resting ~price_cents ~size =
  { Fill.fill_id = 0
  ; symbol = aapl
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_order_id = Order_id.For_testing.of_int 1
  ; aggressor_participant = aggressor
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int 2
  ; resting_participant = resting
  ; aggressor_client_order_id = Client_order_id.of_int 1
  ; resting_client_order_id = Client_order_id.of_int 2
  }
;;

let print_summary pnl participant =
  print_string (Pnl.Summary.to_string_hum (Pnl.summary pnl participant))
;;

(* Alice buys 100 @ $150, sells 40 @ $155, then AAPL prints at $153. Alice
   realizes the $5 gain on the 40 shares sold and marks her remaining 60 long
   to $153. Bob is the counterparty on both fills, so he sees the mirror
   image: a short with a realized loss and a negative mark. *)
let%expect_test "partial close, mark-to-market, and the counterparty mirror" =
  let pnl =
    Pnl.empty
    |> fun pnl ->
    let pnl =
      Pnl.apply_fill
        pnl
        (fill
           ~aggressor:alice
           ~aggressor_side:Buy
           ~resting:bob
           ~price_cents:15000
           ~size:100)
    in
    let pnl =
      Pnl.apply_fill
        pnl
        (fill
           ~aggressor:alice
           ~aggressor_side:Sell
           ~resting:bob
           ~price_cents:15500
           ~size:40)
    in
    Pnl.apply_trade_report pnl ~symbol:aapl ~price:(Price.of_int_cents 15300)
  in
  print_summary pnl alice;
  [%expect
    {|
    0 inv=60 avg=$150.00 ref=$153.00 realized=$200.00 unrealized=$180.00
    TOTAL realized=$200.00 unrealized=$180.00
    |}];
  print_summary pnl bob;
  [%expect
    {|
    0 inv=-60 avg=$150.00 ref=$153.00 realized=-$200.00 unrealized=-$180.00
    TOTAL realized=-$200.00 unrealized=-$180.00
    |}]
;;

(* Alice buys 50 @ $100 then sells 80 @ $110, flipping from +50 long to -30
   short. The 50 shares that close realize a $10/share gain; the extra 30
   shares open a fresh short at $110. A print at $108 marks the short up. *)
let%expect_test "position flip realizes the closed leg and reopens at the \
                 fill"
  =
  let pnl =
    Pnl.empty
    |> fun pnl ->
    let pnl =
      Pnl.apply_fill
        pnl
        (fill
           ~aggressor:alice
           ~aggressor_side:Buy
           ~resting:bob
           ~price_cents:10000
           ~size:50)
    in
    let pnl =
      Pnl.apply_fill
        pnl
        (fill
           ~aggressor:alice
           ~aggressor_side:Sell
           ~resting:bob
           ~price_cents:11000
           ~size:80)
    in
    Pnl.apply_trade_report pnl ~symbol:aapl ~price:(Price.of_int_cents 10800)
  in
  print_summary pnl alice;
  [%expect
    {|
    0 inv=-30 avg=$110.00 ref=$108.00 realized=$500.00 unrealized=$60.00
    TOTAL realized=$500.00 unrealized=$60.00
    |}]
;;

(* Before any trade print there is no reference price, so unrealized P&L is
   reported as $0 even with an open position. *)
let%expect_test "unrealized is zero until a trade print sets the reference" =
  let pnl =
    Pnl.apply_fill
      Pnl.empty
      (fill
         ~aggressor:alice
         ~aggressor_side:Buy
         ~resting:bob
         ~price_cents:20000
         ~size:10)
  in
  print_summary pnl alice;
  [%expect
    {|
    0 inv=10 avg=$200.00 ref=-- realized=$0.00 unrealized=$0.00
    TOTAL realized=$0.00 unrealized=$0.00
    |}]
;;
