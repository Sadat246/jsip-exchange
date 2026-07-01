(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_test_harness
open Jsip_market_maker
open E2e_helpers

let default_config : Market_maker.Config.t =
  { participant = Harness.market_maker
  ; symbol = Harness.aapl
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  }
;;

let%expect_test "seed_book: places symmetric bids and asks around fair value"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let%bind () = Market_maker.seed_book default_config (connection mm) in
    [%expect
      {|
      [MarketMaker] ACCEPTED id=1 AAPL BUY 100@$149.90 0 DAY
      [MarketMaker] ACCEPTED id=2 AAPL SELL 100@$150.10 1 DAY
      [MarketMaker] ACCEPTED id=3 AAPL BUY 100@$149.89 2 DAY
      [MarketMaker] ACCEPTED id=4 AAPL SELL 100@$150.11 3 DAY
      [MarketMaker] ACCEPTED id=5 AAPL BUY 100@$149.88 4 DAY
      [MarketMaker] ACCEPTED id=6 AAPL SELL 100@$150.12 5 DAY
      |}];
    return ())
;;
