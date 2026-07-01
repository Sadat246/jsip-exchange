open! Core
open Jsip_types
open Jsip_gateway

let print_parse line =
  match Exchange_command.parse line with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok command -> print_endline (Exchange_command.to_string command)
;;

let%expect_test "parse: order command" =
  print_parse "BUY AAPL 100 150.25";
  [%expect {| ERROR: expected: BUY|SELL <symbol> <size> <price> [%{Time.in_force.all_str}] [as <name>] |}]
;;

let%expect_test "parse: order with IOC and participant" =
  print_parse "SELL GOOG 75 2800.50 IOC as Bob";
  [%expect {|
    ERROR: invalid client order id: GOOG
    exception: (Failure "Int.of_string: \"GOOG\"")
    |}]
;;

let%expect_test "parse: BOOK with symbol" =
  print_parse "BOOK AAPL";
  [%expect {| BOOK AAPL |}]
;;

let%expect_test "parse: SUBSCRIBE case-insensitive" =
  print_parse "subscribe aapl";
  print_parse "Subscribe AAPL";
  [%expect {|
    SUBSCRIBE aapl
    SUBSCRIBE AAPL
    |}]
;;

let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  match
    Exchange_command.parse "BUY AAPL 100 150.00" ~default_participant:default
  with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok command ->
    print_endline [%string "%{command#Exchange_command}"];
    [%expect.unreachable];
  [%expect {| ERROR: expected: BUY|SELL <symbol> <size> <price> [%{Time.in_force.all_str}] [as <name>] |}]
;;

let%expect_test "default participant: explicit as overrides default" =
  let default = Participant.of_string "DefaultTrader" in
  match
    Exchange_command.parse
      "BUY AAPL 100 150.00 as Alice"
      ~default_participant:default
  with
  | Error msg -> print_endline [%string "ERROR: %{Error.to_string_hum msg}"]
  | Ok command ->
    print_endline [%string "%{command#Exchange_command}"];
    [%expect.unreachable];
  [%expect {|
    ERROR: invalid client order id: AAPL
    exception: (Failure "Int.of_string: \"AAPL\"")
    |}]
;;
