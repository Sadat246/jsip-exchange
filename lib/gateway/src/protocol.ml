open! Core
open Jsip_types

(* Default participant when no "as <name>" is specified in the command.
   [parse_command_with_default_participant] overrides this with the
   caller-supplied default. *)
let default_participant = Participant.of_string "anonymous"

let parse_command line =
  let line = String.strip line in
  if String.is_empty line
  then Error "empty command"
  else (
    let parts =
      String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Error "empty command"
    | side_str :: rest ->
      let open Result.Let_syntax in
      let%bind side =
        match String.uppercase side_str with
        | "BUY" -> Ok Side.Buy
        | "SELL" -> Ok Side.Sell
        | other ->
          Error [%string "unknown command: %{other} (expected BUY or SELL)"]
      in
      (match rest with
       | client_order_id_str :: symbol_str :: size_str :: price_str :: rest
         ->
         let%bind client_order_id =
           try Ok (Client_order_id.of_string client_order_id_str) with
           | exn ->
             let exn_str = Exn.to_string exn in
             Error
               [%string
                 "invalid client order id: %{client_order_id_str}\n\
                  exception: %{exn_str}"]
         in
         let%bind size =
           match Int.of_string_opt size_str with
           | Some n when n > 0 -> Ok n
           | Some _ -> Error "size must be positive"
           | None -> Error [%string "invalid size: %{size_str}"]
         in
         let%bind price =
           try Ok (Price.of_string price_str) with
           | exn ->
             let exn_str = Exn.to_string exn in
             Error
               [%string "invalid price: %{price_str}\nexception: %{exn_str}"]
         in
         let%bind symbol =
           try Ok (Symbol.of_string symbol_str) with
           | exn ->
             let exn_str = Exn.to_string exn in
             Error
               [%string
                 "invalid symbol: %{symbol_str}\nexception: %{exn_str}"]
         in
         let%bind time_in_force, rest =
           match rest with
           | tif_str :: rest' ->
             (match String.uppercase tif_str with
              | "IOC" -> Ok (Time_in_force.Ioc, rest')
              | "DAY" -> Ok (Day, rest')
              | "AS" -> Ok (Day, rest)
              | _ ->
                Error
                  [%string
                    "unknown time-in-force: %{tif_str} (expected DAY or IOC)"])
           | [] -> Ok (Day, [])
         in
         let%bind participant =
           match rest with
           | "as" :: name :: _ | "AS" :: name :: _ ->
             Ok (Participant.of_string name)
           | [] -> Ok default_participant
           | _ ->
             let trailing = String.concat ~sep:" " rest in
             Error [%string "unexpected trailing arguments: %{trailing}"]
         in
         Ok
           ({ symbol
            ; participant
            ; side
            ; price
            ; size = Size.of_int size
            ; client_order_id
            ; time_in_force
            }
            : Order.Request.t)
       | _ ->
         Error
           "expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]"))
;;

let parse_command_with_default_participant line ~default =
  match parse_command line with
  | Error _ as err -> err
  | Ok request ->
    if Participant.equal request.participant default_participant
    then Ok { request with participant = default }
    else Ok request
;;

let format_event = function
  | Exchange_event.Order_accept { order_id; request } ->
    sprintf
      "ACCEPTED id=%s %s %s %d@%s %s %s"
      (Order_id.to_string order_id)
      (Symbol.to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Client_order_id.to_string request.client_order_id)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill -> [%string "FILL %{fill#Fill}"]
  | Order_cancel
      { order_id
      ; participant = _
      ; symbol
      ; remaining_size
      ; reason
      ; client_order_id = _
      } ->
    sprintf
      "CANCELLED id=%s %s remaining=%d reason=%s"
      (Order_id.to_string order_id)
      (Symbol.to_string symbol)
      (Size.to_int remaining_size)
      (Cancel_reason.to_string reason)
  | Cancel_reject { participant; client_order_id; reason } ->
    sprintf
      "CANCEL_REJECT participant=%s client_order_id=%s reason=%s"
      (Participant.to_string participant)
      (Client_order_id.to_string client_order_id)
      reason
  | Order_reject { request; reason } ->
    sprintf
      "REJECTED %s %s %d@%s reason=%s"
      (Symbol.to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Best_bid_offer_update { symbol; bbo } ->
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    [%string "BBO %{symbol#Symbol} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol; price; size } ->
    let size = Size.to_int size in
    [%string "TRADE %{symbol#Symbol} %{price#Price} x%{size#Int}"]
;;

let format_events events =
  List.map events ~f:format_event |> String.concat ~sep:"\n"
;;
