open! Core
open Jsip_types

module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving string ~case_insensitive ~capitalize:"SCREAMING_SNAKE_CASE"]
end

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t
[@@deriving sexp_of]

let to_string = function
  | Submit req -> [%string "ORDER %{req#Order.Request}"]
  | Book symbol -> [%string "BOOK %{symbol#Symbol}"]
  | Subscribe symbol -> [%string "SUBSCRIBE %{symbol#Symbol}"]
;;

let parse_command side line =
  let line = String.strip line in
  let default_participant = Participant.of_string "anonymous" in
  if String.is_empty line
  then Error "empty command"
  else (
    let parts =
      String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Error "empty command"
    | rest ->
      let open Result.Let_syntax in
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
           | [] -> Ok (Time_in_force.Day, [])
           | "as" :: _ -> Ok (Time_in_force.Day, rest)
           | tif_str :: rest' ->
             (match
                Or_error.try_with (fun () -> Time_in_force.of_string tif_str)
              with
              | Ok x -> Ok (x, rest')
              | Error _ ->
                Error
                  [%string
                    "invalid time-in-force: %{tif_str} (expected \
                     %{Time_in_force.all_str#String})"])
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
           "expected: BUY|SELL <symbol> <size> <price> \
            [%{Time.in_force.all_str}] [as <name>]"))
;;

let parse_command_with_default_participant side line ~default =
  let default_participant = Participant.of_string "anonymous" in
  match parse_command side line with
  | Error _ as err -> err
  | Ok request ->
    if Participant.equal request.participant default_participant
    then Ok { request with participant = default }
    else Ok request
;;

module Verb_action = struct
  type t =
    | Side of Side.t
    | Book
    | Subscribe
end

let parse_verb verb_string : Verb_action.t Or_error.t =
  let%map.Or_error verb =
    Or_error.try_with (fun () -> Verb.of_string verb_string)
  in
  match verb with
  | Buy -> Verb_action.Side Buy
  | Sell -> Side Sell
  | Book -> Book
  | Subscribe -> Subscribe
;;

let parse ?default_participant line =
  let sep_string =
    String.split line ~on:' '
    |> List.filter ~f:(fun c -> not (String.equal c ""))
  in
  match sep_string with
  | [] ->
    Or_error.error_string "Empty Command Line" (* how do i get an error? *)
  | verb :: args ->
    (match parse_verb verb with
     | Error error -> Error error
     | Ok (Side Buy) ->
       let command_line = String.concat ~sep:" " args in
       let base_participant =
         Option.value
           default_participant
           ~default:(Participant.of_string "anonymous")
       in
       let req =
         parse_command_with_default_participant
           Side.Buy
           command_line
           ~default:base_participant
       in
       (match req with
        | Error err_msg -> Or_error.error_string err_msg
        | Ok x -> Ok (Submit x))
     | Ok (Side Sell) ->
       let command_line = String.concat ~sep:" " args in
       let base_participant =
         Option.value
           default_participant
           ~default:(Participant.of_string "anonymous")
       in
       let req =
         parse_command_with_default_participant
           Side.Sell
           command_line
           ~default:base_participant
       in
       (match req with
        | Error err_msg -> Or_error.error_string err_msg
        | Ok x -> Ok (Submit x))
     | Ok Book ->
       (match args with
        | [ symb ] ->
          Or_error.try_with (fun () -> Book (Symbol.of_string symb))
        | _ -> Or_error.error_string "Error")
     | Ok Subscribe ->
       (match args with
        | [ symb ] ->
          Or_error.try_with (fun () -> Subscribe (Symbol.of_string symb))
        | _ -> Or_error.error_string "Error"))
;;

(* | symbol_line :: size_line::
   price_line::time_in_force_line::participant_line -> let symb =
   Symbol.of_string symbol_line in let sze = Size.of_string size_line in let
   prce = Price.of_string price_line in let tif = Time_in_force.of_string
   time_in_force_line in let participnt = Participant.of_string
   participant_line in *)
