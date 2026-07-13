open! Core
open Jsip_types

let format_ids ~order_id ~client_order_id =
  sprintf
    "server_id=%s client_id=%s"
    (Order_id.to_string order_id)
    (Client_order_id.to_string client_order_id)
;;

(* [Fill.to_string] lives in [lib/types] and can only print the raw symbol id.
   To render the symbol as a name we reformat the fill here, in the gateway's
   presentation layer, routing the symbol through [symbol_to_string]. The
   format is otherwise identical to [Fill.to_string]. *)
let format_fill ~symbol_to_string (fill : Fill.t) =
  sprintf
    "fill_id=%d %s %s x%d aggressor=[%s %s] %s resting=[%s %s]"
    fill.fill_id
    (symbol_to_string fill.symbol)
    (Price.to_string_dollar fill.price)
    (Size.to_int fill.size)
    (format_ids
       ~order_id:fill.aggressor_order_id
       ~client_order_id:fill.aggressor_client_order_id)
    (Participant.to_string fill.aggressor_participant)
    (Side.to_string fill.aggressor_side)
    (format_ids
       ~order_id:fill.resting_order_id
       ~client_order_id:fill.resting_client_order_id)
    (Participant.to_string fill.resting_participant)
;;

(* [symbol_to_string] decides how a [Symbol_id.t] is shown. It defaults to the
   raw int (what the pure types can print); the interactive client passes an
   id->name resolver built from the symbol directory, so events render with
   symbol names. The default keeps existing (server-side, test) output
   unchanged. *)
let format_event ?(symbol_to_string = Symbol_id.to_string) = function
  | Exchange_event.Order_accept { order_id; participant = _; request } ->
    sprintf
      "ACCEPTED %s %s %s %d@%s %s"
      (format_ids ~order_id ~client_order_id:request.client_order_id)
      (symbol_to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill ->
    let fill_str = format_fill ~symbol_to_string fill in
    [%string "FILL %{fill_str}"]
  | Order_cancel
      { order_id
      ; client_order_id
      ; participant = _
      ; symbol
      ; remaining_size
      ; reason
      } ->
    sprintf
      "CANCELLED %s %s remaining=%d reason=%s"
      (format_ids ~order_id ~client_order_id)
      (symbol_to_string symbol)
      (Size.to_int remaining_size)
      (Cancel_reason.to_string reason)
  | Order_reject { participant = _; request; reason } ->
    sprintf
      "REJECTED client_id=%s %s %s %d@%s reason=%s"
      (Client_order_id.to_string request.client_order_id)
      (symbol_to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Cancel_reject { participant = _; client_order_id; reason } ->
    sprintf
      "CANCEL REJECTED client_id=%s reason=%s"
      (Client_order_id.to_string client_order_id)
      reason
  | Best_bid_offer_update { symbol; bbo } ->
    let sym = symbol_to_string symbol in
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    [%string "BBO %{sym} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol; price; size } ->
    let sym = symbol_to_string symbol in
    let size = Size.to_int size in
    [%string "TRADE %{sym} %{price#Price} x%{size#Int}"]
;;

let format_events ?symbol_to_string events =
  List.map events ~f:(format_event ?symbol_to_string)
  |> String.concat ~sep:"\n"
;;
