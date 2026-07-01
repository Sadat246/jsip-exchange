open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_side : Side.t
  ; resting_order_id : Order_id.t
  ; resting_participant : Participant.t
  ; aggressor_client_order_id : Client_order_id.t
  ; resting_client_order_id : Client_order_id.t
  }
[@@deriving sexp, bin_io]

let to_string
  ({ fill_id
   ; symbol
   ; price
   ; size
   ; aggressor_order_id
   ; aggressor_participant
   ; aggressor_side
   ; resting_order_id
   ; resting_participant
   ; aggressor_client_order_id
   ; resting_client_order_id
   } :
    t)
  =
  ignore aggressor_client_order_id;
  ignore resting_client_order_id;
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s(%s) %s resting=%s(%s)"
    fill_id
    (Symbol.to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Order_id.to_string aggressor_order_id)
    (Participant.to_string aggressor_participant)
    (Side.to_string aggressor_side)
    (Order_id.to_string resting_order_id)
    (Participant.to_string resting_participant)
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size

let to_participant_view t participant =
  if Participant.equal participant t.aggressor_participant
  then (
    let verb =
      match t.aggressor_side with
      | Side.Buy -> "bought"
      | Side.Sell -> "sold"
    in
    Some
      (sprintf
         "You %s %d %s at %s"
         verb
         (Size.to_int t.size)
         (Symbol.to_string t.symbol)
         (Price.to_string_dollar t.price)))
  else if Participant.equal participant t.resting_participant
  then (
    let verb =
      match Side.flip t.aggressor_side with
      | Side.Buy -> "bought"
      | Side.Sell -> "sold"
    in
    Some
      (sprintf
         "You %s %d %s at %s"
         verb
         (Size.to_int t.size)
         (Symbol.to_string t.symbol)
         (Price.to_string_dollar t.price)))
  else None
;;
