open! Core
open Jsip_types

module Key = struct
  module T = struct
    type t = Price.t * Order_id.t [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
  include Comparable.Make (T)
end

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t Key.Map.t
  ; mutable asks : Order.t Key.Map.t
  ; mutable orders_by_id : Order.t Order_id.Map.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Map.empty (module Key)
  ; asks = Map.empty (module Key)
  ; orders_by_id = Map.empty (module Order_id)
  }
;;

let symbol t = t.symbol

let side_map t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_map t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let find_key order = Order.price order, Order.order_id order

let add t order =
  let key = find_key order in
  let side = Order.side order in
  set_side_map t side (Map.set (side_map t side) ~key ~data:order);
  t.orders_by_id
  <- Map.set t.orders_by_id ~key:(Order.order_id order) ~data:order
;;

let remove' t order_id =
  match Map.find t.orders_by_id order_id with
  | None -> None
  | Some order_to_remove ->
    t.orders_by_id <- Map.remove t.orders_by_id order_id;
    let side = Order.side order_to_remove in
    let key_to_remove = find_key order_to_remove in
    set_side_map t side (Map.remove (side_map t side) key_to_remove);
    Some order_to_remove
;;

let remove t order_id =
  let order_to_remove = Map.find_exn t.orders_by_id order_id in
  t.orders_by_id <- Map.remove t.orders_by_id order_id;
  let side = Order.side order_to_remove in
  let key_to_remove = find_key order_to_remove in
  match side with
  | Side.Buy -> set_side_map t Side.Buy (Map.remove t.bids key_to_remove)
  | Side.Sell -> set_side_map t Side.Sell (Map.remove t.asks key_to_remove)
;;

let find t order_id = Map.find t.orders_by_id order_id

let best_order t side : Order.t option =
  match side with
  | Side.Buy ->
    (match Map.max_elt t.bids with
     | Some (_, order) -> Some order
     | None -> None)
  | Side.Sell ->
    (match Map.min_elt t.asks with
     | Some (_, order) -> Some order
     | None -> None)
;;

(* let better_order side order1 order2 = let price1 = Order.price order1 in
   let price2 = Order.price order2 in let more_aggressive_order = if
   Price.is_more_aggressive side ~price:price1 ~than:price2 then Some order1
   else if Price.is_more_aggressive side ~price:price2 ~than:price1 then Some
   order2 else None in match more_aggressive_order with | Some x -> x | None
   -> let time1 = Order.order_id order1 in let time2 = Order.order_id order2
   in if Order_id.compare time1 time2 < 0 then order1 else order2 ;;

   let compare_order side order1 order2 : int = let price1 = Order.price
   order1 in let price2 = Order.price order2 in let more_aggressive_order =
   if Price.is_more_aggressive side ~price:price1 ~than:price2 then -1 else
   if Price.is_more_aggressive side ~price:price2 ~than:price1 then 1 else 0
   in match more_aggressive_order with | 0 -> let time1 = Order.order_id
   order1 in let time2 = Order.order_id order2 in if Order_id.compare time1
   time2 < 0 then -1 else 1 | x -> x ;;

   let best_order t side = List.reduce (side_list t side) ~f:(fun x y ->
   better_order side x y) ;; *)

(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)

let find_match t incoming =
  let incoming_side = Order.side incoming in
  let opposite_side = Side.flip incoming_side in
  let incomingPrice = Order.price incoming in
  match best_order t opposite_side with
  | None -> None
  | Some order ->
    if Price.is_marketable
         incoming_side
         ~price:incomingPrice
         ~resting_price:(Order.price order)
    then Some order
    else None
;;

let orders_on_side t side = Map.data (side_map t side)
let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = Map.length (side_map t side)

let best_level t side : Level.t option =
  match best_order t side with
  | None -> None
  | Some order ->
    let price = Order.price order in
    let total_size =
      Map.fold
        (side_map t side)
        ~init:Size.zero
        ~f:(fun ~key:_ ~data:order acc ->
          if Price.equal (Order.price order) price
          then Size.( + ) acc (Order.remaining_size order)
          else acc)
    in
    Some { price; size = total_size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

(*=let snapshot_side t (side : Side.t) =
  let compare =
    match side with
    | Buy -> Comparable.reverse Level.compare
    | Sell -> Level.compare
  in
  orders_on_side t side |> List.map ~f:Level.of_order |> List.sort ~compare
;;*)

let compare_order side order1 order2 : int =
  let price1 = Order.price order1 in
  let price2 = Order.price order2 in
  let more_aggressive_order =
    if Price.is_more_aggressive side ~price:price1 ~than:price2
    then -1
    else if Price.is_more_aggressive side ~price:price2 ~than:price1
    then 1
    else 0
  in
  match more_aggressive_order with
  | 0 ->
    let time1 = Order.order_id order1 in
    let time2 = Order.order_id order2 in
    if Order_id.compare time1 time2 < 0 then -1 else 1
  | x -> x
;;

let snapshot_side t (side : Side.t) =
  let compare = compare_order side in
  orders_on_side t side |> List.sort ~compare |> List.map ~f:Level.of_order
;;

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end
