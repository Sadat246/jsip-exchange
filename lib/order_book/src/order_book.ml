open! Core
open Jsip_types
open Async_log_kernel.Ppx_log_syntax

module Key = struct
  module T = struct
    type t = Price.t * Order_id.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

type t =
  { symbol : Symbol_id.t
  ; mutable bids : Order.t Key.Map.t
  ; mutable asks : Order.t Key.Map.t
  ; reverse_index : (Side.t * Key.t) Order_id.Table.t
  ; mutable bid_orders_at_price : Order.t Queue.t Price.Map.t
  ; mutable ask_orders_at_price : Order.t Queue.t Price.Map.t
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Key.Map.empty
  ; asks = Key.Map.empty
  ; reverse_index = Order_id.Table.create ()
  ; bid_orders_at_price = Price.Map.empty
  ; ask_orders_at_price = Price.Map.empty
  }
;;

let symbol t = t.symbol

let side_data t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let add_to_price_map t (side : Side.t) order =
  let price = Order.price order in
  let price_map =
    match side with
    | Buy -> t.bid_orders_at_price
    | Sell -> t.ask_orders_at_price
  in
  match Map.find price_map price with
  | Some queue -> Queue.enqueue queue order
  | None ->
    let queue = Queue.create () in
    Queue.enqueue queue order;
    let new_price_map = Map.set price_map ~key:price ~data:queue in
    (match side with
     | Buy -> t.bid_orders_at_price <- new_price_map
     | Sell -> t.ask_orders_at_price <- new_price_map)
;;

let remove_from_price_map t (side : Side.t) order =
  let price = Order.price order in
  let order_id = Order.order_id order in
  let price_map =
    match side with
    | Buy -> t.bid_orders_at_price
    | Sell -> t.ask_orders_at_price
  in
  match Map.find price_map price with
  | None -> ()
  | Some queue ->
    (* drop this order from the price's FIFO queue, in place *)
    Queue.filter_inplace queue ~f:(fun o ->
      not (Order_id.equal (Order.order_id o) order_id));
    (* if that emptied the price level, drop the price entirely *)
    if Queue.is_empty queue
    then (
      let new_price_map = Map.remove price_map price in
      match side with
      | Buy -> t.bid_orders_at_price <- new_price_map
      | Sell -> t.ask_orders_at_price <- new_price_map)
;;

let add t order =
  let side = Order.side order in
  let order_id = Order.order_id order in
  let key = Order.price order, order_id in
  let existing_orders = side_data t side in
  match Map.add existing_orders ~key ~data:order with
  | `Duplicate -> [%log.info "BUG: duplicate (price * order_id) key"]
  | `Ok new_data ->
    set_side t side new_data;
    add_to_price_map t side order;
    Hashtbl.set t.reverse_index ~key:order_id ~data:(side, key)
;;

let remove' t order_id =
  match Hashtbl.find_and_remove t.reverse_index order_id with
  | None -> None
  | Some (side, key) ->
    let side_data = side_data t side in
    let%bind.Option order = Map.find side_data key in
    let updated_side = Map.remove side_data key in
    set_side t side updated_side;
    remove_from_price_map t side order;
    Some order
;;

let remove t order_id = ignore (remove' t order_id)

let find t order_id =
  let%bind.Option side, key = Hashtbl.find t.reverse_index order_id in
  let data = side_data t side in
  Map.find data key
;;

(* Find the resting order [incoming] should match against, if any. We walk
   the opposite side in price-time priority -- best price first, then FIFO
   within a price level -- and take the first eligible order (see below),
   visiting the most aggressive resting orders first.

   Self-trade prevention: an order never matches against its own owner. We
   skip the incoming participant's own resting orders and match against the
   first order from another participant. The skipped orders stay on the book;
   the aggressor simply trades deeper. Because deeper orders are at
   equal-or-worse prices, once the best *other* order is not marketable,
   nothing behind it can be either -- so a single price check on that first
   eligible order decides the whole thing. *)
let find_match t incoming =
  let incoming_side = Order.side incoming in
  let incoming_participant = Order.participant incoming in
  let opposite_side = Side.flip incoming_side in
  let price_map =
    match opposite_side with
    | Buy -> t.bid_orders_at_price
    | Sell -> t.ask_orders_at_price
  in
  (* Walk best price first (highest bid / lowest ask), then FIFO within each
     price level -- i.e. price-time priority. We read the per-price queues
     rather than the [(price, order_id)] key map: a single key can only sort
     price and time in the *same* direction, which reverses time priority on
     the bid side. The queues store arrival order independently, so both
     sides stay FIFO. The sequence is lazy, so we only materialize the levels
     we actually reach -- usually just the best one. *)
  let price_order =
    match opposite_side with
    | Buy -> `Decreasing_key
    | Sell -> `Increasing_key
  in
  let in_priority_order =
    Map.to_sequence price_map ~order:price_order
    |> Sequence.concat_map ~f:(fun (price, queue) ->
      Queue.to_list queue
      |> Sequence.of_list
      |> Sequence.map ~f:(fun order -> price, order))
  in
  let%bind.Option best_price, best_resting_order =
    Sequence.find in_priority_order ~f:(fun (_price, resting) ->
      not
        (Participant.equal (Order.participant resting) incoming_participant))
  in
  Option.some_if
    (Price.is_marketable
       incoming_side
       ~price:(Order.price incoming)
       ~resting_price:best_price)
    best_resting_order
;;

let orders_on_side t side = side_data t side |> Map.data
let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = Map.length (side_data t side)

let best_price_and_queue t (side : Side.t) =
  let price_map =
    match side with
    | Buy -> t.bid_orders_at_price
    | Sell -> t.ask_orders_at_price
  in
  match side with
  | Buy -> Map.max_elt price_map
  | Sell -> Map.min_elt price_map
;;

let best_level t side : Level.t option =
  match best_price_and_queue t side with
  | None -> None
  | Some (price, queue) ->
    let size =
      Queue.fold queue ~init:Size.zero ~f:(fun acc o ->
        Size.( + ) acc (Order.remaining_size o))
    in
    Some { Level.price; size }
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

let snapshot_side t (side : Side.t) =
  let compare =
    match side with
    | Buy -> Comparable.reverse Level.compare
    | Sell -> Level.compare
  in
  orders_on_side t side
  |> List.fold
       ~init:([] : Level.t list)
       ~f:(fun acc order ->
         match acc with
         | level :: rest when Price.equal level.price (Order.price order) ->
           { level with size = Size.( + ) level.size (Order.size order) }
           :: rest
         | acc -> Level.of_order order :: acc)
  |> List.sort ~compare
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
