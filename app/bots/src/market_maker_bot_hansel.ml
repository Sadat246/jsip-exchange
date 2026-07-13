open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

type symbol_info =
  { mutable inventory : int
  ; mutable half_spread_cents : int
  ; mutable last_spread_cents : int option
      (* Most recent market spread from a BBO update; [None] when a side of
         the book is empty. Used to decide whether the market is dislocated. *)
  ; mutable quoted : (int * int) option
      (* The [(skewed_fair, half_spread)] our currently-resting ladder was
         placed at, or [None] if we're standing aside. Lets us skip
         re-quoting when the target hasn't moved (otherwise our own quotes
         would loop us forever). *)
  ; resting_orders : int Client_order_id.Table.t
  }

module Config = struct
  type t =
    { symbols : Symbol_id.t list
    ; size_per_level : int
    ; num_levels : int
    ; inventory_skew_cents_per_share : int
    ; initial_half_spread_cents : int
    ; min_half_spread_cents : int
        (* Floor on our half-spread, so we never quote a zero/crossed market. *)
    ; max_spread_cents : int
        (* Reseed tolerance: if the observed market spread is wider than
           this, we assume a whale swept the book and stand aside instead of
           quoting into the gap -- the liquidity will fill back in. *)
    ; symbol_state : symbol_info Symbol_id.Table.t
    ; generator : Client_order_id.Generator.t
    }

  let create
    ~symbols
    ~size_per_level
    ~num_levels
    ~inventory_skew_cents_per_share
    ~half_spread_cents
    ~min_half_spread_cents
    ~max_spread_cents
    =
    { symbols
    ; size_per_level
    ; num_levels
    ; inventory_skew_cents_per_share
    ; initial_half_spread_cents = half_spread_cents
    ; min_half_spread_cents
    ; max_spread_cents
    ; symbol_state = Symbol_id.Table.create ()
    ; generator = Client_order_id.Generator.create ()
    }
  ;;
end

let name = "Market_Maker"

let get_info (config : Config.t) symbol =
  Hashtbl.find_or_add config.symbol_state symbol ~default:(fun () ->
    { inventory = 0
    ; half_spread_cents = config.initial_half_spread_cents
    ; last_spread_cents = None
    ; quoted = None
    ; resting_orders = Client_order_id.Table.create ()
    })
;;

(* Is the market healthy enough to quote into? A missing spread (a side swept
   empty) or one wider than [max_spread_cents] means "no". *)
let market_ok (config : Config.t) (info : symbol_info) =
  match info.last_spread_cents with
  | Some spread_cents -> spread_cents <= config.max_spread_cents
  | None -> false
;;

(* The center of our ladder and the half-spread to use, read fresh from the
   oracle and skewed by current inventory. *)
let quote_targets (config : Config.t) context symbol (info : symbol_info) =
  let fair = Price.to_int_cents (Context.fundamental context symbol) in
  let skewed_fair =
    fair - (info.inventory * config.inventory_skew_cents_per_share)
  in
  skewed_fair, info.half_spread_cents
;;

let place_ladder (config : Config.t) context symbol ~skewed_fair ~half_spread
  =
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = half_spread + level in
      let%bind _ =
        Context.submit
          context
          ({ symbol
           ; participant = Context.participant context
           ; side = Buy
           ; price = Price.of_int_cents (skewed_fair - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id =
               Client_order_id.Generator.next config.generator
           }
           : Order.Request.t)
      and _ =
        Context.submit
          context
          ({ symbol
           ; participant = Context.participant context
           ; side = Sell
           ; price = Price.of_int_cents (skewed_fair + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id =
               Client_order_id.Generator.next config.generator
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

(* Cancel whatever we currently have working for [symbol], then quote a fresh
   ladder around the (skewed) fundamental. *)
let reseed (config : Config.t) context symbol (info : symbol_info) =
  let cids = Hashtbl.keys info.resting_orders in
  Hashtbl.clear info.resting_orders;
  don't_wait_for
    (Deferred.List.iter ~how:`Parallel cids ~f:(fun cid ->
       Deferred.ignore_m (Context.cancel context cid)));
  let skewed_fair, half_spread = quote_targets config context symbol info in
  info.quoted <- Some (skewed_fair, half_spread);
  place_ladder config context symbol ~skewed_fair ~half_spread
;;

let on_start (config : Config.t) context =
  Deferred.List.iter ~how:`Parallel config.symbols ~f:(fun symbol ->
    reseed config context symbol (get_info config symbol))
;;

let on_tick (_config : Config.t) _context = Deferred.unit

let on_event (config : Config.t) context (event : Exchange_event.t) =
  match event with
  | Fill event ->
    let info = get_info config event.symbol in
    let side, client_order_id =
      if Participant.( = )
           (Context.participant context)
           event.aggressor_participant
      then Side.sign event.aggressor_side, event.aggressor_client_order_id
      else
        ( Side.sign (Side.flip event.aggressor_side)
        , event.resting_client_order_id )
    in
    info.inventory <- info.inventory + (side * Size.to_int event.size);
    let remaining =
      Hashtbl.update_and_return
        info.resting_orders
        client_order_id
        ~f:(function
        | Some remaining -> remaining - Size.to_int event.size
        | None -> 0)
    in
    if remaining <= 0 then Hashtbl.remove info.resting_orders client_order_id;
    (* Re-quote around the new inventory, unless the market is dislocated --
       in which case stand aside and forget our quotes until it recovers. *)
    if market_ok config info
    then reseed config context event.symbol info
    else (
      info.quoted <- None;
      Deferred.unit)
  | Order_accept accepted ->
    let info = get_info config accepted.request.symbol in
    Hashtbl.set
      info.resting_orders
      ~key:accepted.request.client_order_id
      ~data:(Size.to_int accepted.request.size);
    Deferred.unit
  | Order_cancel request ->
    let info = get_info config request.symbol in
    Hashtbl.remove info.resting_orders request.client_order_id;
    Deferred.unit
  | Best_bid_offer_update { symbol; bbo } ->
    let info = get_info config symbol in
    info.last_spread_cents
    <- Option.map (Bbo.spread bbo) ~f:Price.to_int_cents;
    if not (market_ok config info)
    then (
      (* Wide spread or a side swept out -- likely a whale. Stand aside and
         forget our quotes so we re-enter once liquidity fills back in. *)
      info.quoted <- None;
      Deferred.unit)
    else (
      (* Keep our half-spread in line with the market so we stay competitive. *)
      let spread_cents = Option.value info.last_spread_cents ~default:0 in
      info.half_spread_cents
      <- Int.max config.min_half_spread_cents (spread_cents / 2);
      let target = quote_targets config context symbol info in
      match info.quoted with
      | Some current when [%compare.equal: int * int] current target ->
        (* Our resting ladder is already where it should be. *)
        Deferred.unit
      | _ -> reseed config context symbol info)
  | _ -> Deferred.unit
;;
