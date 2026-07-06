open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
module Context = Bot_runtime.Context

(* This market maker bot implements dynamicism by reacting to events and not
   through any tick functions. Every tick, if print_books is true, then the
   internal books are printed. Internal books are tracked through a table
   keyed by symbol with symbol_state values. Unified across alll symbols is
   the inventory skew per share, globally unique client order id, # of shares
   per level, and the number of lvls. *)

type symbol_state =
  { mutable asks : Client_order_id.t Hash_set.t
  ; mutable bids : Client_order_id.t Hash_set.t
  ; inventory : Int.t Ref.t
  ; mutable fair_value_cents : int
  ; bbo : Bbo.t
  }
[@@deriving sexp_of]

module Config = struct
  type t =
    { size_per_level : int
    ; num_levels : int
    ; inventory_skew_cents_per_share : int
    ; state : symbol_state Symbol.Table.t
    ; client_order_id_ref : Int.t Ref.t
    ; print_books : Bool.t
    }
  [@@deriving sexp_of]
end

let name = "Market Maker"

(* ...................Internal helper function start........................ *)
let update_all_fair_prices (config : Config.t) context =
  Hashtbl.iteri config.state ~f:(fun ~key:symbol ~data:symbol_state ->
    symbol_state.fair_value_cents
    <- Price.to_int_cents (Bot_runtime.Context.fundamental context symbol))
;;

let blank_symbol_state () =
  { inventory = ref 0
  ; asks = Hash_set.create (module Client_order_id)
  ; bids = Hash_set.create (module Client_order_id)
  ; fair_value_cents = 0
  ; bbo = Bbo.empty
  }
;;

let priced_symbol_state
  (_config : Config.t)
  (context : Bot_runtime.Context.t)
  symbol
  =
  let new_state = blank_symbol_state () in
  new_state.fair_value_cents
  <- Price.to_int_cents (Bot_runtime.Context.fundamental context symbol);
  new_state
;;

let get_symbol_state
  (config : Config.t)
  (context : Bot_runtime.Context.t)
  symbol
  =
  Hashtbl.find_or_add config.state symbol ~default:(fun () ->
    priced_symbol_state config context symbol)
;;

let set_symbol_state (config : Config.t) symbol state =
  Hashtbl.update config.state symbol ~f:(fun _state_opt -> state)
;;

let set_symbol_bids (config : Config.t) context symbol new_bids =
  let symbol_state = get_symbol_state config context symbol in
  symbol_state.bids <- new_bids
;;

let set_symbol_asks (config : Config.t) context symbol new_asks =
  let symbol_state = get_symbol_state config context symbol in
  symbol_state.asks <- new_asks
;;

let set_symbol_inventory (config : Config.t) context symbol new_inventory =
  let { asks = _; bids = _; inventory; fair_value_cents = _; bbo = _ } =
    get_symbol_state config context symbol
  in
  inventory := new_inventory
;;

let new_client_order_id (config : Config.t) =
  config.client_order_id_ref := !(config.client_order_id_ref) + 1;
  !(config.client_order_id_ref)
;;

(* half_spread will default to 50 cents if no BBO exists. *)
let half_spread_cents (bbo : Bbo.t) =
  match Bbo.spread bbo with
  | Some spread ->
    let spread = Price.to_int_cents spread in
    spread / 2
  | None -> 50
;;

let skewed_fair_value (config : Config.t) fair_value_cents inventory =
  fair_value_cents - (!inventory * config.inventory_skew_cents_per_share)
;;

(* ....................................................... *)

(* This function dummy sets fair value for ecah symbol at 0 bc on_start will
   be called to make it nice and pretty yay *)
let create_config
  ?(testing = false)
  ()
  ~size_per_level
  ~num_levels
  ~inventory_skew_cents_per_share
  ~symbols
  : Config.t
  =
  let symbol_state_list =
    List.map symbols ~f:(fun symbol -> symbol, blank_symbol_state ())
  in
  { size_per_level
  ; num_levels
  ; inventory_skew_cents_per_share
  ; state = Hashtbl.of_alist_exn (module Symbol) symbol_state_list
  ; client_order_id_ref = ref 0
  ; print_books = testing
  }
;;

let seed_book
  (config : Config.t)
  (context : Bot_runtime.Context.t)
  (symbols : Symbol.t List.t)
  =
  (* XCR claude for robyn: this binds the list of submit deferreds to [_] and
     then returns [return ()], so [seed_book] does NOT await the submits —
     [on_start] completes before any order is actually sent. Use
     [Deferred.List.iter ~how:`Parallel symbols ~f:...] (or
     [Deferred.all_unit (List.map ...)]) so the returned deferred reflects
     the real work.

     claude: verified — now
     [let%bind () = Deferred.List.iter ~how:(`Max_concurrent_jobs 64) symbols ...]
     awaits every submit before returning. Minor: the inner "TODO: Think
     about making this a parallel map" comment is now stale (you did it) —
     drop it. *)
  let%bind () =
    (* TODO: Think about making this a parallel map *)
    Deferred.List.iter
      ~how:(`Max_concurrent_jobs 64)
      symbols
      ~f:(fun symbol ->
        let { asks; bids; inventory; fair_value_cents; bbo } =
          get_symbol_state config context symbol
        in
        Deferred.List.iter
          ~how:`Parallel
          (List.init config.num_levels ~f:Fn.id)
          ~f:(fun level ->
            let offset = half_spread_cents bbo + level in
            let skewed_fair_value_cents =
              skewed_fair_value config fair_value_cents inventory
            in
            let buy_client_order_id =
              new_client_order_id config |> Client_order_id.of_int
            in
            let sell_client_order_id =
              new_client_order_id config |> Client_order_id.of_int
            in
            let%bind buy_result =
              Bot_runtime.Context.submit
                context
                ({ symbol
                 ; participant = Context.participant context
                 ; side = Buy
                 ; price =
                     Price.of_int_cents (skewed_fair_value_cents - offset)
                 ; size = Size.of_int config.size_per_level
                 ; time_in_force = Day
                 ; client_order_id = buy_client_order_id
                 }
                 : Order.Request.t)
            and sell_result =
              Bot_runtime.Context.submit
                context
                ({ symbol
                 ; participant = Context.participant context
                 ; side = Sell
                 ; price =
                     Price.of_int_cents (skewed_fair_value_cents + offset)
                 ; size = Size.of_int config.size_per_level
                 ; time_in_force = Day
                 ; client_order_id = sell_client_order_id
                 }
                 : Order.Request.t)
            in
            (* Track resting orders at submit time — the moment they go onto
               the wire — not at [Order_accept]. This keeps the local books
               in sync with what the exchange holds, so [cancel_all_orders]
               can always see (and cancel) a freshly-seeded ladder rather
               than orphaning it while the accepts are still in flight. A
               rejected order never rests, so it is pulled back out in the
               [Order_reject] arm. *)
            (match buy_result with
             | Ok _ok -> Hash_set.add bids buy_client_order_id
             | Error err -> [%log.error "Buy failed: " (err : Error.t)]);
            match sell_result with
            | Ok _ok ->
              Hash_set.add asks sell_client_order_id;
              return ()
            | Error err ->
              [%log.error "Sell failed: " (err : Error.t)];
              return ()))
  in
  return ()
;;

let on_start config context =
  update_all_fair_prices config context;
  seed_book config context (Hashtbl.keys config.state)
;;

(* I dont think this will actually do anything for MM bc we react to events *)
let on_tick (config : Config.t) context =
  let print_books () =
    Hashtbl.iteri
      config.state
      ~f:
        (fun
          ~key:symbol
          ~data:{ asks; bids; inventory; fair_value_cents; bbo }
        ->
        print_endline
          [%string "\nSTART for %{symbol#Symbol}===================="];
        print_endline [%string "Fair value price: %{fair_value_cents#Int}"];
        print_endline [%string "BBO: %{bbo#Bbo}"];
        print_endline [%string "Inventory: %{!(inventory)#Int}\n"];
        print_string [%string "\nBIDS: "];
        Hash_set.iter bids ~f:(fun client_order_id ->
          print_string
            [%string "%{(Client_order_id.to_int client_order_id)#Int}, "]);
        print_string [%string "\nASKS: "];
        Hash_set.iter asks ~f:(fun client_order_id ->
          print_string
            [%string "%{(Client_order_id.to_int client_order_id)#Int}, "]);
        print_endline [%string "\nEND ===================="])
  in
  update_all_fair_prices config context;
  if config.print_books then print_books () else ();
  return ()
;;

let on_event config context event =
  let participant = Context.participant context in
  (* Our side of a fill: if we crossed the spread we were the aggressor and
     keep the aggressor's side; otherwise we were resting and take the
     opposite side. (The client-order-id no longer matters — a fill re-quotes
     the whole book — so we only need the side.) *)
  let side_of_fill (fill : Fill.t) =
    if Participant.equal fill.aggressor_participant participant
    then fill.aggressor_side
    else Side.flip fill.aggressor_side
  in
  (* On a fill we only move inventory here. We deliberately do NOT drop the
     filled order id from the books: on a *partial* fill the un-filled
     remainder is still resting on the exchange, so forgetting it here would
     orphan it (never cancelled, never re-quoted). Instead
     [cancel_all_orders] below sweeps *both entire books* and re-seeds, so
     the just-filled order is cancelled there — a partial remainder gets
     pulled, and a fully-filled order simply yields a [Cancel_reject], which
     we already ignore.

     Inventory accumulates signed [fill.size] (cents-per-*share* skew), not a
     +/-1 order count. *)
  let update_books side symbol size : unit =
    let { asks = _; bids = _; inventory; fair_value_cents = _; bbo = _ } =
      get_symbol_state config context symbol
    in
    set_symbol_inventory
      config
      context
      symbol
      (!inventory + match (side : Side.t) with Buy -> size | Sell -> -size)
  in
  (* TODO keep track of Cancel_reject. would mess up our books *)
  (* Cancels every resting order on both sides for [symbol] and clears the
     local books. We cancel both sides (not just the filled one) because a
     fill moves inventory, which shifts the skewed fair value, so quotes
     on *both* sides are stale and get re-seeded together — see the [Fill]
     arm. *)
  let cancel_all_orders symbol =
    let { asks; bids; inventory = _; fair_value_cents = _; bbo = _ } =
      get_symbol_state config context symbol
    in
    let cancel_ids ids =
      Deferred.List.iter
        ~how:`Sequential
        (Hash_set.to_list ids)
        ~f:(fun id -> Context.cancel context id >>| (ignore : _ -> unit))
    in
    let%bind () = cancel_ids bids in
    let%bind () = cancel_ids asks in
    set_symbol_bids
      config
      context
      symbol
      (Hash_set.create (module Client_order_id) ~size:config.num_levels);
    set_symbol_asks
      config
      context
      symbol
      (Hash_set.create (module Client_order_id) ~size:config.num_levels);
    return ()
  in
  (* This is where we actually match and handle all events *)
  let%bind () =
    match (event : Exchange_event.t) with
    | Order_accept _ ->
      (* Resting orders are tracked at submit time in [seed_book], so the
         acceptance confirmation needs no further bookkeeping. *)
      return ()
    | Best_bid_offer_update { symbol; bbo } ->
      (* Only track BBOs for symbols we actually quote. Market data is
         broadcast for every subscribed symbol, but [get_symbol_state] would
         [find_or_add] an unknown one and price it via the oracle — which
         raises for a symbol outside our config. So look up the existing
         state and ignore data for anything we don't trade. *)
      (match Hashtbl.find config.state symbol with
       | None -> ()
       | Some curr_symbol_state ->
         set_symbol_state config symbol { curr_symbol_state with bbo });
      return ()
      (* TODO: maybe adjust some of the config stuff based on BBO *)
    | Order_cancel _ ->
      return ()
      (* XCR claude for robyn: on a fill you [cancel_all_orders side] (one
         side) but [seed_book] re-places a *full two-sided* ladder — so the
         un-cancelled side keeps its old resting orders AND gets a fresh set
         stacked on top, doubling the opposite book over repeated fills.

         claude: verified — [cancel_all_orders] no longer takes a [side] and
         now cancels *both* books before [seed_book] re-seeds them, so
         nothing is left resting to stack on top of. *)
    | Fill fill ->
      let side = side_of_fill fill in
      update_books side fill.symbol (Size.to_int fill.size);
      let%bind () = cancel_all_orders fill.symbol in
      seed_book config context [ fill.symbol ]
    | Order_reject { request; reason = _; participant = _ } ->
      (* A rejected order never rests, so drop it from the book we
         optimistically added it to at submit time. *)
      let { asks; bids; inventory = _; fair_value_cents = _; bbo = _ } =
        get_symbol_state config context request.symbol
      in
      (match request.side with
       | Buy -> Hash_set.remove bids request.client_order_id
       | Sell -> Hash_set.remove asks request.client_order_id);
      return ()
    | Trade_report _ | Cancel_reject _ -> return ()
  in
  return ()
;;
