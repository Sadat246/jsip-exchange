open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

(* Ticks skipped after a submission before the bot will trade again. The
   default of zero disables the cooldown entirely. *)
let default_cooldown_ticks = 0

(* Entry orders take liquidity: momentum decays quickly, so by default the
   remainder of a partially-filled entry is cancelled rather than rested. *)
let default_entry_time_in_force : Time_in_force.t = Ioc

(* How far beyond the newest trade price an entry is priced by default, in
   cents, so the order is marketable against a book still quoting near that
   trade. *)
let default_aggression_offset_cents = 1

(* A fixed-capacity ring of the most recent trade prices. [next] is the slot
   the next push overwrites, so once the ring is full it always points at the
   oldest live price. *)
module Ring : sig
  type t [@@deriving sexp_of]

  val create : capacity:int -> t

  (** Record [price] as the newest entry, evicting the oldest when full. *)
  val push : t -> Price.t -> unit

  (** [(newest, oldest)] once [capacity] prices have been pushed; [None]
      until then. *)
  val newest_and_oldest : t -> (Price.t * Price.t) option
end = struct
  type t =
    { prices : Price.t array
    ; mutable next : int
    ; mutable length : int
    }
  [@@deriving sexp_of]

  let create ~capacity =
    { prices = Array.create ~len:capacity Price.zero; next = 0; length = 0 }
  ;;

  let capacity t = Array.length t.prices

  let push t price =
    t.prices.(t.next) <- price;
    t.next <- (t.next + 1) mod capacity t;
    t.length <- Int.min (capacity t) (t.length + 1)
  ;;

  let newest_and_oldest t =
    if t.length < capacity t
    then None
    else (
      let newest = t.prices.((t.next + capacity t - 1) mod capacity t) in
      let oldest = t.prices.(t.next) in
      Some (newest, oldest))
  ;;
end

module Config = struct
  type t =
    { symbol : Symbol.t (** The symbol the bot watches and trades. *)
    ; window_capacity : int
    (** How many recent trade prices the signal looks across. *)
    ; threshold_cents : int
    (** Minimum absolute signal before the bot submits an order. *)
    ; max_order_size : int (** Cap in shares on any single submission. *)
    ; max_position : int
    (** Cap in shares on the absolute filled position. *)
    ; cooldown_ticks : int
    (** Ticks skipped after a submission before trading again. *)
    ; entry_time_in_force : Time_in_force.t
    (** Time-in-force of every entry order. Carried opaquely so future order
        types work here without changing the bot. *)
    ; aggression_offset_cents : int
    (** How far beyond the newest trade an entry is priced. *)
    ; ring : Ring.t (** Sliding window of recent trade prices. *)
    ; mutable position : int
    (** Signed filled position in shares: positive long, negative short. *)
    ; mutable cooldown_remaining : int
    (** Ticks left to skip before the next entry is allowed. *)
    ; generator : Client_order_id.Generator.t
    (** Sequential, collision-free client order IDs for our orders. *)
    }
  [@@deriving sexp_of]

  let create_exn
    ?(cooldown_ticks = default_cooldown_ticks)
    ?(entry_time_in_force = default_entry_time_in_force)
    ?(aggression_offset_cents = default_aggression_offset_cents)
    ~symbol
    ~window_capacity
    ~threshold_cents
    ~max_order_size
    ~max_position
    ()
    =
    let check name value ~at_least =
      if value < at_least
      then
        raise_s
          [%message
            "Momentum_trader_hansel.Config.create_exn: parameter out of \
             range"
              (name : string)
              (value : int)
              (at_least : int)]
    in
    check "window_capacity" window_capacity ~at_least:2;
    check "threshold_cents" threshold_cents ~at_least:1;
    check "max_order_size" max_order_size ~at_least:1;
    check "max_position" max_position ~at_least:1;
    check "cooldown_ticks" cooldown_ticks ~at_least:0;
    check "aggression_offset_cents" aggression_offset_cents ~at_least:0;
    { symbol
    ; window_capacity
    ; threshold_cents
    ; max_order_size
    ; max_position
    ; cooldown_ticks
    ; entry_time_in_force
    ; aggression_offset_cents
    ; ring = Ring.create ~capacity:window_capacity
    ; position = 0
    ; cooldown_remaining = 0
    ; generator = Client_order_id.Generator.create ()
    }
  ;;
end

let name = "Momentum_Trader"

(* Build and send the bot's one kind of order: an entry in the signal's
   direction, priced [aggression_offset_cents] beyond the newest trade so it
   is marketable against a book still quoting near that trade. All order
   construction lives here, so a new [Time_in_force] or a smarter pricing
   rule is a one-place change. *)
let submit_entry (config : Config.t) context ~side ~size ~newest_price =
  let offset_cents = Side.sign side * config.aggression_offset_cents in
  let price = Price.(newest_price + of_int_cents offset_cents) in
  let request : Order.Request.t =
    { client_order_id = Client_order_id.Generator.next config.generator
    ; symbol = config.symbol
    ; participant = Context.participant context
    ; side
    ; price
    ; size = Size.of_int size
    ; time_in_force = config.entry_time_in_force
    }
  in
  match%map Context.submit context request with
  | Ok () -> ()
  | Error err ->
    [%log.error
      "momentum trader submit failed"
        (request : Order.Request.t)
        (err : Error.t)]
;;

(* Apply one of our fills to the signed position. A fill names the aggressor
   side, so the resting party traded the flipped side; self-trade prevention
   guarantees we are at most one of the two. *)
let apply_fill (config : Config.t) context (fill : Fill.t) =
  let me = Context.participant context in
  let apply side =
    config.position
    <- config.position + (Side.sign side * Size.to_int fill.size)
  in
  if Participant.equal fill.aggressor_participant me
  then apply fill.aggressor_side
  else if Participant.equal fill.resting_participant me
  then apply (Side.flip fill.aggressor_side)
;;

let on_start (_config : Config.t) _context = Deferred.unit

let on_tick (config : Config.t) context =
  if config.cooldown_remaining > 0
  then (
    config.cooldown_remaining <- config.cooldown_remaining - 1;
    Deferred.unit)
  else (
    match Ring.newest_and_oldest config.ring with
    | None -> Deferred.unit
    | Some (newest, oldest) ->
      let signal_cents =
        Price.to_int_cents newest - Price.to_int_cents oldest
      in
      if abs signal_cents < config.threshold_cents
      then Deferred.unit
      else (
        let side : Side.t = if signal_cents > 0 then Buy else Sell in
        (* One share per cent of signal, capped by the per-order limit and by
           however much room the position limit leaves on this side. *)
        let desired = Int.min config.max_order_size (abs signal_cents) in
        let room =
          config.max_position - (Side.sign side * config.position)
        in
        let size = Int.min desired room in
        if size <= 0
        then Deferred.unit
        else (
          (* Armed even if the submit errors: the attempt consumed this
             trigger. *)
          config.cooldown_remaining <- config.cooldown_ticks;
          submit_entry config context ~side ~size ~newest_price:newest)))
;;

let on_event (config : Config.t) context (event : Exchange_event.t) =
  (match event with
   | Trade_report { symbol; price; size = _ } ->
     if Symbol.equal symbol config.symbol then Ring.push config.ring price
   | Fill fill ->
     if Symbol.equal fill.symbol config.symbol
     then apply_fill config context fill
   | Order_accept _ | Order_cancel _ | Order_reject _ | Cancel_reject _
   | Best_bid_offer_update _ ->
     ());
  Deferred.unit
;;
