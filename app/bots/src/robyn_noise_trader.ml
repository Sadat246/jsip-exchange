open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
module Context = Bot_runtime.Context

(* A noise trader stands in for the mass of real-world trading that has no
   view on price direction: index rebalancing, retail flow, a corporation
   liquidating an acquisition. Each tick it flips a coin on whether to trade
   at all, then picks a symbol, side, size, price, and time-in-force at
   random and fires a single order. It never cancels and never reacts to its
   own fills; its only bookkeeping is a per-symbol BBO cache (maintained in
   [on_event]) used to price marketable vs. resting orders. See
   [doc/exercises-part-2.md] Exercise 4. *)

module Config = struct
  type t =
    { symbol_state : Bbo.t Symbol_id.Table.t
        (* The symbols this bot trades. Each maps to the latest BBO seen for
           it (or [Bbo.empty] until the first [Best_bid_offer_update]
           arrives). *)
    ; avg_size : int
    ; tick_chance : float
    ; aggressiveness_pct : int
    ; ioc_pct : int
    ; client_order_id_ref : Int.t Ref.t
    }
  [@@deriving sexp_of]
end

let name = "Noise trader"

(* Orders quote within this many cents of their reference price -- "a few
   cents" past the opposite best (marketable) or away from our own best
   (resting). *)
let max_price_offset_cents = 5

(* Each order's size is drawn from [avg_size] +/- this percentage of it, so
   sizes vary but stay in a small band around the configured mean. *)
let size_jitter_pct = 25

(* ...................Internal helper functions start...................... *)

let next_client_order_id (config : Config.t) =
  incr config.client_order_id_ref;
  Client_order_id.of_int !(config.client_order_id_ref)
;;

let random_size (config : Config.t) rng =
  let jitter = config.avg_size * size_jitter_pct / 100 in
  let lo = Int.max 1 (config.avg_size - jitter) in
  let hi = Int.max lo (config.avg_size + jitter) in
  Bot_random.int_inclusive rng ~lo ~hi
;;

(* Choose a limit price for an order on [side] for [symbol].

   A marketable order crosses the spread: it references the opposite side's
   best and steps a few cents past it in our trading direction (a buy lifts
   the ask, a sell hits the bid), so it trades immediately. A resting order
   references our own side's best and steps a few cents the other way, so it
   sits in the book instead of crossing. When the reference price is missing
   (empty book), fall back to the oracle's fundamental. *)
let choose_price (config : Config.t) context rng ~symbol ~side ~is_marketable
  =
  let bbo =
    Hashtbl.find config.symbol_state symbol
    |> Option.value ~default:Bbo.empty
  in
  let reference_side = if is_marketable then Side.flip side else side in
  let reference_cents =
    match Bbo.price bbo reference_side with
    | Some price -> Price.to_int_cents price
    | None -> Price.to_int_cents (Context.fundamental context symbol)
  in
  let offset_cents =
    Bot_random.int_inclusive rng ~lo:1 ~hi:max_price_offset_cents
  in
  let direction =
    if is_marketable then Side.sign side else -Side.sign side
  in
  Price.of_int_cents (reference_cents + (direction * offset_cents))
;;

(* ....................................................................... *)

(* A noise trader keeps no resting ladder, so there is nothing to prime; it
   begins trading on the first tick. *)
let on_start (_config : Config.t) (_context : Context.t) = return ()

let on_tick (config : Config.t) (context : Context.t) =
  let rng = Context.random context in
  let sends_order =
    Bot_random.does_occur rng (Percent.of_mult config.tick_chance)
  in
  if not sends_order
  then return ()
  else (
    match Hashtbl.keys config.symbol_state with
    | [] -> return ()
    | symbols ->
      let symbol = Bot_random.uniform_exn rng symbols in
      let side = Bot_random.uniform_exn rng Side.all in
      let size = random_size config rng in
      let is_marketable =
        Bot_random.does_occur
          rng
          (Percent.of_percentage (Int.to_float config.aggressiveness_pct))
      in
      let price =
        choose_price config context rng ~symbol ~side ~is_marketable
      in
      let is_ioc =
        Bot_random.does_occur
          rng
          (Percent.of_percentage (Int.to_float config.ioc_pct))
      in
      let time_in_force = if is_ioc then Time_in_force.Ioc else Day in
      let request : Order.Request.t =
        { symbol
        ; participant = Context.participant context
        ; side
        ; price
        ; size = Size.of_int size
        ; time_in_force
        ; client_order_id = next_client_order_id config
        }
      in
      let%bind result = Context.submit context request in
      (match result with
       | Ok () -> ()
       | Error error ->
         [%log.error "Noise_trader submit failed" (error : Error.t)]);
      return ())
;;

(* The only event a noise trader cares about is the BBO: we cache the latest
   best bid/offer per symbol so [on_tick] can price relative to the current
   market. Our own accepts, fills, cancels, and public trade reports need no
   bookkeeping -- the noise trader never cancels or reacts to fills. *)
let on_event
  (config : Config.t)
  (_context : Context.t)
  (event : Exchange_event.t)
  =
  (match event with
   | Best_bid_offer_update { symbol; bbo } ->
     Hashtbl.set config.symbol_state ~key:symbol ~data:bbo
   | Order_accept _ | Order_cancel _ | Order_reject _ | Fill _
   | Trade_report _ | Cancel_reject _ ->
     ());
  return ()
;;

(* [create_config ~symbols ...] builds a config trading [symbols]; the BBO
   cache starts empty for each and fills in as market data arrives. *)
let create_config
  ~symbols
  ~avg_size
  ~tick_chance
  ~aggressiveness_pct
  ~ioc_pct
  : Config.t
  =
  let symbol_state = Hashtbl.create (module Symbol_id) in
  List.iter symbols ~f:(fun symbol ->
    Hashtbl.set symbol_state ~key:symbol ~data:Bbo.empty);
  { symbol_state
  ; avg_size
  ; tick_chance
  ; aggressiveness_pct
  ; ioc_pct
  ; client_order_id_ref = ref 0
  }
;;
