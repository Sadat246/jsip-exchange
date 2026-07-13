open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
module Context = Bot_runtime.Context

(* A cancel storm stands in for a client stuck in a submit/cancel loop -- a
   buggy strategy retrying forever, or a rival deliberately flooding the
   cancel path. Each tick it fires a burst of [cycles_per_tick] cycles; every
   cycle submits one order under a fresh client-order-id and immediately
   cancels it. It holds no state beyond the id counter and ignores every
   event. See [doc/exercises-part-3.md] Section 1. *)

module Config = struct
  type t =
    { symbols : Symbol_id.t list
    ; cycles_per_tick : int
    ; size : int
    ; pct_marketable : int
    ; price_offset_cents : int
    ; client_order_id_ref : Int.t Ref.t
    }
  [@@deriving sexp_of]
end

let name = "Cancel Storm"

(* ...................Internal helper functions start...................... *)

(* The storm's whole correctness hinge: a *new* id every cycle. Reuse one and
   the exchange's duplicate-id check rejects every submit after the first. *)
let next_client_order_id (config : Config.t) =
  incr config.client_order_id_ref;
  Client_order_id.of_int !(config.client_order_id_ref)
;;

(* Choose a limit price [price_offset_cents] away from the fundamental. A
   marketable order steps in its trading direction (a buy above, a sell
   below) so it crosses; a resting order steps the other way so it sits. We
   price off the fundamental rather than a cached BBO because the storm does
   not care about precise fills -- an offset larger than the market maker's
   half-spread is enough to guarantee crossing. *)
let choose_price (config : Config.t) context ~symbol ~side ~is_marketable =
  let fundamental_cents =
    Price.to_int_cents (Context.fundamental context symbol)
  in
  let direction =
    if is_marketable then Side.sign side else -Side.sign side
  in
  Price.of_int_cents
    (fundamental_cents + (direction * config.price_offset_cents))
;;

(* Build one order to submit-then-cancel under [client_order_id]. Symbol,
   side, and marketable/resting are all drawn from the bot's seeded RNG. *)
let make_request (config : Config.t) context ~client_order_id
  : Order.Request.t
  =
  let rng = Context.random context in
  let symbol = Bot_random.uniform_exn rng config.symbols in
  let side = Bot_random.uniform_exn rng [ Side.Buy; Side.Sell ] in
  let marketable_chance =
    Percent.of_percentage (Float.of_int config.pct_marketable)
  in
  let is_marketable = Bot_random.does_occur rng marketable_chance in
  let price = choose_price config context ~symbol ~side ~is_marketable in
  { symbol
  ; participant = Context.participant context
  ; side
  ; price
  ; size = Size.of_int config.size
  ; time_in_force = Day
  ; client_order_id
  }
;;

(* One submit->cancel cycle: the unit of pressure the storm repeats. Allocate
   a fresh id, build an order under it, [Context.submit] it, await that, then
   [Context.cancel] the *same* id. Both calls return
   [unit Deferred.Or_error.t]; a storm fires and forgets, so we ignore the
   [Or_error] results. *)
let submit_then_cancel (config : Config.t) (context : Context.t)
  : unit Deferred.t
  =
  let client_order_id = next_client_order_id config in
  let (request : Order.Request.t) =
    make_request config context ~client_order_id
  in
  let%bind (_ : unit Or_error.t) = Context.submit context request in
  let%bind (_ : unit Or_error.t) = Context.cancel context client_order_id in
  return ()
;;

(* ....................................................................... *)

(* A cancel storm keeps no resting ladder, so there is nothing to prime; it
   begins churning on the first tick. *)
let on_start (_config : Config.t) (_context : Context.t) = return ()

(* Fire the burst sequentially so a single tick is one deterministic run of
   [cycles_per_tick] cycles; intensity comes from the count and the tick
   rate, not from within-tick concurrency. *)
let on_tick (config : Config.t) (context : Context.t) =
  Deferred.List.iter
    ~how:`Sequential
    (List.init config.cycles_per_tick ~f:Fn.id)
    ~f:(fun _ -> submit_then_cancel config context)
;;

let on_event
  (_config : Config.t)
  (_context : Context.t)
  (_event : Exchange_event.t)
  =
  return ()
;;

let create_config
  ~symbols
  ~cycles_per_tick
  ~size
  ~pct_marketable
  ~price_offset_cents
  : Config.t
  =
  { symbols
  ; cycles_per_tick
  ; size
  ; pct_marketable
  ; price_offset_cents
  ; client_order_id_ref = ref 0
  }
;;
