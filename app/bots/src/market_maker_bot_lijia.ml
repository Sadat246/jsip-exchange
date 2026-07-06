open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime

module Config = struct
  type t =
    { symbol : Symbol.t
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; next_client_order_id : int ref
        (* Quotes get a fresh client order id every tick, so we never reuse
           an id that a still-in-flight cancel hasn't cleared yet. *)
    ; resting : Client_order_id.t list ref
    (* The ids we posted last tick, so we can cancel them before re-quoting. *)
    }

  let create ~symbol ~half_spread_cents ~size_per_level ~num_levels =
    { symbol
    ; half_spread_cents
    ; size_per_level
    ; num_levels
    ; next_client_order_id = ref 1
    ; resting = ref []
    }
  ;;
end

let name = "market-maker"

(* Post a fresh two-sided ladder straddling the current fundamental, and
   return the client order ids used so the next tick can cancel them. *)
let place_ladder (config : Config.t) ctx =
  let fair_value_cents =
    Price.to_int_cents (Bot_runtime.Context.fundamental ctx config.symbol)
  in
  let fresh_id () =
    let id = !(config.next_client_order_id) in
    config.next_client_order_id := id + 1;
    Client_order_id.of_int id
  in
  let submit ~side ~price_cents =
    let client_order_id = fresh_id () in
    let request : Order.Request.t =
      { client_order_id
      ; symbol = config.symbol
      ; participant = Bot_runtime.Context.participant ctx
      ; side
      ; price = Price.of_int_cents price_cents
      ; size = Size.of_int config.size_per_level
      ; time_in_force = Day
      }
    in
    let%map result = Bot_runtime.Context.submit ctx request in
    (match result with
     | Ok () -> ()
     | Error error ->
       [%log.error "market_maker_bot: submit failed" (error : Error.t)]);
    client_order_id
  in
  Deferred.List.concat_map
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let%bind bid = submit ~side:Buy ~price_cents:(fair_value_cents - offset)
      and ask = submit ~side:Sell ~price_cents:(fair_value_cents + offset) in
      return [ bid; ask ])
;;

let cancel_all (config : Config.t) ctx =
  Deferred.List.iter ~how:`Parallel !(config.resting) ~f:(fun id ->
    let%map result = Bot_runtime.Context.cancel ctx id in
    match result with
    | Ok () -> ()
    | Error error ->
      [%log.error "market_maker_bot: cancel failed" (error : Error.t)])
;;

let on_start (config : Config.t) ctx =
  let%map ids = place_ladder config ctx in
  config.resting := ids
;;

let on_tick (config : Config.t) ctx =
  let%bind () = cancel_all config ctx in
  let%map ids = place_ladder config ctx in
  config.resting := ids
;;

(* The market maker is a liquidity provider, not a reactive strategy: it
   re-quotes on a clock rather than in response to individual events. *)
let on_event (_ : Config.t) _ctx (_ : Exchange_event.t) = return ()
