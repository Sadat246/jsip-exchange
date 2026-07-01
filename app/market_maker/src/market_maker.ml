open! Core
open! Async
open Jsip_types
open Jsip_gateway

module Config = struct
  module T = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; inventory_by_symbol : Symbol.Table.t
    ; accepted_order_ids : list
    }
  [@@deriving sexp_of]
  end 
include T 
include Hashable.Make(T)
end

let seed_book (config : Config.t) conn =
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
    in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  Deferred.List.iteri
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun i level ->
      let offset = config.half_spread_cents + level in
      let%bind () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Buy
           ; price = Price.of_int_cents (config.fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; client_order_id = Client_order_id.of_int (2 * i)
           ; time_in_force = Day
           }
           : Order.Request.t)
      and () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; client_order_id = Client_order_id.of_int ((2 * i) + 1)
           ; time_in_force = Day
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

let run (config : Config.t) conn =
  let inventory_by_symbol = Symbol.Table.create () in
  let update_inventory symbol delta =
    let current =
      Hashtbl.find inventory_by_symbol symbol |> Option.value ~default:0
    in
    Hashtbl.set inventory_by_symbol ~key:symbol ~data:(current + delta)
  in
  let%bind () = seed_book config conn in
  let%map session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  Pipe.iter_without_pushback
    session_feed
    ~f:(fun event ->
      match event with
      | Fill fill ->
        let size = Size.to_int fill.size in
        if Participant.equal fill.aggressor_participant config.participant
        then (
          match fill.aggressor_side with
          | Buy -> update_inventory fill.symbol size
          | Sell -> update_inventory fill.symbol (-1 * size))
        else if Participant.equal fill.resting_participant config.participant
        then (
          match fill.aggressor_side with
          | Buy -> update_inventory fill.symbol (-1 * size)
          | Sell ->
            update_inventory fill.symbol size;
            print_endline (Protocol.format_event event))
      | Order_accept request -> 
      | _ -> print_endline (Protocol.format_event event))
    Deferred.never
    ()
;;

let run (config : Config.t) conn =
  let update_inventory symbol delta =
    let current =
      Hashtbl.find Config.inventory_by_symbol symbol |> Option.value ~default:0
    in
    Hashtbl.set Config.inventory_by_symbol ~key:symbol ~data:(current + delta)
  in
  let%bind () = seed_book config conn in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  Pipe.iter_without_pushback session_feed ~f:(fun event ->
    match event with
    | Fill fill ->
      let size = Size.to_int fill.size in
      if Participant.equal fill.aggressor_participant config.participant
      then
        if Side.equal fill.aggressor_side Buy
        then update_inventory fill.symbol size
        else update_inventory fill.symbol (-size)
      else if Participant.equal fill.resting_participant config.participant
      then
        if Side.equal fill.aggressor_side Buy
        then update_inventory fill.symbol (-size)
        else update_inventory fill.symbol size;
      print_endline (Protocol.format_event event)
    | Order_accept request -> Config.accepted_order_ids = Config.accepted_order_ids @ [request.order_id]
    | _ -> print_endline (Protocol.format_event event))
;;
