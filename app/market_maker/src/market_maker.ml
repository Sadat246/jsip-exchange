open! Core
open! Async
open Jsip_types
open Jsip_gateway

module Config = struct
  type t =
    { participant : Participant.t
    ; symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    }
  [@@deriving sexp_of]
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
           ; time_in_force = Day
           ; client_order_id = Client_order_id.of_int (2 * i)
           }
           : Order.Request.t)
      and () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = Client_order_id.of_int ((2 * i) + 1)
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

(* The signed change to our own inventory when [we] execute [size] shares on
   [side]. A buy grows a long (or shrinks a short); a sell does the reverse.
   This is the same sign convention that {!Jsip_pnl} uses to fold fills into a
   position. *)
let inventory_delta ~(side : Side.t) ~size =
  (* TODO(human): return the signed [int] delta. [size] is a [Size.t]; use
     [Size.to_int] to get its magnitude, then apply the sign implied by
     [side]. *)
  ignore (side, size);
  failwith "TODO: implement Market_maker.inventory_delta"
;;

(* Seed the book, then follow our own session feed, keeping a running
   inventory per symbol as fills arrive. We may be either the aggressor or the
   resting side of a fill; in both cases we execute at [fill.price], but the
   resting side trades the {e opposite} side from the aggressor. *)
let run (config : Config.t) conn =
  let inventory_by_symbol = Symbol.Table.create () in
  let update_inventory symbol ~delta =
    Hashtbl.update inventory_by_symbol symbol ~f:(fun current ->
      Option.value current ~default:0 + delta)
  in
  let%bind () = seed_book config conn in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  Pipe.iter_without_pushback session_feed ~f:(fun event ->
    (match event with
     | Fill fill ->
       let my_side =
         if Participant.equal fill.aggressor_participant config.participant
         then Some fill.aggressor_side
         else if Participant.equal
                   fill.resting_participant
                   config.participant
         then Some (Side.flip fill.aggressor_side)
         else None
       in
       Option.iter my_side ~f:(fun side ->
         update_inventory
           fill.symbol
           ~delta:(inventory_delta ~side ~size:fill.size))
     | _ -> ());
    print_endline (Protocol.format_event event))
;;
