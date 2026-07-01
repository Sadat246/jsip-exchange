open! Core
open! Async
open Jsip_types

type t =
  { market_data_subscribers_by_symbol :
      Exchange_event.t Pipe.Writer.t Bag.t Symbol.Table.t
  ; audit_subscribers : Exchange_event.t Pipe.Writer.t Bag.t
  ; participant_sessions : Session.t Participant.Table.t
  }

let create () =
  { market_data_subscribers_by_symbol = Symbol.Table.create ()
  ; audit_subscribers = Bag.create ()
  ; participant_sessions = Participant.Table.create ()
  }
;;

let subscribe_market_data t symbols =
  let reader, writer = Pipe.create () in
  (* Register the same writer in every requested symbol's bag. A per-symbol
     publish iterates a single bag, so a subscriber listed in multiple bags
     receives each event exactly once — only via whichever bag matches the
     event's symbol. *)
  let elts =
    List.map symbols ~f:(fun symbol ->
      let subscribers =
        Hashtbl.find_or_add
          t.market_data_subscribers_by_symbol
          ~default:Bag.create
          symbol
      in
      symbol, Bag.add subscribers writer)
  in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     List.iter elts ~f:(fun (symbol, elt) ->
       match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
       | None -> ()
       | Some subscribers -> Bag.remove subscribers elt));
  reader
;;

let subscribe_audit t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.audit_subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.audit_subscribers elt);
  reader
;;

let push_market_data t event symbol =
  match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
  | None -> ()
  | Some subscribers ->
    Bag.iter subscribers ~f:(fun writer ->
      Pipe.write_without_pushback_if_open writer event)
;;

let push_audit t event =
  Bag.iter t.audit_subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer event)
;;

let push_to_session t participant event =
  (* TODO: Once sessions have been implemented this function should write the
     event to the appropriate session's pipe. For now we have the server
     binary print these events to stdout while tests can silence them. *)
  match Hashtbl.find t.participant_sessions participant with
  | None -> ()
  | Some session -> Session.push session event
;;

let dispatch_event t (event : Exchange_event.t) =
  push_audit t event;
  match event with
  | Best_bid_offer_update { symbol; bbo = _ } ->
    push_market_data t event symbol
  | Trade_report { symbol; price = _; size = _ } ->
    push_market_data t event symbol
  | Order_accept { order_id = _; request }
  | Order_reject { request; reason = _ } ->
    push_to_session t request.participant event
  | Order_cancel
      { order_id = _
      ; participant
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      ; client_order_id = _
      } ->
    push_to_session t participant event
  | Cancel_reject { participant; client_order_id = _; reason = _ } ->
    push_to_session t participant event
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size = _
      ; aggressor_order_id = _
      ; aggressor_participant
      ; aggressor_side = _
      ; resting_order_id = _
      ; resting_participant
      ; aggressor_client_order_id = _
      ; resting_client_order_id = _
      } ->
    push_to_session t aggressor_participant event;
    push_to_session t resting_participant event
;;

let dispatch t events = List.iter events ~f:(dispatch_event t)

let clean_up_session' t session =
  let participant = Session.participant session in
  Hashtbl.remove t.participant_sessions participant;
  Session.close session
;;

let clean_up_session t session =
  Deferred.return (clean_up_session' t session)
;;

let set_up_session t participant =
  Option.iter
    (Hashtbl.find t.participant_sessions participant)
    ~f:(fun session ->
      clean_up_session' t session;
      let session = Session.create participant in
      Hashtbl.set t.participant_sessions ~key:participant ~data:session);
  Deferred.unit
;;

let session_in_table t participant =
  Hashtbl.find t.participant_sessions participant
;;

let register_session t participant session =
  match session_in_table t participant with
  | None ->
    Hashtbl.set t.participant_sessions ~key:participant ~data:session;
    Ok ()
  | Some _session -> Or_error.error_string "Session already exists"
;;

(* let%bind () = match Hashtbl.find t.participant_sessions participant with |
   None -> () | Some session -> clean_up_session' t session in let session =
   Session.create participant in Hashtbl.set t.participant_sessions
   ~key:participant ~data:session; Deferred.unit *)

module For_testing = struct
  let audit_subscriber_count t = Bag.length t.audit_subscribers
end
