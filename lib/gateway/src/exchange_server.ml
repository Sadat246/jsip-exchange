open! Core
open! Async
open Jsip_types
open Jsip_order_book

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let participant t = Option.map t.session ~f:Session.participant
end

(* [received_at] is stamped in the RPC handler, before the request enters the
   queue, so latency measured against it includes queue wait. In-process
   only: no derivers, so no wire impact. *)
module Matching_engine_action = struct
  type t =
    | New_order of
        { participant : Participant.t
        ; request : Order.Request.t
        ; received_at : Time_ns.t
        }
    | Cancel_order of
        { participant : Participant.t
        ; client_order_id : Client_order_id.t
        ; received_at : Time_ns.t
        }
end

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : Matching_engine_action.t Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  ; logged_in_participants : Participant.Hash_set.t
  ; collector : Stats_collector.t
  ; stopped : unit Ivar.t (* filled on [close] to stop the stats timer *)
  }

let require_login (conn_state : Connection_state.t) =
  match conn_state.session with
  | Some session -> Ok session
  | None -> Or_error.error_string "not logged in"
;;

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_submit
  ~(request_writer : Matching_engine_action.t Pipe.Writer.t)
  ~participant
  request
  =
  let%map () =
    Pipe.write_if_open
      request_writer
      (New_order { participant; request; received_at = Time_ns.now () })
  in
  Ok ()
;;

let handle_cancel
  ~(request_writer : Matching_engine_action.t Pipe.Writer.t)
  ~participant
  client_order_id
  =
  let%map () =
    Pipe.write_if_open
      request_writer
      (Cancel_order
         { participant; client_order_id; received_at = Time_ns.now () })
  in
  Ok ()
;;

let start_matching_loop
  ~engine
  ~dispatcher
  ~collector
  (request_reader : Matching_engine_action.t Pipe.Reader.t)
  =
  let elapsed_since received_at =
    Time_ns.diff (Time_ns.now ()) received_at
  in
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(function
      | Cancel_order { participant; client_order_id; received_at } ->
        let events =
          Matching_engine.cancel engine ~participant ~client_order_id
        in
        Stats_collector.record_cancel collector (elapsed_since received_at);
        Dispatcher.dispatch dispatcher events
      | New_order { participant; request; received_at } ->
        let events = Matching_engine.submit engine ~participant request in
        Stats_collector.record_submit collector (elapsed_since received_at);
        Dispatcher.dispatch dispatcher events))
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let collector = Stats_collector.create () in
  let stopped = Ivar.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher ~collector request_reader;
  Clock_ns.every
    ~stop:(Ivar.read stopped)
    (Time_ns.Span.of_sec 1.)
    (fun () ->
       let submit_latency, cancel_latency = Stats_collector.take collector in
       let gc = Gc.stat () in
       Dispatcher.push_stats
         dispatcher
         { live_words = gc.live_words
         ; heap_words = gc.heap_words
         ; top_heap_words = gc.top_heap_words
         ; minor_words = gc.minor_words
         ; promoted_words = gc.promoted_words
         ; minor_collections = gc.minor_collections
         ; major_collections = gc.major_collections
         ; submit_latency
         ; cancel_latency
         });
  let logged_in_participants = Participant.Hash_set.create () in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement Rpc_protocol.login_rpc (fun conn_state name ->
            if String.is_empty (String.strip name)
            then
              Deferred.Or_error.error_string "login name must not be empty"
            else (
              let participant = Participant.of_string name in
              match Connection_state.participant conn_state with
              | Some existing ->
                Deferred.Or_error.error_s
                  [%message "already logged in" (existing : Participant.t)]
              | None ->
                if Hash_set.mem logged_in_participants participant
                then
                  Deferred.Or_error.error_s
                    [%message
                      "participant name already taken"
                        (participant : Participant.t)]
                else (
                  let session =
                    Dispatcher.set_up_session dispatcher participant
                  in
                  conn_state.session <- Some session;
                  Hash_set.add logged_in_participants participant;
                  Deferred.Or_error.return participant)))
        ; Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun conn_state request ->
               match require_login conn_state with
               | Error _ as err -> return err
               | Ok session ->
                 handle_submit
                   ~request_writer
                   ~participant:(Session.participant session)
                   request)
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun conn_state client_order_id ->
               match require_login conn_state with
               | Error _ as err -> return err
               | Ok session ->
                 handle_cancel
                   ~request_writer
                   ~participant:(Session.participant session)
                   client_order_id)
        ; Rpc.Rpc.implement'
            Rpc_protocol.book_query_rpc
            (fun _conn_state symbol ->
               Matching_engine.book engine symbol
               |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun conn_state symbols ->
               ignore conn_state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.audit_log_rpc
            (fun conn_state () ->
               ignore conn_state;
               let reader = Dispatcher.subscribe_audit dispatcher in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.exchange_stats_rpc
            (fun conn_state () ->
               ignore conn_state;
               let reader = Dispatcher.subscribe_stats dispatcher in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun conn_state () ->
               match require_login conn_state with
               | Error _ as err -> return err
               | Ok session ->
                 Deferred.Or_error.return (Session.reader session))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr conn ->
        let (state : Connection_state.t) = { session = None } in
        don't_wait_for
          (let%map () = Rpc.Connection.close_finished conn in
           match state.session with
           | None -> ()
           | Some session ->
             Dispatcher.clean_up_session dispatcher session;
             Hash_set.remove
               logged_in_participants
               (Session.participant session));
        state)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine
  ; dispatcher
  ; request_writer
  ; tcp_server
  ; port = actual_port
  ; logged_in_participants
  ; collector
  ; stopped
  }
;;

let port t = t.port

let close t =
  Ivar.fill_if_empty t.stopped ();
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
