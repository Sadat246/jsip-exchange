open! Core
open! Async
open Jsip_types
open Jsip_order_book

type engine_request =
  | Submit of Order.Request.t
  | Cancel of
      { participant : Participant.t
      ; client_order_id : Client_order_id.t
      }

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : engine_request Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  }

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let _participant t = Option.map t.session ~f:Session.participant
end

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_submit
  ~request_writer
  (request : Order.Request.t)
  (state : Connection_state.t)
  =
  match state.session with
  | None -> return (Or_error.error_string "Not logged in")
  | Some session ->
    let new_request =
      { request with participant = Session.participant session }
    in
    let%map () = Pipe.write_if_open request_writer (Submit new_request) in
    Ok ()
;;

let handle_login dispatcher (state : Connection_state.t) name =
  if String.is_empty (String.strip name)
  then
    return (Or_error.error_string "Name cannot be empty or whitespace only")
    (* else if Option.is_some connection.Connection_state.session then return
       (Or_error.error_string "Connection already logged in") *)
  else (
    let participant = Participant.of_string name in
    let session = Session.create participant in
    match Dispatcher.register_session dispatcher participant session with
    | Error err -> return (Error err)
    | Ok _ ->
      state.session <- Some session;
      return (Ok participant))
;;

let handle_session_feed (state : Connection_state.t) =
  match state.session with
  | None -> Or_error.error_string "Not logged in"
  | Some session -> Ok (Session.reader session)
;;

let start_matching_loop ~engine ~dispatcher request_reader =
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(fun request ->
       let events =
         match request with
         | Submit order_request ->
           Matching_engine.submit engine order_request
         | Cancel { participant; client_order_id } ->
           Matching_engine.cancel engine ~participant ~client_order_id
       in
       Dispatcher.dispatch dispatcher events))
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher request_reader;
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun state request ->
               handle_submit ~request_writer request state)
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun state symbols ->
               ignore state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.audit_log_rpc (fun state () ->
            ignore state;
            let reader = Dispatcher.subscribe_audit dispatcher in
            return (Ok reader))
        ; Rpc.Rpc.implement Rpc_protocol.login_rpc (fun state name ->
            handle_login dispatcher state name)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun state () -> return (handle_session_feed state))
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun state client_order_id ->
               match Connection_state._participant state with
               | None -> return (Or_error.error_string "not logged in")
               | Some participant ->
                 let%bind () =
                   Pipe.write
                     request_writer
                     (Cancel { participant; client_order_id })
                 in
                 return (Ok ()))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr _conn ->
        { Connection_state.session = None })
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine; dispatcher; request_writer; tcp_server; port = actual_port }
;;

let port t = t.port

let close t =
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
