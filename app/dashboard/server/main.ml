(** [jsip-dashboard-server]: the bridge between a browser dashboard and the
    TCP-only exchange.

    A browser cannot open a raw TCP socket, so it cannot speak to the
    exchange's Async-RPC server directly. This binary sits in between. It:

    - connects to the exchange as an ordinary RPC client and drains
      {!Jsip_gateway.Rpc_protocol.exchange_stats_rpc} (a 1 Hz [Pipe_rpc]),
      keeping only the most recent {!Exchange_stats.t} in [latest];
    - serves HTTP on [-port]: the dashboard's [index.html] + [main.bc.js]
      bundle for plain GETs, and a WebSocket that carries Async-RPC;
    - over that WebSocket, implements {!Dashboard_rpc.current_stats_rpc}, a
      plain request/response RPC that returns [!latest]. The browser polls it
      (Bonsai has no [Rpc_effect.Pipe_rpc], so it cannot subscribe to the
      exchange pipe itself).

    [latest] is a single slot, not a queue: a slow browser can never build up
    a backlog that back-pressures the exchange feed. *)

open! Core
open! Async
open Jsip_dashboard_protocol

let connect_to_exchange ~host ~port =
  let%map result =
    Rpc.Connection.client
      (Tcp.Where_to_connect.of_host_and_port { host; port })
  in
  match result with
  | Ok conn -> conn
  | Error exn ->
    raise_s
      [%message
        "dashboard: failed to connect to exchange"
          (host : string)
          (port : int)
          (exn : Exn.t)]
;;

(* Drain the exchange's stats pipe into [latest] in the background. Each
   snapshot overwrites the previous one; we only ever serve the newest. *)
let subscribe_stats ~connection ~latest =
  match%map
    Rpc.Pipe_rpc.dispatch
      Jsip_gateway.Rpc_protocol.exchange_stats_rpc
      connection
      ()
  with
  | Error err | Ok (Error err) ->
    raise_s
      [%message
        "dashboard: exchange-stats subscription failed" (err : Error.t)]
  | Ok (Ok (pipe, _metadata)) ->
    don't_wait_for
      (Pipe.iter_without_pushback pipe ~f:(fun snapshot ->
         latest := Some snapshot))
;;

let content_type file =
  match snd (Filename.split_extension file) with
  | Some "html" -> "text/html; charset=utf-8"
  | Some "js" -> "text/javascript"
  | _ -> "application/octet-stream"
;;

(* Serve exactly two files from [static_dir], re-read per request so a fresh
   [dune build] shows up on browser refresh. The explicit whitelist keeps
   this from becoming a path-traversal hole. *)
let static_handler
  ~static_dir
  ~body:(_ : Cohttp_async.Body.t)
  (_ : Socket.Address.Inet.t)
  request
  =
  let path = Uri.path (Cohttp_async.Request.uri request) in
  let file =
    match path with
    | "" | "/" | "/index.html" -> Some "index.html"
    | "/main.bc.js" -> Some "main.bc.js"
    | _ -> None
  in
  match file with
  | None ->
    Cohttp_async.Server.respond_string ~status:`Not_found "not found\n"
  | Some file ->
    (match%bind
       Monitor.try_with (fun () -> Reader.file_contents (static_dir ^/ file))
     with
     | Ok contents ->
       let headers =
         Cohttp.Header.init_with "content-type" (content_type file)
       in
       Cohttp_async.Server.respond_string ~headers contents
     | Error _ ->
       Cohttp_async.Server.respond_string
         ~status:`Not_found
         [%string "missing %{file} under %{static_dir}\n"])
;;

let main ~exchange_host ~exchange_port ~web_port ~static_dir () =
  let latest : Exchange_stats.t option ref = ref None in
  let%bind connection =
    connect_to_exchange ~host:exchange_host ~port:exchange_port
  in
  let%bind () = subscribe_stats ~connection ~latest in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement' Dashboard_rpc.current_stats_rpc (fun () () ->
            !latest)
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%bind (_ : (_, _) Cohttp_async.Server.t) =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:(Tcp.Where_to_listen.of_port web_port)
      ~implementations
      ~initial_connection_state:
        (fun
          ()
          (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
          (_ : Socket.Address.Inet.t)
          (_ : Rpc.Connection.t)
        -> ())
      ~http_handler:(fun () -> static_handler ~static_dir)
      ()
  in
  printf
    "dashboard on http://localhost:%d  (exchange %s:%d, static %s)\n%!"
    web_port
    exchange_host
    exchange_port
    static_dir;
  Deferred.never ()
;;

let command =
  Command.async
    ~summary:
      "Bridge that serves the browser dashboard and proxies the exchange's \
       per-second stats feed over a WebSocket."
    (let%map_open.Command exchange_host =
       flag
         "-exchange-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST exchange server hostname (default localhost)"
     and exchange_port =
       flag
         "-exchange-port"
         (optional_with_default 12345 int)
         ~doc:"PORT exchange server port (default 12345)"
     and web_port =
       flag
         "-port"
         (optional_with_default 8080 int)
         ~doc:"PORT port to serve the dashboard on (default 8080)"
     and static_dir =
       flag
         "-static-dir"
         (required string)
         ~doc:"DIR directory holding index.html and main.bc.js"
     in
     fun () -> main ~exchange_host ~exchange_port ~web_port ~static_dir ())
    ~behave_nicely_in_pipeline:false
;;

let () = Command_unix.run command
