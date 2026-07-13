(** Exchange client.

    Connects to a running exchange server and provides an interactive
    command-line interface for submitting orders and querying the book.

    Run with: dune exec app/client/bin/main.exe -- -host localhost -port
    12345 -name Alice *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway

let run_client ~host ~port ~participant_name =
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host; port }
  in
  let%bind conn = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
  let%bind participant =
    Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc conn participant_name
    >>| ok_exn
  in
  (* Fetch the name<->id directory once. The wire carries [Symbol_id.t]s, so
     the client needs it both ways: [name -> id] to turn the "AAPL" the user
     types into the id the server wants, and [id -> name] to render events
     back as names. *)
  let%bind symbol_directory =
    Rpc.Rpc.dispatch_exn Rpc_protocol.symbol_directory_rpc conn ()
  in
  let name_to_id = Symbol.Map.of_alist_exn symbol_directory in
  let id_to_name =
    List.map symbol_directory ~f:(fun (name, id) -> id, name)
    |> Symbol_id.Map.of_alist_exn
  in
  (* Resolve a user-typed token to an id: a symbol name, or (still) a raw id. *)
  let symbol_of_string s =
    match Map.find name_to_id (Symbol.of_string s) with
    | Some id -> Ok id
    | None ->
      (match Int.of_string_opt s with
       | Some n when n >= 0 -> Ok (Symbol_id.Private.of_int n)
       | _ -> Or_error.error_string [%string "unknown symbol: %{s}"])
  in
  (* Render an id as its name, falling back to the raw id if unknown. *)
  let symbol_to_string id =
    match Map.find id_to_name id with
    | Some name -> Symbol.to_string name
    | None -> Symbol_id.to_string id
  in
  let symbols_str =
    List.map symbol_directory ~f:(fun (name, id) ->
      [%string "%{name#Symbol}=%{id#Symbol_id}"])
    |> String.concat ~sep:", "
  in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       let event = Protocol.format_event ~symbol_to_string event in
       print_endline [%string "[%{participant#Participant}] %{event}"]));
  print_endline
    [%string
      {|
Connected to exchange at %{host}:%{port#Int} as %{participant#Participant}
Symbols (name=id): %{symbols_str}
Commands: BUY|SELL <client_id> <symbol> <size> <price> [%{Time_in_force.all_str}]
          CANCEL <client_id>
          BOOK <symbol>
          SUBSCRIBE <symbol>  (stream market data)

Order acknowledgements, fills, and cancellations are printed as they arrive;
the SUBSCRIBE command attaches you to a per-symbol market-data feed.|}];
  let rec loop () =
    print_string "> ";
    match%bind Reader.read_line (Lazy.force Reader.stdin) with
    | `Eof ->
      print_endline "\nDisconnected.";
      Deferred.Or_error.ok_unit
    | `Ok line ->
      let line = String.strip line in
      if String.is_empty line
      then loop ()
      else (
        match
          Jsip_gateway.Exchange_command.parse
            ~symbol_of_string
            ~participant
            line
        with
        | Ok (Book symbol) ->
          if Option.is_none (String.chop_prefix line ~prefix:"BOOK ")
          then print_endline [%string "(defaulting to %{symbol#Symbol_id})"];
          let%bind result =
            Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc conn symbol
          in
          (match result with
           | None ->
             print_endline
               [%string "No book available for %{symbol#Symbol_id}"]
           | Some result -> print_endline (Book.to_string result));
          loop ()
        | Ok (Subscribe symbol) ->
          if Option.is_none (String.chop_prefix line ~prefix:"SUBSCRIBE ")
          then print_endline [%string "(defaulting to %{symbol#Symbol_id})"];
          let%bind result =
            Rpc.Pipe_rpc.dispatch
              Rpc_protocol.market_data_rpc
              conn
              [ symbol ]
          in
          (match result with
           | Error err ->
             print_endline
               [%string "ERROR subscribing: %{Error.to_string_hum err}"];
             loop ()
           | Ok (Error err) ->
             print_endline
               [%string "ERROR subscribing: %{Error.to_string_hum err}"];
             loop ()
           | Ok (Ok (reader, _id)) ->
             print_endline
               [%string
                 {|
Subscribed to %{symbol#Symbol_id} market data. Updates will appear below.
Continue entering commands as normal.|}];
             (* Read market data in the background; the command loop
                continues running concurrently. *)
             don't_wait_for
               (Pipe.iter_without_pushback reader ~f:(fun event ->
                  let event =
                    Protocol.format_event ~symbol_to_string event
                  in
                  print_endline [%string "[MD] %{event}"]));
             loop ())
        | Ok (Login { name = _ }) ->
          print_endline
            [%string
              {|ERROR: this session is already logged in as %{participant#Participant}; \
LOGIN is only accepted once at connection start|}];
          loop ()
        | Ok (Submit request) ->
          let%bind.Deferred.Or_error () =
            Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
          in
          loop ()
        | Ok (Cancel { client_order_id }) ->
          let%bind.Deferred.Or_error () =
            Rpc.Rpc.dispatch_exn
              Rpc_protocol.cancel_order_rpc
              conn
              client_order_id
          in
          loop ()
        | Error msg ->
          print_endline [%string "ERROR: %{Error.to_string_hum msg}"];
          loop ())
  in
  loop ()
;;

let () =
  Command.async_or_error
    ~summary:"JSIP Exchange client"
    (let%map_open.Command host =
       flag
         "-host"
         (optional_with_default "localhost" string)
         ~doc:"HOST server hostname"
     and port = flag "-port" (required int) ~doc:"PORT server port"
     and participant_name =
       flag
         "-name"
         (optional_with_default (Core_unix.getlogin ()) string)
         ~doc:"NAME participant name"
     in
     fun () -> run_client ~host ~port ~participant_name)
  |> Command_unix.run
;;
