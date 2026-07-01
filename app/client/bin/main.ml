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
  let%bind login_result =
    Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc conn participant_name
  in
  let participant =
    match login_result with Error err -> Error.raise err | Ok p -> p
  in
  let%bind session_feed_result, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed_result ~f:(fun event ->
       match event with
       | Exchange_event.Fill fill ->
         let message =
           match Fill.to_participant_view fill participant with
           | None -> ""
           | Some msg -> msg
         in
         print_endline message
       | _ -> print_endline [%string "%{Protocol.format_event event}"]));
  print_endline
    [%string
      {|
Connected to exchange at %{host}:%{port#Int} as %{participant#Participant}
Commands: BUY|SELL <symbol> <size> <price> [IOC|DAY]
          BOOK <symbol>
          SUBSCRIBE <symbol>  (stream market data)

Order acknowledgements, fills, and cancellations are temporarily printed
by the server process; the SUBSCRIBE command attaches you to a per-symbol
market-data feed.|}];
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
      else if String.is_prefix line ~prefix:"BOOK"
      then (
        match String.chop_prefix line ~prefix:"BOOK " with
        | None ->
          print_endline "ERROR: expected BOOK <symbol>";
          loop ()
        | Some rest ->
          let symbol =
            Symbol.of_string (String.uppercase (String.strip rest))
          in
          let%bind result =
            Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc conn symbol
          in
          (match result with
           | None ->
             print_endline [%string "No book available for %{symbol#Symbol}"]
           | Some result -> print_endline (Book.to_string result));
          loop ())
      else if String.is_prefix line ~prefix:"SUBSCRIBE"
      then (
        match String.chop_prefix line ~prefix:"SUBSCRIBE " with
        | None ->
          print_endline "ERROR: expected SUBSCRIBE <symbol>";
          loop ()
        | Some rest ->
          let symbol =
            Symbol.of_string (String.uppercase (String.strip rest))
          in
          let%bind result =
            Rpc.Pipe_rpc.dispatch
              Rpc_protocol.market_data_rpc
              conn
              [ symbol ]
          in
          (match result with
           | Error err | Ok (Error err) ->
             print_endline
               [%string "ERROR subscribing: %{Error.to_string_hum err}"];
             loop ()
           | Ok (Ok (reader, _id)) ->
             print_endline
               [%string
                 {|
Subscribed to %{symbol#Symbol} market data. Updates will appear below.
Continue entering commands as normal.|}];
             (* Read market data in the background; the command loop
                continues running concurrently. *)
             don't_wait_for
               (Pipe.iter_without_pushback reader ~f:(fun event ->
                  print_endline
                    [%string "[MD] %{Protocol.format_event event}"]));
             loop ()))
      else (
        match
          Protocol.parse_command_with_default_participant
            line
            ~default:participant
        with
        | Error msg ->
          print_endline [%string "ERROR: %{msg}"];
          loop ()
        | Ok request ->
          let%bind.Deferred.Or_error () =
            Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
          in
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
