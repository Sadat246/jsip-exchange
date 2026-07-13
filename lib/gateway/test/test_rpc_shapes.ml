open! Core
open! Async
open Jsip_gateway

(* Wire-shape tests for the exchange's RPC protocol.

   Each test below pins one RPC's *wire contract*: its name, its version, and
   a digest of the [bin_io] shape of every type it puts on the wire — the
   query it receives, the response it sends, and (for streaming pipe RPCs)
   the error type.

   A bin-shape digest is a stable fingerprint of how a value is serialized.
   It changes whenever the serialized layout of a type changes: adding a
   field to [Order.Request.t], adding a variant to [Exchange_event.t], or
   pointing an RPC at a different type all move the digest. Two exchanges can
   only talk to each other over an RPC if they agree on its name, version,
   and these digests, so these tests are the precise statement of "what's on
   the wire."

   Keep them in sync as you build out the protocol: when you change or extend
   an RPC, update its test; when you add an RPC, add a test for it (copy one
   of the blocks below). If a digest changes, read the diff, convince
   yourself the change was intended, then accept it with [dune promote]. *)

let%expect_test "login RPC" =
  print_s
    [%sexp
      (Rpc.Rpc.shapes Rpc_protocol.login_rpc : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Rpc (query d9a8da25d5656b016fb4dbdc2e4197fb)
     (response a77b3b6e3753246ce7ec1f3467c939eb))
    |}];
  return ()
;;

let%expect_test "submit-order RPC" =
  print_s
    [%sexp
      (Rpc.Rpc.shapes Rpc_protocol.submit_order_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Rpc (query 395f9f3320bba7e0056f16f61cd8ae19)
     (response 27f76252e5181aab209cd62aa6e42268))
    |}];
  return ()
;;

let%expect_test "cancel-order RPC" =
  print_s
    [%sexp
      (Rpc.Rpc.shapes Rpc_protocol.cancel_order_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Rpc (query 698cfa4093fe5e51523842d37b92aeac)
     (response 27f76252e5181aab209cd62aa6e42268))
    |}];
  return ()
;;

let%expect_test "book-query RPC" =
  print_s
    [%sexp
      (Rpc.Rpc.shapes Rpc_protocol.book_query_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Rpc (query 698cfa4093fe5e51523842d37b92aeac)
     (response 69122cea75d1c08088ac96792b33f39f))
    |}];
  return ()
;;

let%expect_test "symbol-directory RPC" =
  print_s
    [%sexp
      (Rpc.Rpc.shapes Rpc_protocol.symbol_directory_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Rpc (query 86ba5df747eec837f0b391dd49f33f9e)
     (response 4c1e50c93b38c2ad0554cbd929bef3ac))
    |}];
  return ()
;;

let%expect_test "session-feed RPC" =
  print_s
    [%sexp
      (Rpc.Pipe_rpc.shapes Rpc_protocol.session_feed_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Streaming_rpc (query 86ba5df747eec837f0b391dd49f33f9e)
     (initial_response 86ba5df747eec837f0b391dd49f33f9e)
     (update_response 0a2c6ffec48f1e98fe90b70ebbcdc32d)
     (error 52966f4a49a77bfdff668e9cc61511b3))
    |}];
  return ()
;;

let%expect_test "market-data RPC" =
  print_s
    [%sexp
      (Rpc.Pipe_rpc.shapes Rpc_protocol.market_data_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Streaming_rpc (query 4cd553520709511864846bda25c448d0)
     (initial_response 86ba5df747eec837f0b391dd49f33f9e)
     (update_response 0a2c6ffec48f1e98fe90b70ebbcdc32d)
     (error 52966f4a49a77bfdff668e9cc61511b3))
    |}];
  return ()
;;

let%expect_test "audit-log RPC" =
  print_s
    [%sexp
      (Rpc.Pipe_rpc.shapes Rpc_protocol.audit_log_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Streaming_rpc (query 86ba5df747eec837f0b391dd49f33f9e)
     (initial_response 86ba5df747eec837f0b391dd49f33f9e)
     (update_response 0a2c6ffec48f1e98fe90b70ebbcdc32d)
     (error 52966f4a49a77bfdff668e9cc61511b3))
    |}];
  return ()
;;

let%expect_test "exchange-stats RPC" =
  print_s
    [%sexp
      (Rpc.Pipe_rpc.shapes Rpc_protocol.exchange_stats_rpc
       : Async_rpc_kernel.Rpc_shapes.t)];
  [%expect
    {|
    (Streaming_rpc (query 86ba5df747eec837f0b391dd49f33f9e)
     (initial_response 86ba5df747eec837f0b391dd49f33f9e)
     (update_response 8d4bf15d47f4bf6575fa94d90ebb5dd5)
     (error 52966f4a49a77bfdff668e9cc61511b3))
    |}];
  return ()
;;
