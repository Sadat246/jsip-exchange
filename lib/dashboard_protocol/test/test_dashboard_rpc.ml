open! Core
open! Async_rpc_kernel
open Jsip_dashboard_protocol

(* Pin the wire contract of the dashboard's poll RPC — its name, version, and
   the bin-shape digests of its query and response — the same way
   [lib/gateway/test/test_rpc_shapes.ml] pins every exchange RPC. If the
   digest moves, [Exchange_stats.t]'s serialized layout changed; read the
   diff and confirm it was intended before promoting. *)

let%expect_test "dashboard-current-stats RPC" =
  print_s
    [%sexp (Rpc.Rpc.shapes Dashboard_rpc.current_stats_rpc : Rpc_shapes.t)];
  [%expect
    {|
    (Rpc (query 86ba5df747eec837f0b391dd49f33f9e)
     (response cf07d714d38e1a0f441dac0a6fdb25b8))
    |}]
;;
