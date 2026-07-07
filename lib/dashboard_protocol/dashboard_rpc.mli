(** The browser dashboard's poll RPC.

    [bonsai_web] has no [Rpc_effect.Pipe_rpc], so the dashboard cannot drink
    from {!Rpc_protocol.exchange_stats_rpc} (a [Pipe_rpc]) directly. Instead
    the dashboard web-server ([app/dashboard/server]) drains that pipe
    natively, keeps the most recent {!Exchange_stats.t}, and re-exposes it
    through this plain request/response RPC. The browser polls it (~2Hz) via
    [Rpc_effect.Rpc.poll] and folds the snapshots into its own rolling
    window.

    [None] means the server has not yet received a snapshot from the
    exchange; since the exchange emits at 1Hz, this clears within a second of
    startup. *)

open! Core
open! Async_rpc_kernel

val current_stats_rpc : (unit, Exchange_stats.t option) Rpc.Rpc.t
