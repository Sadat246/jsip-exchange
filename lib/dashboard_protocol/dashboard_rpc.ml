open! Core
open! Async_rpc_kernel

let current_stats_rpc =
  Rpc.Rpc.create
    ~name:"dashboard-current-stats"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:[%bin_type_class: Exchange_stats.t option]
    ~include_in_error_count:Only_on_exn
;;
