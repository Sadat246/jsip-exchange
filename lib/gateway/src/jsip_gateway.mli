(** Gateway layer for the JSIP exchange.

    Provides RPC definitions for client-server communication, the exchange
    server that bundles the matching engine with network handling, and the
    [Dispatcher] that routes matching-engine events to the right subscribers
    (per-participant session feeds, per-symbol market data, audit firehose). *)

module Exchange_stats = Exchange_stats
module Protocol = Protocol
module Rpc_protocol = Rpc_protocol
module Stats_collector = Stats_collector
module Session = Session
module Dispatcher = Dispatcher
module Exchange_command = Exchange_command
module Exchange_server = Exchange_server
