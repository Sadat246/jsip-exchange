(** Browser dashboard for the JSIP exchange.

    Renders live diagnostic panes — process memory (rolling window), submit
    and cancel latency percentiles, and order-throughput/GC counters — by
    polling the dashboard server's {!Dashboard_rpc.current_stats_rpc}.

    Structured like {!Jsip_monitor}: {!Controller} is the pure, testable
    state machine; {!View} is the thin Bonsai layer.
    {!Bonsai_web.Start.start} is handed {!View.app} by the entry point in
    [bin/]. *)

module Controller = Jsip_dashboard_controller.Controller
module Styles = Styles
module View = View
