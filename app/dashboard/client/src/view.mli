(** The Bonsai layer: polls the dashboard server for the latest
    {!Exchange_stats.t}, folds each snapshot into a {!Controller.t} state
    machine, and renders the diagnostic panes.

    Analogous to {!Jsip_monitor.Term_app}, but rendering [Vdom] for the
    browser instead of terminal cells. All the interesting logic lives in the
    pure {!Controller}; this module only wires it to [Rpc_effect] and
    [ppx_html]. *)

open! Core
open! Bonsai_web

(** The whole dashboard. Passed to [Bonsai_web.Start.start] by the entry
    point. Connects back (via [Rpc_effect]'s default [self]) to the server
    that served the page. *)
val app : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
