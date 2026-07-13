(** Text protocol for communicating with the exchange.

    This module defines how exchange events are formatted for display. On a
    production exchange, this would be a binary protocol like FIX for
    performance and interoperability. We use a simple human-readable text
    format for ease of debugging and interactive use. *)

open! Core
open Jsip_types

(** Format an exchange event as a single line of human-readable text. Shows
    both the server-assigned and client-chosen order IDs — an
    exchange-centric view suitable for operator logs and tests.

    [symbol_to_string] controls how a {!Jsip_types.Symbol_id} is rendered; it
    defaults to the raw integer id (all a pure type can print). The
    interactive client passes an id->name resolver (from the symbol
    directory) so events show symbol names instead. *)
val format_event
  :  ?symbol_to_string:(Symbol_id.t -> string)
  -> Exchange_event.t
  -> string

(** Format a list of events, one per line. *)
val format_events
  :  ?symbol_to_string:(Symbol_id.t -> string)
  -> Exchange_event.t list
  -> string
