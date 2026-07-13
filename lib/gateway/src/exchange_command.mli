(** Text commands accepted by the interactive exchange client.

    Each command is a single line of text:
    {v
    LOGIN <participant>
    BUY  <client_id> <symbol> <size> <price> [<time_in_force>]
    SELL <client_id> <symbol> <size> <price> [<time_in_force>]
    CANCEL <client_id>
    BOOK <symbol>
    SUBSCRIBE <symbol>
    v}

    Examples:
    {v
    LOGIN Alice
    BUY 1 AAPL 100 150.25
    SELL 2 TSLA 50 200.00 IOC
    CANCEL 1
    BOOK AAPL
    SUBSCRIBE AAPL
    v}

    The [<client_id>] is chosen by the client and used to correlate
    acknowledgments, fills, and cancellations. Time-in-force defaults to DAY
    if omitted. A participant must log in once per connection before
    submitting or cancelling orders; order and cancel commands implicitly act
    on behalf of the logged-in participant. *)

open! Core
open Jsip_types

type t =
  | Login of { name : string }
  | Submit of Order.Request.t
  | Cancel of { client_order_id : Client_order_id.t }
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
[@@deriving to_string]

(** Parse a text command. [participant] is the caller's logged-in identity;
    it is stamped onto the [Order.Request.t] of a [Submit] (the server still
    re-checks it against the session, so it cannot be spoofed).

    [symbol_of_string] turns the symbol token into a {!Jsip_types.Symbol_id}.
    It defaults to reading the token as the integer id (["BOOK 0"]); a client
    that has fetched the symbol directory passes one backed by its name->id map
    so the user can type a name (["BOOK AAPL"]), which is why the examples
    above use names. Returns [Error] with a human-readable message if the input
    is malformed (including an unknown symbol name). *)
val parse
  :  ?symbol_of_string:(string -> Symbol_id.t Or_error.t)
  -> participant:Participant.t
  -> string
  -> t Or_error.t
