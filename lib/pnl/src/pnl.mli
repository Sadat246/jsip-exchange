(** Per-participant profit-and-loss (P&L) tracking.

    A {!t} folds a stream of executions and public trade prints into a
    running view of every participant's book. For each
    [(participant, symbol)] pair it tracks:

    - {b inventory}: signed share count (positive = long, negative = short);
    - {b cost basis}: the total cost of the currently-open position, from
      which the average entry price is derived;
    - {b realized cash}: P&L locked in when a position is reduced or closed.

    Two flavors of P&L fall out of this state:

    - {b realized} P&L is cash from closed positions — when you sell shares
      you are long, the difference between the sale price and your average
      entry price is realized;
    - {b unrealized} P&L marks the open position to a reference price:
      [inventory * (reference_price - average_entry_price)].

    The reference price comes from public trade prints (see
    {!apply_trade_report}) and is shared by every participant holding that
    symbol — it is what the whole market last traded at.

    Example:
    {[
      let pnl =
        Pnl.empty
        |> Fn.flip Pnl.apply_fill alice_buys_100_aapl_at_150
        |> Fn.flip Pnl.apply_fill alice_sells_40_aapl_at_155
        |> fun t ->
        Pnl.apply_trade_report
          t
          ~symbol:aapl
          ~price:(Price.of_int_cents 15300)
      in
      Pnl.summary pnl alice
    ]}

    A {!Fill.t} names both counterparties, so {!apply_fill} updates the
    aggressor and the resting participant together. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** A tracker with no positions and no reference prices. *)
val empty : t

(** Fold one execution into the tracker. Both the aggressor and the resting
    participant have their inventory, cost basis, and realized cash updated —
    each from their own side of the trade. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference price used to mark open positions in [symbol] to
    market. A public trade print carries no participant, so this updates the
    single market-wide reference for [symbol]; it does not change anyone's
    inventory or realized cash.

    (The prompt for this exercise asked for
    [apply_trade_report : t -> Trade_report.t -> t], but there is no
    [Trade_report.t] in this codebase — [Trade_report] is a constructor of
    {!Exchange_event.t} carrying [{ symbol; price; size }]. P&L only needs
    the symbol and price, so we take those directly.) *)
val apply_trade_report : t -> symbol:Symbol_id.t -> price:Price.t -> t

(** A snapshot of one participant's P&L. *)
module Summary : sig
  (** One row of the breakdown: the participant's standing in a single
      symbol. [unrealized_cents] is [0] when there is no open position or no
      reference price has been seen yet. *)
  module Per_symbol : sig
    type t =
      { symbol : Symbol_id.t
      ; inventory : int
      ; average_entry_price : Price.t option
      ; reference_price : Price.t option
      ; realized_cents : int
      ; unrealized_cents : int
      }
    [@@deriving sexp_of, fields ~getters]
  end

  type t =
    { by_symbol : Per_symbol.t list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    }
  [@@deriving sexp_of, fields ~getters]

  (** A human-readable, dollar-formatted rendering — handy for expect tests. *)
  val to_string_hum : t -> string
end

(** Break down [participant]'s P&L per symbol, plus session totals. Symbols
    with no activity for [participant] are omitted; rows are ordered by
    symbol. *)
val summary : t -> Participant.t -> Summary.t
