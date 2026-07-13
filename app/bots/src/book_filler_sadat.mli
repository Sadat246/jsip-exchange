(** A pathological bot that floods the order book with resting orders it
    never intends to trade.

    On every tick, {!on_tick} submits {!Config.orders_per_tick} [Day] orders
    priced far enough from the current fundamental that they cannot be
    marketable — buys well below fair value, sells well above it — so each
    one {e rests} on the book instead of filling. Because [Day] orders live
    until end of day, the book grows without bound: every tick adds new
    resting orders (and, with a non-zero {!Config.level_spacing_cents}, new
    distinct price levels). The bot reads no market data and reacts to no
    events; its only effect is to pile up state on the exchange.

    This stresses two things: the memory held by the order book (one entry
    per resting order, one map key per price level), and the latency of
    anything that walks the book — the matching engine's search for a
    counterparty and the per-symbol book snapshot, both of which get slower
    as the number of price levels grows.

    Intensity is set by {!Config.orders_per_tick} together with the bot's
    tick interval (supplied by the scenario via [Bot_spec], not this config):
    the same module produces a gentle trickle or an aggressive flood just by
    changing constants. *)

open! Core
open Jsip_types

module Config : sig
  type t =
    { symbols : Symbol_id.t list
    (** Books to flood. The bot spreads its per-tick orders across these
        symbols in round-robin. Must be non-empty. *)
    ; orders_per_tick : int
    (** How many resting orders to add each tick. This is the primary
        intensity knob; combined with the tick interval it sets the fill
        rate. Suggested: 5 for a gentle scenario, several hundred for an
        aggressive one. Must be positive. *)
    ; order_size : int
    (** Shares per order. The orders never trade, so this mainly affects the
        notional resting on the book, not fills. Suggested default: 1. Must
        be positive. *)
    ; price_offset_cents : int
    (** Minimum distance, in cents, from the current fundamental at which
        orders are placed (buys at [fundamental - offset] and below, sells at
        [fundamental + offset] and above). Must be large enough that orders
        stay non-marketable as the fundamental drifts, so the bot never
        accidentally trades. Suggested default: 500 (i.e. $5.00). *)
    ; level_spacing_cents : int
    (** Gap, in cents, between consecutive orders placed in the same tick. A
        positive value marches each successive order to a new, more distant
        price level, maximizing the number of distinct levels in the book
        (the attack on snapshot / match latency). A value of [0] stacks every
        order at the same price, instead growing the depth of a single level.
        Suggested default: 1. *)
    ; next_client_order_id : int ref
    (** Mutable counter for minting a fresh {!Client_order_id.t} per order.
        The exchange rejects duplicate client order ids, so the bot must
        never reuse one; this [ref] persists the next unused id across ticks
        (the [Bot] interface keeps no state of its own). Initialize to [0]. *)
    }
  [@@deriving sexp_of]
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config
