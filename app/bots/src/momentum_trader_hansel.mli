open! Core
open! Async
open Jsip_types

(** A momentum (trend-following) trader: it bets that a price that has been
    moving recently will keep moving the same way for a little while.

    The bot keeps a fixed-capacity sliding window of the most recent public
    [Trade_report] prices for one symbol, updated in {!on_event}. On each
    {!on_tick} it computes [signal = newest - oldest] across the window; once
    the window is full and the signal's magnitude reaches [threshold_cents],
    it submits an entry order in the signal's direction (buy when prices
    rose, sell when they fell), sized at one share per cent of signal and
    capped by [max_order_size]. See [doc/exercises-part-2.md], Exercise 6.

    The bot's own [Fill] events maintain a signed position, and entries are
    clamped so the filled position never exceeds [max_position] in either
    direction. Note that a resting entry (e.g. a [Day] time-in-force) is not
    counted against the limit until it fills.

    The bot reads [Trade_report] rather than [Fill] for its signal because
    the runtime only delivers [Fill] events involving this bot's own
    participant, while [Trade_report] is broadcast to every market-data
    subscriber — so its {!Jsip_scenario_runner.Bot_spec.t} must set
    [is_marketdata_consumer = true]. *)
module Config : sig
  type t [@@deriving sexp_of]

  (** Build a config with fresh strategy state (price window, position,
      cooldown, order-id generator), so two bots never share state. Raises if
      a numeric parameter is out of range.

      - [window_capacity]: number of recent trade prices the signal looks
        across; at least [2] (one price has no direction). A bigger window
        smooths the signal but reacts more slowly.
      - [threshold_cents]: minimum absolute signal before the bot trades;
        positive. Higher means the bot stays flat through small wiggles.
      - [max_order_size]: cap in shares on any single submission, so a very
        strong signal can't produce an unreasonably large order; positive.
      - [max_position]: cap in shares on the absolute filled position;
        positive.
      - [cooldown_ticks]: after submitting, skip this many subsequent ticks
        so one sustained move doesn't fire on every tick; non-negative.
        Default [0] (disabled).
      - [entry_time_in_force]: time-in-force of every entry order. Default
        [Ioc], which takes what liquidity is there and cancels the rest.
      - [aggression_offset_cents]: entries are priced this many cents beyond
        the newest trade (above it when buying, below when selling) so they
        are marketable against a book still quoting near that trade;
        non-negative. Default [1]. *)
  val create_exn
    :  ?cooldown_ticks:int
    -> ?entry_time_in_force:Time_in_force.t
    -> ?aggression_offset_cents:int
    -> symbol:Symbol_id.t
    -> window_capacity:int
    -> threshold_cents:int
    -> max_order_size:int
    -> max_position:int
    -> unit
    -> t
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config
