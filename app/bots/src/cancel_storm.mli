(** A cancel storm: a pathological bot that submits an order and immediately
    cancels it, over and over, to hammer the exchange's cancel path.

    Each tick it fires a burst of [cycles_per_tick] submit->cancel cycles.
    Every cycle allocates a *fresh* [client_order_id], submits one order,
    then cancels that same id -- so the exchange sees a relentless stream of
    submit / accept / cancel events (and, for the marketable fraction, fill /
    cancel-reject events), and its duplicate-id bookkeeping grows by one
    entry per cycle. The bot keeps no resting book and reacts to no events;
    all it does is churn.

    Unlike a {!Jsip_bots.Robyn_noise_trader}, whose orders are meant to
    trade, the storm's orders exist only to be cancelled. A configurable
    [pct_marketable] fraction are priced to cross the spread (so some fill
    before the cancel races them, exercising the fill and cancel-reject
    paths); the rest are priced to rest and are cancelled while resting.

    Implements {!Jsip_bot_runtime.Bot_runtime.Bot}. See
    [doc/exercises-part-3.md] Section 1 (the [Cancel_storm] bot). Wire copies
    into a scenario with {!Jsip_scenario_runner.Bot_spec} -- the pathology is
    louder in numbers, so a scenario typically launches several with distinct
    participant names and RNG seeds. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
module Context = Bot_runtime.Context

module Config : sig
  (** Tuning knobs for the storm, plus the client-order-id counter it bumps
      each cycle. The counter lives here because the
      {!Jsip_bot_runtime.Bot_runtime.Bot} callbacks only receive a [Config.t]
      and a [Context.t], and each instance needs its own monotonic id source.
      Build one with {!create_config}. *)
  type t [@@deriving sexp_of]
end

val name : string

(** No startup work: the storm keeps no resting ladder and begins churning on
    the first tick. *)
val on_start : Config.t -> Context.t -> unit Deferred.t

(** Fire a burst of [cycles_per_tick] submit->cancel cycles. Firing a *burst*
    rather than a single cycle is what makes this a storm rather than a
    trickle: the pressure per unit time is [cycles_per_tick / tick_interval].
    See {!create_config}. *)
val on_tick : Config.t -> Context.t -> unit Deferred.t

(** Ignored. The cancel storm tracks no book and reacts to no fills, cancels,
    or rejects -- it only submits and cancels. *)
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t

(** [create_config ~symbols ~cycles_per_tick ~size ~pct_marketable ~price_offset_cents]
    builds a config for a storm over [symbols] (each cycle picks one at
    random from the bot's seeded RNG, so scenarios stay reproducible).

    - [cycles_per_tick]: submit->cancel cycles fired per tick. The intensity
      knob; with the runtime's tick interval it sets the storm's rate.
    - [size]: shares per order (every order is this size).
    - [pct_marketable]: percent chance (0-100) that a given order is priced
      to cross the spread (and so may fill before its cancel arrives) rather
      than to rest. [0] = every order rests then is cancelled; [100] = every
      order is marketable.
    - [price_offset_cents]: cents past the fundamental to place each order --
      the aggressive direction for a marketable order, the passive direction
      for a resting one. To make marketable orders actually cross a market
      maker's quote, set this larger than that maker's half-spread. *)
val create_config
  :  symbols:Symbol.t list
  -> cycles_per_tick:int
  -> size:int
  -> pct_marketable:int
  -> price_offset_cents:int
  -> Config.t
