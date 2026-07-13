(** A noise trader: a stand-in for the mass of real-world, non-informed
    trading activity (index rebalancing, retail flow, a corporation
    liquidating an acquisition). It has no view on price direction -- each
    tick it randomly picks a symbol, side, size, price, and time-in-force and
    fires a single order. Together with a
    {!Jsip_market_maker.Market_maker_bot} it gives the matching engine a
    steady stream of activity for the other bots to observe and react to.

    Implements {!Jsip_bot_runtime.Bot_runtime.Bot}. See
    [doc/exercises-part-2.md] Exercise 4. Wire one into a scenario with
    {!Jsip_scenario_runner.Bot_spec}. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
module Context = Bot_runtime.Context

module Config : sig
  (** Tuning knobs for the noise trader. The BBO cache it maintains and its
      client-order-id counter live in here too, since the {!Bot_runtime.Bot}
      callbacks only receive a [Config.t] and a [Context.t]. Build one with
      {!create_config}. *)
  type t [@@deriving sexp_of]
end

val name : string
val on_start : Config.t -> Context.t -> unit Deferred.t

(** Once per tick, with probability [tick_chance], submit one random order
    (see {!create_config}). Prices are taken from the cached BBO for the
    chosen symbol, falling back to the oracle fundamental when the book is
    empty. *)
val on_tick : Config.t -> Context.t -> unit Deferred.t

(** Maintain the per-symbol BBO cache from [Best_bid_offer_update] events.
    All other events are ignored -- the noise trader never cancels or reacts
    to fills. *)
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t

(** [create_config ~symbols ~avg_size ~tick_chance ~aggressiveness_pct ~ioc_pct]
    builds a config trading [symbols] (its BBO cache starts empty for each).

    - [avg_size]: mean order size; each order draws a size in a small band
      around it.
    - [tick_chance]: probability in [[0., 1.]] that a given [on_tick] sends
      any order at all, so the bot can run on a fast clock yet trade
      sparsely.
    - [aggressiveness_pct]: percent chance (0-100) that an order is
      marketable (crosses the spread and trades now) rather than resting in
      the book.
    - [ioc_pct]: percent chance (0-100) that an order is [Ioc] rather than
      [Day]. *)
val create_config
  :  symbols:Symbol_id.t list
  -> avg_size:int
  -> tick_chance:float
  -> aggressiveness_pct:int
  -> ioc_pct:int
  -> Config.t
