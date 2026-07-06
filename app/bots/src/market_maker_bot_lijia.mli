(** A minimal market-making bot, for use as scenario supporting cast.

    It continuously quotes a symmetric ladder of resting bids and asks around
    the oracle's current fundamental price. On every tick it cancels its
    previous quotes and re-posts a fresh ladder, so a drifting fundamental
    produces a steady stream of order-book changes — and therefore
    market-data ([Best_bid_offer_update]) events — for other participants to
    observe.

    This exists because the real {!Jsip_market_maker.Market_maker} is a bare
    [seed_book] function over a raw connection, not a {!Bot_runtime.Bot}, and
    the scenario runner can only start [Bot] modules. It is intentionally
    dumber than the real market maker: it does not track inventory or skew
    its quotes; it just keeps a fresh two-sided market alive. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime

module Config : sig
  type t

  (** Build a market-maker config. [half_spread_cents] is the gap from fair
      value to the innermost quote; [num_levels] quotes are posted on each
      side, one cent further out each, and every quote carries
      [size_per_level] shares. *)
  val create
    :  symbol:Symbol.t
    -> half_spread_cents:int
    -> size_per_level:int
    -> num_levels:int
    -> t
end

val name : string
val on_start : Config.t -> Bot_runtime.Context.t -> unit Deferred.t
val on_tick : Config.t -> Bot_runtime.Context.t -> unit Deferred.t

val on_event
  :  Config.t
  -> Bot_runtime.Context.t
  -> Exchange_event.t
  -> unit Deferred.t
