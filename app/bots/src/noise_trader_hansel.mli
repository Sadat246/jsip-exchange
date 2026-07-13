open! Core
open! Async
open Jsip_types

(** A noise trader: a stand-in for the huge amount of real-world buying and
    selling that carries no view on where the price is headed (index funds
    rebalancing, retail traders, a corporation liquidating shares, ...).

    It picks a symbol, a side, a size, a price, and a time-in-force more or
    less at random and submits an order. It doesn't try to make money;
    together with {!Jsip_market_maker} it gives the matching engine something
    to do so the informed bots in later exercises have something to react to.

    Prices are chosen relative to the current market: with probability
    [aggressiveness] the bot crosses the opposite best (a marketable order
    that trades immediately), otherwise it rests a few cents away from its
    own side's best. {!Jsip_bot_runtime.Bot_runtime} does not track BBOs, so
    the bot keeps its own per-symbol cache, updated from
    [Best_bid_offer_update] events. When a needed price is missing (empty
    book) it falls back to the oracle's fundamental. All randomness is drawn
    from [Context.random] so a scenario stays reproducible from its seed. *)
module Config : sig
  type t

  (** Build a noise-trader config. The BBO cache and client-order-id source
      are internal and start empty.

      - [tick_chance] gates whether a given [on_tick] sends any order at all,
        so the bot can run on a fast clock yet stay sparse.
      - [aggressiveness] is the probability an order is marketable (crosses
        the spread) rather than resting away from the best.
      - [time_in_force_distribution] is the distribution the order's
        time-in-force is drawn from; expressing it over all of
        {!Time_in_force.t} means a new order type is mixed in by adding a
        weighted entry rather than by changing this bot. *)
  val create
    :  symbols:Symbol_id.t list
    -> mean_size:int
    -> tick_chance:Percent.t
    -> aggressiveness:Percent.t
    -> time_in_force_distribution:Time_in_force.t Bot_random.distribution
    -> t
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config
