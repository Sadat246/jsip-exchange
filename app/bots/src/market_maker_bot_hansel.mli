open! Core
open! Async
open Jsip_types

(** A dynamic, multi-symbol market maker on the bot-runtime interface.

    For each configured symbol it quotes a ladder of bids and asks around the
    symbol's fundamental value (read from the oracle via
    [Context.fundamental]), skews the ladder by its inventory, and re-quotes
    as it gets filled.

    Per-symbol state -- inventory, an adaptive half-spread, and the set of
    resting orders -- is tracked internally and updated from the events the
    bot receives. When the observed market spread blows out past
    [max_spread_cents] (e.g. a whale has swept a side of the book), the maker
    stands aside rather than quoting into the dislocation. *)
module Config : sig
  type t

  (** Build a market-maker config. Per-symbol state is initialised empty and
      evolves as events arrive; fair value is read from the oracle at
      (re)seed time, so it is not part of the config.

      - [half_spread_cents] is the starting half-spread on each side;
        thereafter it adapts toward the observed market half-spread, floored
        at [min_half_spread_cents].
      - [max_spread_cents] is the whale tolerance: if the market spread
        exceeds it, the maker stops quoting that symbol until the spread
        recovers. *)
  val create
    :  symbols:Symbol_id.t list
    -> size_per_level:int
    -> num_levels:int
    -> inventory_skew_cents_per_share:int
    -> half_spread_cents:int
    -> min_half_spread_cents:int
    -> max_spread_cents:int
    -> t
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config
