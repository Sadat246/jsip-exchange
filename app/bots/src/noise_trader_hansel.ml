(** A noise trader: a stand-in for the huge amount of real-world buying and
    selling that carries no view on where the price is headed (index funds
    rebalancing, retail traders, a corporation liquidating shares, ...).

    From the matching engine's point of view that activity is
    indistinguishable from random buying and selling, so this bot simply
    picks a symbol, a side, a size, a price, and a time-in-force more or less
    at random and submits an order. It doesn't try to make money; together
    with {!Jsip_market_maker} it gives the engine something to do — fills
    happen, the BBO moves, and the informed bots in later exercises have
    something to react to.

    Prices are chosen relative to the current market: with probability
    [aggressiveness] the bot crosses the opposite best (a marketable order
    that trades immediately), otherwise it rests a few cents away from its
    own side's best. {!Jsip_bot_runtime.Bot_runtime} does not track BBOs, so
    the bot maintains its own per-symbol cache from [Best_bid_offer_update]
    events in {!on_event}. When a needed price is missing (empty book) it
    falls back to the oracle's fundamental via {!Context.fundamental}.

    All randomness is drawn from {!Context.random} so a scenario stays
    reproducible from its seed. *)

open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

(* Half-width of the size range: each order's size is drawn uniformly from
   [mean_size - size_jitter, mean_size + size_jitter] (clamped to at least
   [min_size]) so the bot doesn't always trade the same quantity. *)
let size_jitter = 3

(* Smallest order size we will ever submit, in case the jittered draw pushes
   below one share. *)
let min_size = 1

(* The "few cents" the spec calls for: the per-order price offset is drawn
   uniformly from [min_price_offset_cents, max_price_offset_cents]. For a
   marketable order this is how far past the opposite best we reach; for a
   resting order it is how far away from our own best we sit. *)
let min_price_offset_cents = 1
let max_price_offset_cents = 3

module Config = struct
  type t =
    { symbols : Symbol.t list
    (** Symbols the bot trades, chosen uniformly. *)
    ; mean_size : int (** Center of each order's randomized size. *)
    ; tick_chance : Percent.t
    (** Probability that a given [on_tick] sends any order at all. Lets
        [on_tick] run on a fast clock yet stay sparse. *)
    ; aggressiveness : Percent.t
    (** Probability that an order is marketable (crosses the spread) rather
        than resting away from the best. *)
    ; time_in_force_distribution : Time_in_force.t Bot_random.distribution
    (** Distribution the order's time-in-force is drawn from. Expressed over
        all of {!Jsip_types.Time_in_force.t} rather than a single
        [Ioc]-vs-[Day] probability, so a new order type is supported by
        adding a weighted entry rather than by changing this bot. Resting
        [Day] orders pile up on the book, which later exercises rely on. *)
    ; bbo_cache : Bbo.t Symbol.Table.t
    (** Latest BBO per symbol, maintained from market-data events. *)
    ; generator : Client_order_id.Generator.t
    (** Sequential, collision-free client order IDs for our orders. *)
    }

  let create
    ~symbols
    ~mean_size
    ~tick_chance
    ~aggressiveness
    ~time_in_force_distribution
    =
    { symbols
    ; mean_size
    ; tick_chance
    ; aggressiveness
    ; time_in_force_distribution
    ; bbo_cache = Symbol.Table.create ()
    ; generator = Client_order_id.Generator.create ()
    }
  ;;
end

let name = "Noise_Trader"

(* The best price on [side] for [symbol] from our BBO cache, in cents,
   falling back to the oracle fundamental when that side of the book is
   empty. *)
let reference_price_cents (config : Config.t) context symbol side =
  let from_book =
    let%bind.Option bbo = Hashtbl.find config.bbo_cache symbol in
    Bbo.price bbo side
  in
  match from_book with
  | Some price -> Price.to_int_cents price
  | None -> Price.to_int_cents (Context.fundamental context symbol)
;;

let pick_symbol (config : Config.t) rng =
  Bot_random.uniform_exn rng config.symbols
;;

let pick_side rng : Side.t = Bot_random.uniform_exn rng [ Side.Buy; Sell ]

let pick_size (config : Config.t) rng =
  let raw =
    Splittable_random.int
      rng
      ~lo:(config.mean_size - size_jitter)
      ~hi:(config.mean_size + size_jitter)
  in
  Size.of_int (Int.max min_size raw)
;;

(* Where to price a [size]-share order on [side]. With probability
   [aggressiveness] we cross the opposite best by [offset] cents (a
   marketable order); otherwise we sit [offset] cents behind our own best (a
   resting order). *)
let pick_price (config : Config.t) context rng symbol side =
  let offset =
    Splittable_random.int
      rng
      ~lo:min_price_offset_cents
      ~hi:max_price_offset_cents
  in
  let marketable = Bot_random.does_occur rng config.aggressiveness in
  let cents =
    match marketable, (side : Side.t) with
    (* Marketable buy: reach up past the best ask so we cross. *)
    | true, Buy -> reference_price_cents config context symbol Sell + offset
    (* Marketable sell: reach down past the best bid. *)
    | true, Sell -> reference_price_cents config context symbol Buy - offset
    (* Resting buy: sit below the best bid so we rest. *)
    | false, Buy -> reference_price_cents config context symbol Buy - offset
    (* Resting sell: sit above the best ask. *)
    | false, Sell ->
      reference_price_cents config context symbol Sell + offset
  in
  Price.of_int_cents cents
;;

let pick_time_in_force (config : Config.t) rng : Time_in_force.t =
  Bot_random.categorically_weighted_exn rng config.time_in_force_distribution
;;

let on_start (_config : Config.t) _context = Deferred.unit

let on_tick (config : Config.t) context =
  let rng = Context.random context in
  if not (Bot_random.does_occur rng config.tick_chance)
  then Deferred.unit
  else (
    let symbol = pick_symbol config rng in
    let side = pick_side rng in
    let size = pick_size config rng in
    let price = pick_price config context rng symbol side in
    let time_in_force = pick_time_in_force config rng in
    let request : Order.Request.t =
      { client_order_id = Client_order_id.Generator.next config.generator
      ; symbol
      ; participant = Context.participant context
      ; side
      ; price
      ; size
      ; time_in_force
      }
    in
    Deferred.ignore_m (Context.submit context request))
;;

let on_event (config : Config.t) _context (event : Exchange_event.t) =
  match event with
  | Best_bid_offer_update { symbol; bbo } ->
    Hashtbl.set config.bbo_cache ~key:symbol ~data:bbo;
    Deferred.unit
  | _ -> Deferred.unit
;;
