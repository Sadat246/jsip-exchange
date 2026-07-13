open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

(* A pathological bot that simulates an abusive exchange participant. Rather
   than trading with any strategy, it hammers the exchange with large bursts
   of orders on every tick, deliberately stressing three shared resources:

   - the server's bounded request queue (submissions pile up faster than the
     single matching loop can drain them),
   - the dispatcher's per-event fan-out work, and
   - the (unbounded) subscriber pipes every accept / BBO / trade event is
     written to.

   The spammer is [Config.behavior]-driven so pathologies are added as new
   variants of {!Config.behavior} plus a match arm in {!on_tick}, without
   touching the runtime wiring. Two behaviors exist: [Resource_exhaustion] (a
   strategy-free flood that stresses shared server resources) and
   [Pump_and_dump] (a stateful two-phase manipulation that walks a price up
   on marketable buys, then dumps its inventory into whoever chased the
   move). *)

module Config = struct
  type resource_exhaustion_params =
    { orders_per_burst : int
        (* Orders fired in a single tight burst per tick -- the core stress
           lever. Combined with a small tick interval this pins the request
           queue and floods every subscriber pipe. *)
    ; buy_chance : Percent.t
        (* Probability an order is a buy. 50% is balanced; skewing it leans
           on one side of the book (a crude directional-pressure knob,
           distinct from the dedicated [Pump_and_dump] behavior below). *)
    ; marketable_chance : Percent.t
        (* Probability an order crosses the spread and trades immediately
           (generating fills, and therefore extra session and trade-report
           fan-out) rather than resting in the book. *)
    ; time_in_force_distribution : Time_in_force.t Bot_random.distribution
        (* Distribution the order's time-in-force is drawn from. Expressed
           over all of {!Time_in_force.t} so new order types slot in as new
           weighted entries. Resting [Day] orders pile up in the book; [Ioc]
           orders churn the matching loop. *)
    ; mean_size : int (* Center of the per-order size distribution. *)
    ; price_jitter_cents : int
    (* Half-width of the uniform price band around the reference price, so
       the burst spreads across many price levels. *)
    }

  (* Phases of a pump-and-dump. State advances
     [Accumulate -> Distribute -> Done] and never moves backward. *)
  type pump_and_dump_phase =
    | Accumulate (* buying, to walk the price up *)
    | Distribute (* dumping the accumulated inventory *)
    | Done (* flat; the scheme has run its course *)
  [@@deriving sexp_of]

  type pump_and_dump_params =
    { target_symbol : Symbol_id.t
        (* The single symbol to manipulate. Concentrating every clip on one
           symbol is what moves its price; spreading across many would dilute
           the impact to nothing. *)
    ; pump_target_pct : Percent.t
        (* Flip from [Accumulate] to [Distribute] once the observed mid has
           risen this far above [anchor_cents]. The success trigger, derived
           purely from observed prices -- never the fundamental oracle, so
           the bot exits on the same information a real manipulator would
           have. *)
    ; clip_size : int
        (* Shares taken per tick. The push-rate lever: a bigger clip walks
           the book faster and moves price harder, but is more conspicuous. *)
    ; max_inventory : int
        (* Cap on the accumulated long. Not a flip trigger: it clamps
           per-tick buying so the position never runs away if the price won't
           rise. *)
    ; give_up_ticks : int
        (* If still accumulating after this many ticks (the price never
           reached [pump_target_pct]), flip to [Distribute] and unwind
           anyway. The honest "the scheme failed" path, so the bot never
           holds forever. *)
    ; aggression_offset_cents : int
        (* How far past the opposite touch each clip is priced, so it
           reliably crosses and trades rather than resting at the touch. *)
    ; entry_time_in_force : Time_in_force.t
        (* Time-in-force of every clip. [Ioc] keeps clips clean -- they trade
           what they can immediately and leave no resting exposure behind. *)
    ; mutable phase : pump_and_dump_phase
    ; mutable position : int
        (* Signed shares held; long while accumulating. *)
    ; mutable cost_cents : int (* Running notional paid while buying. *)
    ; mutable proceeds_cents : int
        (* Running notional taken while selling. *)
    ; mutable anchor_cents : int option
        (* Reference price, captured from the first observed two-sided BBO
           mid; [None] until we have seen one. *)
    ; mutable ticks_in_phase : int (* Ticks spent in the current phase. *)
    }

  (* Build a [pump_and_dump_params] from its knobs, seeding the mutable state
     to a fresh run ([Accumulate], flat, no anchor yet). Callers set the
     knobs and never have to know the initial bookkeeping. *)
  let pump_and_dump_params
    ~target_symbol
    ~pump_target_pct
    ~clip_size
    ~max_inventory
    ~give_up_ticks
    ~aggression_offset_cents
    ~entry_time_in_force
    =
    { target_symbol
    ; pump_target_pct
    ; clip_size
    ; max_inventory
    ; give_up_ticks
    ; aggression_offset_cents
    ; entry_time_in_force
    ; phase = Accumulate
    ; position = 0
    ; cost_cents = 0
    ; proceeds_cents = 0
    ; anchor_cents = None
    ; ticks_in_phase = 0
    }
  ;;

  type behavior =
    | Resource_exhaustion of resource_exhaustion_params
    | Pump_and_dump of pump_and_dump_params

  type t =
    { symbols : Symbol_id.t list
    ; behavior : behavior
    ; generator : Client_order_id.Generator.t
    ; bbo_cache : Bbo.t Symbol_id.Table.t
    }

  let create ~symbols ~behavior =
    { symbols
    ; behavior
    ; generator = Client_order_id.Generator.create ()
    ; bbo_cache = Symbol_id.Table.create ()
    }
  ;;
end

let name = "Spammer"

(* Smallest amount by which a "marketable" order crosses past the opposite
   best price, so it is guaranteed to trade rather than sit at the touch. *)
let cross_cents = 1

let random_size rng ~mean_size =
  let half = Int.max 1 (mean_size / 2) in
  let lo = Int.max 1 (mean_size - half) in
  let hi = mean_size + half in
  Size.of_int (Splittable_random.int rng ~lo ~hi)
;;

(* Reference price for [side]'s own best, taken from the last BBO we cached;
   falls back to the fundamental when that side of the book is empty. *)
let reference_price (config : Config.t) context symbol ~side =
  let cached =
    let%bind.Option bbo = Hashtbl.find config.bbo_cache symbol in
    Bbo.price bbo side
  in
  match cached with
  | Some price -> Price.to_int_cents price
  | None -> Price.to_int_cents (Context.fundamental context symbol)
;;

(* Choose a price for an order. A marketable order crosses the *opposite*
   best (so it trades); a resting order sits a few cents away from *this*
   side's best (so it stays on the book). Random jitter spreads the burst
   across price levels. *)
let choose_price config context symbol ~side ~marketable ~jitter_cents rng =
  let jitter = Splittable_random.int rng ~lo:0 ~hi:jitter_cents in
  let cents =
    match marketable with
    | true ->
      let opposite =
        reference_price config context symbol ~side:(Side.flip side)
      in
      (match side with
       | Side.Buy -> opposite + cross_cents + jitter
       | Sell -> opposite - cross_cents - jitter)
    | false ->
      let own = reference_price config context symbol ~side in
      (match side with
       | Side.Buy -> own - cross_cents - jitter
       | Sell -> own + cross_cents + jitter)
  in
  Price.of_int_cents (Int.max 1 cents)
;;

let random_request
  (config : Config.t)
  context
  (params : Config.resource_exhaustion_params)
  rng
  =
  let symbol = Bot_random.uniform_exn rng config.symbols in
  let side =
    if Bot_random.does_occur rng params.buy_chance then Side.Buy else Sell
  in
  let size = random_size rng ~mean_size:params.mean_size in
  let marketable = Bot_random.does_occur rng params.marketable_chance in
  let price =
    choose_price
      config
      context
      symbol
      ~side
      ~marketable
      ~jitter_cents:params.price_jitter_cents
      rng
  in
  let time_in_force =
    Bot_random.categorically_weighted_exn
      rng
      params.time_in_force_distribution
  in
  ({ client_order_id = Client_order_id.Generator.next config.generator
   ; symbol
   ; participant = Context.participant context
   ; side
   ; price
   ; size
   ; time_in_force
   }
   : Order.Request.t)
;;

(* Fire the whole burst at once. We intentionally do NOT submit one order per
   tick: [~how:`Parallel] launches every submission concurrently so the burst
   lands as a tight cluster, maximizing pressure on the request queue and the
   dispatcher fan-out. Backpressure from the bounded request queue naturally
   couples the burst rate to the matching loop's drain rate. *)
let resource_exhaustion_burst
  config
  context
  (params : Config.resource_exhaustion_params)
  =
  let rng = Context.random context in
  Deferred.List.iter
    ~how:`Parallel
    (List.init params.orders_per_burst ~f:Fn.id)
    ~f:(fun _ ->
      Deferred.ignore_m
        (Context.submit context (random_request config context params rng)))
;;

(* Mid of a two-sided BBO in integer cents, or [None] if either side is
   empty. Anchors the scheme and measures how far price has moved -- all from
   observed market data, never the oracle. *)
let observed_mid_of_bbo bbo =
  let%bind.Option bid = Bbo.price bbo Side.Buy in
  let%map.Option ask = Bbo.price bbo Side.Sell in
  (Price.to_int_cents bid + Price.to_int_cents ask) / 2
;;

let observed_mid (config : Config.t) symbol =
  let%bind.Option bbo = Hashtbl.find config.bbo_cache symbol in
  observed_mid_of_bbo bbo
;;

(* Send one marketable clip: a single order of [size] shares priced to cross
   the opposite touch (via {!choose_price} with [~marketable:true]) so it
   trades immediately rather than resting. A buy lifts the offer during
   [Accumulate]; a sell hits the bid during [Distribute]. *)
let submit_clip
  (config : Config.t)
  context
  (params : Config.pump_and_dump_params)
  ~side
  ~size
  =
  let rng = Context.random context in
  let price =
    choose_price
      config
      context
      params.target_symbol
      ~side
      ~marketable:true
      ~jitter_cents:params.aggression_offset_cents
      rng
  in
  let request : Order.Request.t =
    { client_order_id = Client_order_id.Generator.next config.generator
    ; symbol = params.target_symbol
    ; participant = Context.participant context
    ; side
    ; price
    ; size = Size.of_int size
    ; time_in_force = params.entry_time_in_force
    }
  in
  Deferred.ignore_m (Context.submit context request)
;;

(* Fold one of our own fills into the running position and P&L. Only fills we
   are a party to move the books, and self-trade prevention means we are at
   most one side. Buys add cost, sells add proceeds, so realized P&L at the
   end is [proceeds_cents - cost_cents]. *)
let apply_pump_fill
  context
  (params : Config.pump_and_dump_params)
  (fill : Fill.t)
  =
  let me = Context.participant context in
  let our_side =
    if Participant.equal fill.aggressor_participant me
    then Some fill.aggressor_side
    else if Participant.equal fill.resting_participant me
    then Some (Side.flip fill.aggressor_side)
    else None
  in
  match our_side with
  | None -> ()
  | Some side ->
    let qty = Size.to_int fill.size in
    let notional = Price.to_int_cents fill.price * qty in
    params.position <- params.position + (Side.sign side * qty);
    (match side with
     | Side.Buy -> params.cost_cents <- params.cost_cents + notional
     | Sell -> params.proceeds_cents <- params.proceeds_cents + notional)
;;

(* One tick of the pump-and-dump state machine. [Accumulate] fires buy clips
   until the observed mid has risen [pump_target_pct] off the anchor (or the
   [give_up_ticks] budget runs out), then [Distribute] unwinds the inventory
   with sell clips until flat, then [Done]. *)
let pump_and_dump_tick
  (config : Config.t)
  context
  (params : Config.pump_and_dump_params)
  =
  match params.phase with
  | Done -> Deferred.unit
  | Accumulate ->
    params.ticks_in_phase <- params.ticks_in_phase + 1;
    let target_reached =
      match
        params.anchor_cents, observed_mid config params.target_symbol
      with
      | Some anchor, Some mid ->
        let rise_cents = Float.of_int (mid - anchor) in
        let threshold_cents =
          Percent.apply params.pump_target_pct (Float.of_int anchor)
        in
        Float.( >= ) rise_cents threshold_cents
      | _, _ -> false
    in
    if target_reached || params.ticks_in_phase >= params.give_up_ticks
    then (
      params.phase <- Distribute;
      params.ticks_in_phase <- 0;
      Deferred.unit)
    else (
      let room = params.max_inventory - params.position in
      let size = Int.min params.clip_size room in
      if size <= 0
      then Deferred.unit
      else submit_clip config context params ~side:Side.Buy ~size)
  | Distribute ->
    if params.position <= 0
    then (
      params.phase <- Done;
      Deferred.unit)
    else (
      let size = Int.min params.clip_size params.position in
      submit_clip config context params ~side:Side.Sell ~size)
;;

let on_start (_config : Config.t) _context = Deferred.unit

let on_tick (config : Config.t) context =
  match config.behavior with
  | Resource_exhaustion params ->
    resource_exhaustion_burst config context params
  | Pump_and_dump params -> pump_and_dump_tick config context params
;;

(* Cache every BBO for price reference. The pump-and-dump additionally
   anchors its price target on the first two-sided market it sees and tracks
   its own fills; the resource-exhaustion flood ignores everything but the
   cache. *)
let on_event (config : Config.t) context (event : Exchange_event.t) =
  (match event with
   | Best_bid_offer_update { symbol; bbo } ->
     Hashtbl.set config.bbo_cache ~key:symbol ~data:bbo
   | Order_accept _ | Fill _ | Order_cancel _ | Order_reject _
   | Cancel_reject _ | Trade_report _ ->
     ());
  (match config.behavior with
   | Resource_exhaustion _ -> ()
   | Pump_and_dump params ->
     (match event with
      | Best_bid_offer_update { symbol; bbo } ->
        if Symbol_id.equal symbol params.target_symbol
           && Option.is_none params.anchor_cents
        then (
          match observed_mid_of_bbo bbo with
          | Some mid -> params.anchor_cents <- Some mid
          | None -> ())
      | Fill fill ->
        if Symbol_id.equal fill.symbol params.target_symbol
        then apply_pump_fill context params fill
      | Order_accept _ | Order_cancel _ | Order_reject _ | Cancel_reject _
      | Trade_report _ ->
        ()));
  Deferred.unit
;;
