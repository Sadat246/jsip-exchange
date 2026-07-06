open! Core
open! Async
open Jsip_types

(** A pathological exchange participant used to stress-test the exchange.

    Its misbehavior is data-driven via {!Config.behavior}, so pathologies are
    added as new variants without changing the runtime wiring. Two exist:

    - [Resource_exhaustion]: on every tick it fires a tight burst of many
      orders, deliberately loading the server's bounded request queue, the
      dispatcher's per-event fan-out, and the (unbounded) subscriber pipes.
      It has no trading strategy and does not try to make money.

    - [Pump_and_dump]: a stateful two-phase manipulation on a single symbol.
      It fires marketable buys to walk the price up ([Accumulate]), and once
      the observed mid has risen a target fraction it flips and sells its
      inventory into the bids left by anyone who chased the move
      ([Distribute]). It profits only from price-chasers (e.g. a momentum
      trader), not from a fundamental-anchored market maker -- and it decides
      when to dump purely from observed prices, never the oracle.

    A single scenario can run several independent instances by adding several
    [Bot_spec.t] entries: each entry sets that instance's participant name
    and RNG seed (both live on the spec, and the seeded RNG is reached
    through [Context.random]) and its own [Config.t], so instances tune
    independently. *)
module Config : sig
  (** Parameters for the [Resource_exhaustion] behavior. *)
  type resource_exhaustion_params =
    { orders_per_burst : int
    ; buy_chance : Percent.t
    ; marketable_chance : Percent.t
    ; time_in_force_distribution : Time_in_force.t Bot_random.distribution
    ; mean_size : int
    ; price_jitter_cents : int
    }

  (** Phases of the [Pump_and_dump] behavior. Advances
      [Accumulate -> Distribute -> Done] and never moves backward. *)
  type pump_and_dump_phase =
    | Accumulate
    | Distribute
    | Done
  [@@deriving sexp_of]

  (** Parameters and live state for the [Pump_and_dump] behavior. The knob
      fields are set once; the [mutable] fields are the running state of the
      scheme (phase, position, cost/proceeds, price anchor) and are exposed
      so tests can observe how a run progresses. Build one with
      {!pump_and_dump_params}, which seeds the state. *)
  type pump_and_dump_params =
    { target_symbol : Symbol.t
    ; pump_target_pct : Percent.t
    ; clip_size : int
    ; max_inventory : int
    ; give_up_ticks : int
    ; aggression_offset_cents : int
    ; entry_time_in_force : Time_in_force.t
    ; mutable phase : pump_and_dump_phase
    ; mutable position : int
    ; mutable cost_cents : int
    ; mutable proceeds_cents : int
    ; mutable anchor_cents : int option
    ; mutable ticks_in_phase : int
    }

  (** [pump_and_dump_params ~target_symbol ...] builds a params record with
      its mutable state seeded to a fresh run ([Accumulate], flat position,
      no anchor). See the field comments in {!Spammer} for what each knob
      controls. *)
  val pump_and_dump_params
    :  target_symbol:Symbol.t
    -> pump_target_pct:Percent.t
    -> clip_size:int
    -> max_inventory:int
    -> give_up_ticks:int
    -> aggression_offset_cents:int
    -> entry_time_in_force:Time_in_force.t
    -> pump_and_dump_params

  (** How the spammer misbehaves. [Resource_exhaustion] is a strategy-free
      flood; [Pump_and_dump] is a stateful two-phase price manipulation. *)
  type behavior =
    | Resource_exhaustion of resource_exhaustion_params
    | Pump_and_dump of pump_and_dump_params

  type t

  val create : symbols:Symbol.t list -> behavior:behavior -> t
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config
