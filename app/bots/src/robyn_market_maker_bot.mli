open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime
module Context = Bot_runtime.Context

module Config : sig
  type t
end

val name : string
val on_start : Config.t -> Context.t -> unit Deferred.t
val on_tick : Config.t -> Context.t -> unit Deferred.t
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t

val create_config
  :  ?testing:bool
  -> unit
  -> size_per_level:Int.t
  -> num_levels:Int.t
  -> inventory_skew_cents_per_share:Int.t
  -> symbols:Symbol_id.t List.t
  -> Config.t
