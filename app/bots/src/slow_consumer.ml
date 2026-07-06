open! Core
open! Async
open Jsip_types

module Config = struct
  type t =
    { read_delay : Time_ns.Span.t
    ; mutable consumed : int
    (* how many events we have finished handling so far *)
    }

  let create ~read_delay = { read_delay; consumed = 0 }
end

let name = "slow-consumer"

(* Nothing to set up: this bot only listens, it never trades. *)
let on_start (_ : Config.t) _ctx = return ()

(* On its own (slow) clock, report how far behind it is. The market maker is
   re-quoting many times a second; watch this count crawl to see the lag. *)
let on_tick (config : Config.t) _ctx =
  printf
    "[slow-consumer] finished handling %d events so far\n"
    config.consumed;
  return ()
;;

(* This is where the whole pathology lives.

   The runner drains our market-data feed with [Pipe.iter feed ~f:on_event],
   which will NOT pull the next event until the [unit Deferred.t] we return
   here becomes determined. A normal consumer returns immediately, keeping
   the pipe empty. We want to be pathologically slow instead. *)
let on_event (config : Config.t) _ctx (_ : Exchange_event.t) =
  let%map () = Clock_ns.after config.read_delay in
  config.consumed <- config.consumed + 1
;;
