(** Shared scaffolding for the bot expect tests in this directory: a
    recording {!Bot_runtime} harness plus fixtures (a fixed BBO, common
    symbols and participants). Made for opening, like
    [Jsip_test_harness.E2e_helpers] — each [test_<bot>.ml] file opens it. *)

open! Core
open! Async
open Jsip_types
open Jsip_fundamental
open Jsip_bot_runtime

let aapl = Symbol_id.Private.of_int 0
let msft = Symbol_id.Private.of_int 1
let alice = Participant.of_string "Alice"

(* A zero-volatility, zero-mean-reversion oracle so [Context.fundamental]
   returns a constant per symbol — the price a bot reads is then fully
   determined by [initial_price_cents], which keeps price assertions
   deterministic. [aapl] is always present at [initial_price_cents];
   [extra_symbol_prices] registers additional symbols (with their own
   starting prices) for tests that exercise multi-symbol routing. *)
let oracle_config ~initial_price_cents ~extra_symbol_prices =
  let entry (symbol, initial_price_cents) =
    ( symbol
    , { Fundamental_oracle.Config.initial_price_cents
      ; volatility_cents_per_sec = 0.0
      ; mean_reversion_strength = 0.0
      ; tick_interval = Time_ns.Span.of_sec 1.0
      } )
  in
  Symbol_id.Map.of_alist_exn
    (List.map ((aapl, initial_price_cents) :: extra_symbol_prices) ~f:entry)
;;

(* Build a runtime around a bot module with a mock submit/cancel that records
   what the bot does. *)
let make_recording_bot
  (type cfg)
  (bot_module : (module Bot_runtime.Bot with type Config.t = cfg))
  (config : cfg)
  ?(initial_price_cents = 15000)
  ?(extra_symbol_prices = [])
  ()
  =
  let submitted = ref [] in
  let cancelled = ref [] in
  let submit request =
    submitted := request :: !submitted;
    return (Ok ())
  in
  let cancel order_id =
    cancelled := order_id :: !cancelled;
    return (Ok ())
  in
  let oracle =
    Fundamental_oracle.create
      (oracle_config ~initial_price_cents ~extra_symbol_prices)
      ~seed:42
  in
  let bot =
    Bot_runtime.create
      bot_module
      config
      ~participant:alice
      ~oracle
      ~rng:(Splittable_random.of_int 7)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.0)
  in
  bot, submitted, cancelled
;;

(* Drive [bot] through [ticks] sequential [on_tick] calls, the way
   {!Bot_runtime.start}'s clock loop would -- but synchronously, since that
   loop never returns and so is unusable in an expect test. Every
   clock-driven bot (noise trader, spammer, cancel storm, momentum) shares
   this; the per-file [run_ticks] copies it replaced differed only in which
   module's [on_tick] they named, which the runtime already closes over. *)
let drive_ticks bot ~ticks =
  Deferred.List.iter ~how:`Sequential (List.init ticks ~f:Fn.id) ~f:(fun _ ->
    Bot_runtime.For_testing.manual_tick bot)
;;

(* Feed a sequence of events to [bot]'s [on_event], in order. Bots primed
   with a run of trades, or replayed [Order_accept]s for a seeded ladder,
   share this loop; only the events they build differ, so that stays in each
   test. *)
let feed_events bot events =
  Deferred.List.iter ~how:`Sequential events ~f:(Bot_runtime.feed_event bot)
;;

(* Print a list of requests, one per line: [SIDE SYMBOL size@price tif]. The
   building block for the printers below. *)
let print_requests (requests : Order.Request.t list) =
  List.iter requests ~f:(fun req ->
    printf
      !"%{Side} %{Symbol_id} %d@%{Price#dollar} %{Time_in_force}\n"
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price
      req.time_in_force)
;;

(* Print what a bot submitted, oldest first -- the order it fired the orders.
   Right for one-order-per-tick strategies where submission order is the
   signal (noise trader, momentum trader, spammer bursts). *)
let print_submitted (submitted : Order.Request.t list ref) =
  print_requests (List.rev !submitted)
;;

(* Print a submitted ladder sorted by side then price rather than in
   submission order. A market maker seeds its two-sided ladder with a
   parallel [Deferred.List.iter] over levels, so the order the buys and sells
   land in [submitted] is unspecified; sorting makes the quoted ladder
   deterministic and readable (all bids low-to-high, then all asks
   low-to-high). *)
let print_ladder (submitted : Order.Request.t list ref) =
  !submitted
  |> List.sort ~compare:(fun (a : Order.Request.t) (b : Order.Request.t) ->
    [%compare: Side.t * int]
      (a.side, Price.to_int_cents a.price)
      (b.side, Price.to_int_cents b.price))
  |> print_requests
;;

(* Like {!print_submitted} but also shows the client order id and symbol, for
   tests that care about id progression across ticks or per-symbol routing. *)
let print_orders (submitted : Order.Request.t list ref) =
  List.iter (List.rev !submitted) ~f:(fun (req : Order.Request.t) ->
    printf
      !"cid=%{Client_order_id} %{Side} %{Symbol_id} %d@%{Price#dollar} \
        %{Time_in_force}\n"
      req.client_order_id
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price
      req.time_in_force)
;;

(* A fixed two-sided market to prime a bot's BBO cache before we tick it, so
   its price choices react to a book rather than only the fundamental. *)
let best_bid_cents = 14990
let best_ask_cents = 15010

(* Feed the bot a two-sided BBO at a chosen mid, so a test can move the
   market and watch a price-reactive bot respond. *)
let feed_bbo bot ~bid_cents ~ask_cents =
  Bot_runtime.feed_event
    bot
    (Best_bid_offer_update
       { symbol = aapl
       ; bbo =
           { bid =
               Some
                 { price = Price.of_int_cents bid_cents
                 ; size = Size.of_int 100
                 }
           ; ask =
               Some
                 { price = Price.of_int_cents ask_cents
                 ; size = Size.of_int 100
                 }
           }
       })
;;

(* A fixed two-sided market to prime a bot's BBO cache before we tick it, so
   its price choices react to a book rather than only the fundamental. *)
let feed_fixed_bbo bot =
  feed_bbo bot ~bid_cents:best_bid_cents ~ask_cents:best_ask_cents
;;

(* Feed back a fill in which our bot ([alice]) is the aggressor on [side], so
   a bot that tracks its own position and P&L off the fill stream advances
   exactly as a real fill would drive it. The resting side is some other
   participant. *)
let feed_self_fill bot ~side ~price_cents ~size =
  Bot_runtime.feed_event
    bot
    (Fill
       { fill_id = 0
       ; symbol = aapl
       ; price = Price.of_int_cents price_cents
       ; size = Size.of_int size
       ; aggressor_order_id = Order_id.For_testing.of_int 1
       ; aggressor_client_order_id = Client_order_id.of_int 1
       ; aggressor_participant = alice
       ; aggressor_side = side
       ; resting_order_id = Order_id.For_testing.of_int 2
       ; resting_client_order_id = Client_order_id.of_int 2
       ; resting_participant = Participant.of_string "counterparty"
       })
;;

(* Classify a submitted order as marketable against the fixed BBO above. This
   is an independent check on the resulting price -- it does not reimplement
   a strategy's price-picking logic. *)
let is_marketable (req : Order.Request.t) =
  match req.side with
  | Buy -> Price.( >= ) req.price (Price.of_int_cents best_ask_cents)
  | Sell -> Price.( <= ) req.price (Price.of_int_cents best_bid_cents)
;;

(* [day_pct]% resting [Day] orders, the balance [Ioc]. *)
let day_ioc_mix ~day_pct =
  [ Time_in_force.Day, Percent.of_percentage day_pct
  ; Ioc, Percent.of_percentage (100. -. day_pct)
  ]
;;

(* Smoke test: drive the do-nothing reference bot through one event so the
   harness itself is exercised even if a bot's test file goes quiet. *)
module Inert_bot = struct
  module Config = struct
    type t = unit
  end

  let name = "inert"
  let on_start () _ctx = return ()
  let on_tick () _ctx = return ()
  let on_event () _ctx _event = return ()
end

let%expect_test "make_recording_bot wires up a runnable bot" =
  let bot, submitted, _cancelled =
    make_recording_bot (module Inert_bot) () ()
  in
  let%bind () =
    Bot_runtime.feed_event
      bot
      (Order_accept
         { order_id = Order_id.For_testing.of_int 1
         ; participant = alice
         ; request =
             { client_order_id = Client_order_id.of_int 1
             ; symbol = aapl
             ; participant = alice
             ; side = Buy
             ; price = Price.of_int_cents 15000
             ; size = Size.of_int 10
             ; time_in_force = Day
             }
         })
  in
  print_submitted submitted;
  [%expect {| |}];
  return ()
;;
