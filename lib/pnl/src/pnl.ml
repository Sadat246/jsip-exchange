open! Core
open Jsip_types

(* Format an integer number of cents as a signed dollar string, e.g. [-150]
   becomes ["-$1.50"]. We roll our own instead of using
   {!Price.to_string_dollar} because P&L amounts are routinely negative,
   whereas a [Price.t] models a (non-negative) quote. *)
let dollars cents =
  let sign = if cents < 0 then "-" else "" in
  let cents = abs cents in
  sprintf "%s$%d.%02d" sign (cents / 100) (cents % 100)
;;

let string_of_price_opt = function
  | None -> "--"
  | Some price -> Price.to_string_dollar price
;;

module Summary = struct
  module Per_symbol = struct
    type t =
      { symbol : Symbol.t
      ; inventory : int
      ; average_entry_price : Price.t option
      ; reference_price : Price.t option
      ; realized_cents : int
      ; unrealized_cents : int
      }
    [@@deriving sexp_of, fields ~getters]
  end

  type t =
    { by_symbol : Per_symbol.t list
    ; total_realized_cents : int
    ; total_unrealized_cents : int
    }
  [@@deriving sexp_of, fields ~getters]

  let to_string_hum
    { by_symbol; total_realized_cents; total_unrealized_cents }
    =
    let rows =
      List.map
        by_symbol
        ~f:
          (fun
            { symbol
            ; inventory
            ; average_entry_price
            ; reference_price
            ; realized_cents
            ; unrealized_cents
            }
          ->
          [%string
            "  %{symbol#Symbol} inv=%{inventory#Int} \
             avg=%{string_of_price_opt average_entry_price} \
             ref=%{string_of_price_opt reference_price} realized=%{dollars \
             realized_cents} unrealized=%{dollars unrealized_cents}"])
    in
    let total =
      [%string
        "  TOTAL realized=%{dollars total_realized_cents} \
         unrealized=%{dollars total_unrealized_cents}"]
    in
    String.concat ~sep:"\n" (rows @ [ total ])
  ;;
end

(* A single participant's standing in a single symbol.

   [cost_basis_cents] is the {e signed} total cost of the open position, kept
   equal to [inventory * average_entry_price]. Storing it signed means a
   short has a negative cost basis, so [cost_basis_cents / inventory]
   recovers a positive average entry price for longs and shorts alike. *)
module Position = struct
  type t =
    { inventory : int
    ; cost_basis_cents : int
    ; realized_cents : int
    }
  [@@deriving sexp_of]

  let empty = { inventory = 0; cost_basis_cents = 0; realized_cents = 0 }

  let average_entry_cents t =
    if t.inventory = 0 then None else Some (t.cost_basis_cents / t.inventory)
  ;;

  (* Fold one execution (this participant trading [size] on [side] at
     [price]) into the position. *)
  let apply_execution t ~side ~price ~size =
    let dq = Side.sign side * Size.to_int size in
    let p = Price.to_int_cents price in
    let { inventory; cost_basis_cents; realized_cents } = t in
    if inventory = 0 || Sign.equal (Int.sign inventory) (Int.sign dq)
    then
      (* Growing the position (or opening from flat): the execution just
         folds into the cost basis; nothing is realized yet. *)
      { inventory = inventory + dq
      ; cost_basis_cents = cost_basis_cents + (dq * p)
      ; realized_cents
      }
    else (
      (* The execution reduces — and possibly flips — the position. This is
         the average entry price of the shares being closed. *)
      let avg = cost_basis_cents / inventory in
      (* TODO(human): compute [realized_delta], the cash realized by closing
         existing inventory with this execution.

         Facts you have in scope:
         - [inventory]: signed position before the fill (>0 long, <0 short)
         - [dq]: signed executed quantity (opposite sign to [inventory] here)
         - [avg], [p]: average entry price and execution price, in cents

         Only min(|dq|, |inventory|) shares actually close; a larger |dq|
         flips the position and opens the remainder at [p], which the cost
         basis update below already handles. Closing a long realizes (p -
         avg) per share sold; closing a short realizes (avg - p) per share
         covered. *)
      let closed = Int.min (Int.abs dq) (Int.abs inventory) in
      let realized_delta =
        (p - avg) * Sign.to_int (Int.sign inventory) * closed
      in
      let new_inventory = inventory + dq in
      let cost_basis_cents =
        if new_inventory = 0
           || Sign.equal (Int.sign new_inventory) (Int.sign inventory)
        then
          (* Partial or full close, no flip: the surviving shares keep the
             same average entry price. *)
          avg * new_inventory
        else
          (* Flip: the leftover quantity opens a fresh position at [p]. *)
          new_inventory * p
      in
      { inventory = new_inventory
      ; cost_basis_cents
      ; realized_cents = realized_cents + realized_delta
      })
  ;;

  let to_summary_row t ~symbol ~reference_price : Summary.Per_symbol.t =
    let average_entry_cents = average_entry_cents t in
    let average_entry_price =
      Option.map average_entry_cents ~f:Price.of_int_cents
    in
    let unrealized_cents =
      match average_entry_cents, reference_price with
      | Some avg, Some reference ->
        t.inventory * (Price.to_int_cents reference - avg)
      | None, _ | _, None -> 0
    in
    { symbol
    ; inventory = t.inventory
    ; average_entry_price
    ; reference_price
    ; realized_cents = t.realized_cents
    ; unrealized_cents
    }
  ;;
end

type t =
  { positions : Position.t Symbol.Map.t Participant.Map.t
  ; reference_prices : Price.t Symbol.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; reference_prices = Symbol.Map.empty }
;;

let update_position t ~participant ~symbol ~f =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let position =
    Map.find by_symbol symbol |> Option.value ~default:Position.empty
  in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:(f position) in
  { t with positions = Map.set t.positions ~key:participant ~data:by_symbol }
;;

let apply_fill t (fill : Fill.t) =
  let apply t ~participant ~side =
    update_position t ~participant ~symbol:fill.symbol ~f:(fun position ->
      Position.apply_execution
        position
        ~side
        ~price:fill.price
        ~size:fill.size)
  in
  (* A fill has two counterparties: the aggressor trades [aggressor_side],
     the resting participant trades the opposite side, both at [fill.price]. *)
  let t =
    apply t ~participant:fill.aggressor_participant ~side:fill.aggressor_side
  in
  apply
    t
    ~participant:fill.resting_participant
    ~side:(Side.flip fill.aggressor_side)
;;

let apply_trade_report t ~symbol ~price =
  { t with
    reference_prices = Map.set t.reference_prices ~key:symbol ~data:price
  }
;;

let summary t participant =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
    |> Map.to_alist
    |> List.map ~f:(fun (symbol, position) ->
      let reference_price = Map.find t.reference_prices symbol in
      Position.to_summary_row position ~symbol ~reference_price)
  in
  { Summary.by_symbol
  ; total_realized_cents =
      List.sum (module Int) by_symbol ~f:Summary.Per_symbol.realized_cents
  ; total_unrealized_cents =
      List.sum (module Int) by_symbol ~f:Summary.Per_symbol.unrealized_cents
  }
;;
