open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Jsip_dashboard_protocol
module Controller = Jsip_dashboard_controller.Controller

(* --- Leaf views: pure [Vdom.Node.t], no graph, no state. --- *)

module Sparkline = struct
  (* viewBox coordinate space; CSS stretches it to the panel width. *)
  let width = 300.
  let height = 60.
  let attr = Vdom.Attr.create

  let view ~(series : int list) ~min ~max =
    match series with
    | [] | [ _ ] ->
      let placeholder =
        Styles.style
          [%string
            "height:%{height#Float}px; display:flex; align-items:center; \
             color:%{Styles.text_dim}; font-size:13px"]
      in
      {%html|<div %{placeholder}>collecting…</div>|}
    | _ ->
      let n = List.length series in
      let range = Float.of_int (Int.max 1 (max - min)) in
      let points =
        List.mapi series ~f:(fun i v ->
          let x = Float.of_int i /. Float.of_int (n - 1) *. width in
          let y = height -. (Float.of_int (v - min) /. range *. height) in
          [%string "%{x#Float},%{y#Float}"])
        |> String.concat ~sep:" "
      in
      Vdom.Node.create_svg
        "svg"
        ~attrs:
          [ attr "viewBox" [%string "0 0 %{width#Float} %{height#Float}"]
          ; attr "preserveAspectRatio" "none"
          ; Styles.style
              [%string "width:100%; height:%{height#Float}px; display:block"]
          ]
        [ Vdom.Node.create_svg
            "polyline"
            ~attrs:
              [ attr "points" points
              ; attr "fill" "none"
              ; attr "stroke" Styles.accent
              ; attr "stroke-width" "2"
              ; attr "vector-effect" "non-scaling-stroke"
              ]
            []
        ]
  ;;
end

module Panel = struct
  let view ~title children =
    {%html|
      <div %{Styles.panel}>
        <div %{Styles.panel_title}>#{title}</div>
        *{children}
      </div>
    |}
  ;;
end

let metric ~label ~value =
  {%html|
    <div %{Styles.metric_row}>
      <span %{Styles.metric_label}>#{label}</span>
      <span %{Styles.metric_value}>#{value}</span>
    </div>
  |}
;;

let span_str s = Time_ns.Span.to_string_hum ~decimals:1 s

let latency_panel ~title ~color ~(l : Exchange_stats.Latency_summary.t) =
  let samples_row =
    {%html|
      <div %{Styles.metric_row}>
        <span %{Styles.metric_label}><span %{Styles.swatch color}></span>samples</span>
        <span %{Styles.metric_value}>%{l.count#Int}</span>
      </div>
    |}
  in
  Panel.view
    ~title
    [ samples_row
    ; metric ~label:"p50" ~value:(span_str l.p50)
    ; metric ~label:"p90" ~value:(span_str l.p90)
    ; metric ~label:"p99" ~value:(span_str l.p99)
    ]
;;

let memory_panel ~(d : Controller.Display.t) =
  let now_str =
    Option.value_map d.memory_now ~default:"—" ~f:Int.to_string_hum
  in
  let kb =
    Option.value_map d.memory_now ~default:"" ~f:(fun w ->
      [%string "≈ %{(w * 8 / 1024)#Int} KB"])
  in
  Panel.view
    ~title:"Process memory (live words)"
    [ {%html|
        <div>
          <span %{Styles.big_number}>#{now_str}</span>
          <span %{Styles.unit_label}>#{kb}</span>
        </div>
      |}
    ; Sparkline.view
        ~series:d.memory_series
        ~min:(Option.value d.memory_min ~default:0)
        ~max:(Option.value d.memory_max ~default:0)
    ; metric
        ~label:"window"
        ~value:
          [%string "%{d.sample_count#Int} / %{Controller.window_size#Int} s"]
    ]
;;

let activity_panel ~(d : Controller.Display.t) =
  Panel.view
    ~title:"Throughput & GC"
    [ metric
        ~label:"orders submitted /s"
        ~value:(Int.to_string d.submit_rate)
    ; metric
        ~label:"orders cancelled /s"
        ~value:(Int.to_string d.cancel_rate)
    ; metric
        ~label:"minor collections"
        ~value:(Int.to_string_hum d.minor_collections)
    ; metric
        ~label:"major collections"
        ~value:(Int.to_string_hum d.major_collections)
    ]
;;

let render (d : Controller.Display.t) =
  {%html|
    <div %{Styles.page}>
      <h1 %{Styles.header}>JSIP exchange · live diagnostics</h1>
      <div %{Styles.grid}>
        %{memory_panel ~d}
        %{latency_panel ~title:"Submit latency" ~color:Styles.submit_color ~l:d.submit_latency}
        %{latency_panel ~title:"Cancel latency" ~color:Styles.cancel_color ~l:d.cancel_latency}
        %{activity_panel ~d}
      </div>
    </div>
  |}
;;

(* --- The component: poll the server, fold into the controller, render. --- *)

(* Poll faster than the exchange's 1 Hz emit so we rarely miss a distinct
   snapshot; [Controller.apply_snapshot] drops the resulting duplicates. *)
let poll_every = Time_ns.Span.of_ms 500.

let app (local_ graph) : Vdom.Node.t Bonsai.t =
  let state, inject =
    Bonsai.state_machine
      ~default_model:Controller.empty
      ~apply_action:(fun _ctx model snapshot ->
        Controller.apply_snapshot model snapshot)
      graph
  in
  let on_response_received =
    let%arr inject in
    fun (_ : unit) (response : Exchange_stats.t option Or_error.t) ->
      match response with
      | Ok (Some snapshot) -> inject snapshot
      | Ok None | Error _ -> Effect.return ()
  in
  let (_ : Exchange_stats.t option option Bonsai.t) =
    Rpc_effect.Rpc.poll
      Dashboard_rpc.current_stats_rpc
      ~equal_query:[%equal: unit]
      ~on_response_received
      ~every:(Bonsai.return poll_every)
      ~output_type:Rpc_effect.Poll_result.Output_type.Last_ok_response
      (Bonsai.return ())
      graph
  in
  let%arr state in
  render (Controller.display state)
;;
