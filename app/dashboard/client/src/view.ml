open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Jsip_dashboard_protocol
module Controller = Jsip_dashboard_controller.Controller

(* --- Leaf views: pure [Vdom.Node.t], no graph, no state. --- *)

module Chart = struct
  (* viewBox coordinate space; CSS stretches it to the panel width. *)
  let width = 300.
  let height = 60.
  let attr = Vdom.Attr.create

  let placeholder text =
    let style =
      Styles.style
        [%string
          "height:%{height#Float}px; display:flex; align-items:center; \
           color:%{Styles.text_dim}; font-size:13px"]
    in
    {%html|<div %{style}>#{text}</div>|}
  ;;

  let polyline ~color ~points =
    Vdom.Node.create_svg
      "polyline"
      ~attrs:
        [ attr "points" points
        ; attr "fill" "none"
        ; attr "stroke" color
        ; attr "stroke-width" "2"
        ; attr "vector-effect" "non-scaling-stroke"
        ]
      []
  ;;

  (* [lines]: (colour, values oldest->newest). Every line shares one y-scale
     computed across all of them, so they are directly comparable — p99 sits
     visibly above p50, submit above cancel. *)
  let view ~(lines : (string * float list) list) =
    let all = List.concat_map lines ~f:snd in
    let n =
      List.map lines ~f:(fun (_, s) -> List.length s)
      |> List.max_elt ~compare:Int.compare
      |> Option.value ~default:0
    in
    if n < 2
    then placeholder "collecting…"
    else (
      let lo =
        Option.value (List.min_elt all ~compare:Float.compare) ~default:0.
      in
      let hi =
        Option.value (List.max_elt all ~compare:Float.compare) ~default:0.
      in
      let range = Float.max 1. (hi -. lo) in
      let points series =
        List.mapi series ~f:(fun i v ->
          let x = Float.of_int i /. Float.of_int (n - 1) *. width in
          let y = height -. ((v -. lo) /. range *. height) in
          (* fixed precision avoids [Float.to_string]'s trailing-dot forms
             ("0." / "300.") that some SVG parsers reject *)
          Printf.sprintf "%.2f,%.2f" x y)
        |> String.concat ~sep:" "
      in
      Vdom.Node.create_svg
        "svg"
        ~attrs:
          [ attr "viewBox" [%string "0 0 %{width#Float} %{height#Float}"]
          ; attr "preserveAspectRatio" "none"
          ; attr "width" "100%"
          ; attr "height" (Printf.sprintf "%.0f" height)
          ; Styles.style "display:block"
          ]
        (List.map lines ~f:(fun (color, series) ->
           polyline ~color ~points:(points series))))
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

(* A row of coloured swatches naming the lines in a multi-line chart. *)
let legend items =
  let one (color, label) =
    {%html|
      <span %{Styles.legend_item}>
        <span %{Styles.swatch color}></span>#{label}
      </span>
    |}
  in
  {%html|<div %{Styles.legend_row}>*{List.map items ~f:one}</div>|}
;;

let span_str s = Time_ns.Span.to_string_hum ~decimals:1 s
let floats ints = List.map ints ~f:Float.of_int

(* p50/p90/p99 over the window, in microseconds, as three chartable lines. *)
let latency_lines (series : Exchange_stats.Latency_summary.t list) =
  let us f =
    List.map series ~f:(fun (l : Exchange_stats.Latency_summary.t) ->
      Time_ns.Span.to_us (f l))
  in
  [ Styles.text_dim, us (fun l -> l.p50)
  ; Styles.accent, us (fun l -> l.p90)
  ; Styles.warn, us (fun l -> l.p99)
  ]
;;

let latency_panel
  ~title
  ~color
  ~(l : Exchange_stats.Latency_summary.t)
  ~series
  =
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
    ; Chart.view ~lines:(latency_lines series)
    ; legend
        [ Styles.text_dim, "p50"; Styles.accent, "p90"; Styles.warn, "p99" ]
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
  let delta_node =
    Option.map d.memory_delta ~f:(fun delta ->
      let color =
        if delta > 50_000
        then Styles.warn
        else if delta < 0
        then Styles.submit_color
        else Styles.text_dim
      in
      let sign = if delta > 0 then "+" else "" in
      {%html|
        <span %{Styles.delta color}>Δ60s #{sign}#{Int.to_string_hum delta}</span>
      |})
  in
  Panel.view
    ~title:"Process memory (live words)"
    [ {%html|
        <div>
          <span %{Styles.big_number}>#{now_str}</span>
          <span %{Styles.unit_label}>#{kb}</span>
          ?{delta_node}
        </div>
      |}
    ; Chart.view ~lines:[ Styles.accent, floats d.memory_series ]
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
    ; Chart.view
        ~lines:
          [ Styles.submit_color, floats d.submit_rate_series
          ; Styles.cancel_color, floats d.cancel_rate_series
          ]
    ; legend
        [ Styles.submit_color, "submit/s"; Styles.cancel_color, "cancel/s" ]
    ]
;;

let render ~banner (d : Controller.Display.t) =
  {%html|
    <div %{Styles.page}>
      ?{banner}
      <h1 %{Styles.header}>JSIP exchange · live diagnostics</h1>
      <div %{Styles.grid}>
        %{memory_panel ~d}
        %{latency_panel ~title:"Submit latency" ~color:Styles.submit_color
            ~l:d.submit_latency ~series:d.submit_latency_series}
        %{latency_panel ~title:"Cancel latency" ~color:Styles.cancel_color
            ~l:d.cancel_latency ~series:d.cancel_latency_series}
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
  (* One connection back to the serving origin, shared by the poll and the
     status indicator. [Retry_until_success] makes the browser auto-reconnect
     if the bridge restarts. *)
  let where_to_connect =
    Bonsai.return
      (Rpc_effect.Where_to_connect.self
         ~on_conn_failure:Rpc_effect.On_conn_failure.Retry_until_success
         ())
  in
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
      ~where_to_connect
      ~every:(Bonsai.return poll_every)
      ~output_type:Rpc_effect.Poll_result.Output_type.Last_ok_response
      (Bonsai.return ())
      graph
  in
  let status = Rpc_effect.Status.state ~where_to_connect graph in
  let%arr state and status in
  let banner =
    match (status.state : Rpc_effect.Status.State.t) with
    | Connected -> None
    | Connecting ->
      Some
        {%html|<div %{Styles.banner Styles.warn}>connecting to exchange…</div>|}
    | Disconnected _ | Failed_to_connect _ ->
      Some
        {%html|
          <div %{Styles.banner Styles.cancel_color}>
            disconnected — is the dashboard server running?
          </div>
        |}
  in
  render ~banner (Controller.display state)
;;
