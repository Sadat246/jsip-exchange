open! Core
open! Bonsai_web

(* This project has no [ppx_css], so styles are plain inline "style" strings.
   Keeping every color and measurement as a named token here (rather than
   scattering literals through the view) is what keeps the panes visually
   consistent — the same discipline a [ppx_css] stylesheet would enforce. *)

let bg = "#0d1117"
let panel_bg = "#161b22"
let border = "#30363d"
let text = "#e6edf3"
let text_dim = "#8b949e"
let accent = "#58a6ff"
let submit_color = "#3fb950"
let cancel_color = "#f778ba"
let warn = "#d29922"

(* Turn a raw CSS declaration string into an inline-style attribute. *)
let style s = Vdom.Attr.create "style" s

let page =
  style
    [%string
      "background:%{bg}; color:%{text}; min-height:100vh; margin:0; \
       padding:24px; font-family:'SF Mono',ui-monospace,Menlo,monospace; \
       box-sizing:border-box"]
;;

let header =
  style
    "font-size:18px; font-weight:600; margin:0 0 16px 0; \
     letter-spacing:0.02em"
;;

let grid =
  style
    "display:grid; \
     grid-template-columns:repeat(auto-fit,minmax(320px,1fr)); gap:16px"
;;

let panel =
  style
    [%string
      "background:%{panel_bg}; border:1px solid %{border}; \
       border-radius:8px; padding:16px"]
;;

let panel_title =
  style
    [%string
      "font-size:12px; text-transform:uppercase; letter-spacing:0.08em; \
       color:%{text_dim}; margin:0 0 12px 0"]
;;

let big_number =
  style "font-size:28px; font-weight:600; font-variant-numeric:tabular-nums"
;;

let unit_label =
  style [%string "font-size:13px; color:%{text_dim}; margin-left:6px"]
;;

(* A row in a small label/value table. *)
let metric_row =
  style
    "display:flex; justify-content:space-between; align-items:baseline; \
     padding:4px 0; font-variant-numeric:tabular-nums"
;;

let metric_label = style [%string "color:%{text_dim}; font-size:13px"]
let metric_value = style "font-size:15px; font-weight:500"

let swatch color =
  style
    [%string
      "display:inline-block; width:9px; height:9px; border-radius:2px; \
       background:%{color}; margin-right:7px"]
;;
