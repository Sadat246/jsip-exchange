(** Browser entry point: mount the dashboard into the [#app] div.

    Compiled to [main.bc.js] by [js_of_ocaml] (see [dune]). [Start.start]
    defaults to binding the element with id ["app"], which [index.html]
    provides, and to connecting RPCs back to the server that served the page. *)

let () = Bonsai_web.Start.start Jsip_dashboard.View.app
