open! Core
open! Import

let var = Bonsai_web_ui_url_var.create_exn (module Route) ~fallback:Route.Home
let current = Bonsai_web_ui_url_var.value var
let go_to route = Bonsai_web_ui_url_var.set_effect var route
