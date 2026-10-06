open! Core
open! Import

module Url_route = struct
  include Route

  let parse_exn (components : Bonsai_web_ui_url_var.Components.t) =
    of_url ~path:components.path ~query:components.query
  ;;

  let unparse t =
    let path, query = to_url t in
    Bonsai_web_ui_url_var.Components.create ~path ~query ()
  ;;
end

let var = Bonsai_web_ui_url_var.create_exn (module Url_route) ~fallback:Home
let current = Bonsai_web_ui_url_var.value var
let go_to route = Bonsai_web_ui_url_var.set_effect var route
