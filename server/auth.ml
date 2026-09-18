open! Core
open! Async
open! Import

(* TODO: Implement session-cookie authentication. *)
let not_implemented () =
  Cohttp_async.Server.respond_string ~status:`Not_implemented "Not implemented"
;;

let handle_login _db (_config : Config.t) ~body:_ _request = not_implemented ()
let handle_logout _db (_config : Config.t) _request = not_implemented ()
