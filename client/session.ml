open! Core
open! Import
open Bonsai.Let_syntax

let reload_home =
  Effect.of_sync_fun (fun () ->
    Js_of_ocaml.Dom_html.window##.location##.href := Js_of_ocaml.Js.string "/")
;;

(* [Http.request] reports every non-2xx status as an error, so rejected credentials and a
   network failure arrive here the same way. *)
let post =
  Effect.of_deferred_fun (fun (url, body) ->
    let%map.Async_kernel.Deferred response =
      Async_js.Http.request
        ~headers:[ "content-type", "application/x-www-form-urlencoded" ]
        ~url
        ~response_type:Default
        (Post (Some (String body)))
    in
    Or_error.map response ~f:(fun (_ : string Async_js.Http.Response.t) -> ()))
;;

let log_in ~username ~password =
  let body =
    Uri.encoded_of_query [ "username", [ username ]; "password", [ password ] ]
  in
  post (Urls.login_path, body)
;;

let log_out () = post (Urls.logout_path, "")

let current_user =
  let%sub poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Unit)
      (module Rpcs.Get_current_user.Response)
      Rpcs.Get_current_user.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      (Value.return ())
  in
  let%arr poll = poll in
  match poll.last_ok_response with
  | None -> Rpcs.Get_current_user.Response.Not_logged_in
  | Some ((), response) -> response
;;
