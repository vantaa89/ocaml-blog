open! Core
open! Import
open Bonsai.Let_syntax

let reload_home =
  Effect.of_sync_fun (fun () ->
    Js_of_ocaml.Dom_html.window##.location##.href := Js_of_ocaml.Js.string "/")
;;

(* [Async_js.Http] turns every non-2xx status into an [Error.t] that records the status
   only inside a message, so the request is made by hand here: a rejected password and a
   username that has run out of attempts must not read alike. A status of 0 is what the
   browser reports when the request never completed. *)
let post =
  Effect.of_deferred_fun (fun (url, body) ->
    let open Js_of_ocaml in
    let open Async_kernel in
    let request = XmlHttpRequest.create () in
    let status = Ivar.create () in
    request##_open (Js.string "POST") (Js.string url) Js._true;
    request##setRequestHeader
      (Js.string "content-type")
      (Js.string "application/x-www-form-urlencoded");
    request##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match request##.readyState with
         | DONE -> Ivar.fill_if_empty status request##.status
         | UNSENT | OPENED | HEADERS_RECEIVED | LOADING -> ());
    request##send (Js.some (Js.string body));
    Ivar.read status)
;;

let unexpected ~what ~status =
  Error.create_s [%message "Unexpected response" (what : string) (status : int)]
;;

let log_in ~username ~password =
  let body =
    Uri.encoded_of_query [ "username", [ username ]; "password", [ password ] ]
  in
  let%map.Effect status = post (Urls.login_path, body) in
  match status with
  | 204 -> `Logged_in
  | 401 -> `Rejected
  | 429 -> `Too_many_attempts
  | 0 -> `Failed (Error.of_string "Could not reach the server")
  | status -> `Failed (unexpected ~what:"a login" ~status)
;;

let log_out () =
  let%map.Effect status = post (Urls.logout_path, "") in
  match status with
  | 204 -> Ok ()
  | status -> Error (unexpected ~what:"a logout" ~status)
;;

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
