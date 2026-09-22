open! Core
open! Async
open! Import

let cookie_name = "sessionid"

type t =
  { port : int
  ; time_source : Time_source.Read_write.t
  }

let with_server db ~f =
  let time_source =
    Time_source.create ~now:(Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z") ()
  in
  let media_dir = Filename_unix.temp_dir "media" "" in
  let%bind server =
    Web_server.serve
      ~time_source:(Time_source.read_only time_source)
      db
      { Config.default with port = 0; media_dir }
  in
  let%bind result = f { port = Cohttp_async.Server.listening_on server; time_source } in
  let%bind () = Cohttp_async.Server.close server in
  let%bind () =
    Process.run_expect_no_output_exn ~prog:"rm" ~args:[ "-rf"; media_dir ] ()
  in
  return result
;;

let advance_clock t span = Time_source.advance_by_alarms_by t.time_source span

(* The [Cookie] header a browser holding [token] would send. *)
let cookie_header token =
  Option.map token ~f:(fun token ->
    Cohttp.Cookie.Cookie_hdr.serialize [ cookie_name, token ])
  |> Option.to_list
;;

let post ?origin ?token t ~path params =
  let server = [%string "http://127.0.0.1:%{t.port#Int}"] in
  let headers =
    ("origin", Option.value origin ~default:server) :: cookie_header token
    |> Cohttp.Header.of_list
  in
  (* [post_form] sets the form content type and encodes [params] with the very function
     the server decodes them with. *)
  let%bind response, body =
    Cohttp_async.Client.post_form
      ~headers
      ~params
      (Uri.of_string [%string "%{server}%{path}"])
  in
  let%map () = Cohttp_async.Body.drain body in
  response
;;

let login ?origin ?username ?password t =
  let field name value = Option.map value ~f:(fun value -> name, [ value ]) in
  let%map response =
    [ field "username" username; field "password" password ]
    |> List.filter_opt
    |> post ?origin t ~path:Urls.login_path
  in
  let set_cookies =
    Cohttp.Cookie.Set_cookie_hdr.extract (Cohttp.Response.headers response)
  in
  let token =
    List.Assoc.find set_cookies cookie_name ~equal:String.equal
    |> Option.map ~f:Cohttp.Cookie.Set_cookie_hdr.value
  in
  response, token
;;

let logout ?token t = post ?token t ~path:Urls.logout_path []

let get t ~path =
  let%bind response, body =
    Cohttp_async.Client.get
      (Uri.of_string [%string "http://127.0.0.1:%{t.port#Int}%{path}"])
  in
  let%map body = Cohttp_async.Body.to_string body in
  response, body
;;

let with_rpc_connection ?token t ~f =
  let headers = cookie_header token |> Cohttp.Header.of_list in
  let%bind connection =
    Rpc_websocket.Rpc.client
      ~headers
      (Uri.of_string [%string "ws://127.0.0.1:%{t.port#Int}%{Urls.websocket_path}"])
    >>| ok_exn
  in
  let%bind result = f connection in
  let%map () = Rpc.Connection.close connection in
  result
;;
