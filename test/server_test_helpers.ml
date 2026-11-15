open! Core
open! Async
open! Import

let cookie_name = "sessionid"
let password = "password"

let user ~id ~username : Database_schema.User.t =
  { id
  ; username
  ; email = "user@example.com"
  ; password_hash = Password.hash_exn password
  ; date_joined = Date.of_string "2026-08-01"
  ; last_login = None
  }
;;

type t =
  { port : int
  ; time_source : Time_source.Read_write.t
  }

module Credential = struct
  type t =
    [ `Api_token of string
    | `Session_token of string
    ]
end

let with_server db ~f =
  let time_source =
    Time_source.create ~now:(Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z") ()
  in
  let media_dir = Filename_unix.temp_dir "media" "" in
  let%bind server =
    Web_server.serve
      ~time_source:(Time_source.read_only time_source)
      db
      { Config.default with
        port = 0
      ; (* Tests run in [_build/default/test] *)
        static_dir = "../static"
      ; media_dir
      }
  in
  let%bind result = f { port = Cohttp_async.Server.listening_on server; time_source } in
  let%bind () = Cohttp_async.Server.close server in
  let%bind () =
    Process.run_expect_no_output_exn ~prog:"rm" ~args:[ "-rf"; media_dir ] ()
  in
  return result
;;

let advance_clock t span = Time_source.advance_by_alarms_by t.time_source span

let uri ?(scheme = "http") ?(path = "") t =
  Uri.with_uri
    ~scheme:(Some scheme)
    ~host:(Some "127.0.0.1")
    ~port:(Some t.port)
    (Uri.of_string path)
;;

let credential_headers credential =
  match credential with
  | None -> []
  | Some (`Api_token api_token) -> [ "authorization", [%string "Bearer %{api_token}"] ]
  | Some (`Session_token token) ->
    [ Cohttp.Cookie.Cookie_hdr.serialize [ cookie_name, token ] ]
;;

let post ?origin ?credential t ~path params =
  let headers =
    ("origin", Option.value origin ~default:(Uri.to_string (uri t)))
    :: credential_headers credential
    |> Cohttp.Header.of_list
  in
  (* [post_form] sets the form content type and encodes [params] with the very function
     the server decodes them with. *)
  let%bind response, body =
    Cohttp_async.Client.post_form ~headers ~params (uri t ~path)
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
  let credential =
    List.Assoc.find set_cookies cookie_name ~equal:String.equal
    |> Option.map ~f:(fun cookie ->
      `Session_token (Cohttp.Cookie.Set_cookie_hdr.value cookie))
  in
  response, credential
;;

let logout ?credential t = post ?credential t ~path:Urls.logout_path []

let get ?credential t ~path =
  let%bind response, body =
    Cohttp_async.Client.get
      ~headers:(credential_headers credential |> Cohttp.Header.of_list)
      (uri t ~path)
  in
  let%map body = Cohttp_async.Body.to_string body in
  response, body
;;

let put ?credential t ~path ~body =
  let%bind response, body =
    Cohttp_async.Client.put
      ~headers:(credential_headers credential |> Cohttp.Header.of_list)
      ~body:(Cohttp_async.Body.of_string body)
      (uri t ~path)
  in
  let%map body = Cohttp_async.Body.to_string body in
  response, body
;;

let with_rpc_connection ?credential t ~f =
  let headers = credential_headers credential |> Cohttp.Header.of_list in
  let%bind connection =
    Rpc_websocket.Rpc.client ~headers (uri ~scheme:"ws" t ~path:Urls.websocket_path)
    >>| ok_exn
  in
  let%bind result = f connection in
  let%map () = Rpc.Connection.close connection in
  result
;;
