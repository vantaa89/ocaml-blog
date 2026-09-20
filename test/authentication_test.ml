open! Core
open! Async
open! Import
module Set_cookie_hdr = Cohttp.Cookie.Set_cookie_hdr

(* A rejected login logs its reason, which would otherwise land in the expect output. *)
let () = Log.Global.set_output []
let cookie_name = "sessionid"

(* Spaces and an [&] make the round trip through [Uri] encoding part of the test. *)
let password = "my secret & password"

let user : Database_schema.User.t =
  { id = 1
  ; username = "author"
  ; email = "author@example.com"
  ; password_hash = Password.hash_exn password
  ; date_joined = Date.of_string "2026-08-01"
  ; last_login = None
  }
;;

let hidden_post : Database_schema.Post.t =
  { id = 1
  ; title = "Draft"
  ; slug = "draft"
  ; content_en = Some "Not ready yet"
  ; content_ko = None
  ; author_id = user.id
  ; created_at = Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z"
  ; special_post = false
  ; hidden = true
  }
;;

let with_server ~f =
  let db =
    Database.For_testing.create_in_memory ~users:[ user ] ~posts:[ hidden_post ] ()
  in
  let config : Config.t = { port = 0; static_dir = "static"; media_dir = "media" } in
  let%bind server = Web_server.serve db config in
  let port = Cohttp_async.Server.listening_on server in
  let%bind result = f ~port in
  let%bind () = Cohttp_async.Server.close server in
  return result
;;

(* The [Cookie] header a browser holding [token] would send. *)
let cookie_header token =
  Option.map token ~f:(fun token ->
    Cohttp.Cookie.Cookie_hdr.serialize [ cookie_name, token ])
  |> Option.to_list
;;

let post ~port ~path ?origin ?token params =
  let server = [%string "http://127.0.0.1:%{port#Int}"] in
  let headers =
    ("origin", Option.value origin ~default:server) :: cookie_header token
    |> Cohttp.Header.of_list
  in
  (* [post_form] sets the form content type and encodes [params] with the very function
     the server decodes them with. *)
  Cohttp_async.Client.post_form
    ~headers
    ~params
    (Uri.of_string [%string "%{server}%{path}"])
;;

let login ~port = post ~port ~path:Urls.login_path
let logout ~port ?token () = post ~port ~path:Urls.logout_path ?token []

let credentials ~username ~password =
  [ "username", [ username ]; "password", [ password ] ]
;;

let print_response (response, body) =
  let%map () = Cohttp_async.Body.drain body in
  let set_cookie =
    Cohttp.Header.get (Cohttp.Response.headers response) "set-cookie"
    |> Option.map ~f:(fun set_cookie ->
      (* The token is random, so use its length instead except the case of "deleted" *)
      let cookie, attributes = String.lsplit2_exn set_cookie ~on:';' in
      let name, value = String.lsplit2_exn cookie ~on:'=' in
      match String.equal value "deleted" with
      | true -> set_cookie
      | false ->
        let length = String.length value in
        [%string "%{name}=<token of %{length#Int} chars>;%{attributes}"])
  in
  print_s
    [%message
      ""
        ~status:(Cohttp.Response.status response |> Cohttp.Code.code_of_status : int)
        (set_cookie : string option)]
;;

(* Logs in with the right credentials and returns the token the browser would keep. *)
let start_session ~port =
  let%bind response, body = login ~port (credentials ~username:"author" ~password) in
  let%bind () = Cohttp_async.Body.drain body in
  let set_cookies = Set_cookie_hdr.extract (Cohttp.Response.headers response) in
  List.Assoc.find_exn set_cookies cookie_name ~equal:String.equal
  |> Set_cookie_hdr.value
  |> return
;;

let%expect_test "a correct password starts a session" =
  let%bind () =
    with_server ~f:(fun ~port ->
      let%bind response = login ~port (credentials ~username:"author" ~password) in
      print_response response)
  in
  [%expect
    {|
    ((status 204)
     (set_cookie
      ("sessionid=<token of 43 chars>; Max-Age=1209600; path=/; secure; httponly"))) |}];
  return ()
;;

let%expect_test "a wrong password and an unknown user are rejected alike" =
  with_server ~f:(fun ~port ->
    let%bind response =
      login ~port (credentials ~username:"author" ~password:"not-the-password")
    in
    let%bind () = print_response response in
    [%expect {| ((status 401) (set_cookie ())) |}];
    let%bind response = login ~port (credentials ~username:"nobody" ~password) in
    let%bind () = print_response response in
    [%expect {| ((status 401) (set_cookie ())) |}];
    return ())
;;

let%expect_test "a form without both fields is a bad request" =
  with_server ~f:(fun ~port ->
    let%bind response = login ~port [ "username", [ "author" ] ] in
    let%bind () = print_response response in
    [%expect {| ((status 400) (set_cookie ())) |}];
    let%bind response = login ~port [] in
    let%bind () = print_response response in
    [%expect {| ((status 400) (set_cookie ())) |}];
    return ())
;;

let%expect_test "a login from another origin is refused" =
  let%bind () =
    with_server ~f:(fun ~port ->
      let%bind response =
        login
          ~port
          ~origin:"http://evil.example"
          (credentials ~username:"author" ~password)
      in
      print_response response)
  in
  [%expect {| ((status 403) (set_cookie ())) |}];
  return ()
;;

let%expect_test "logging out clears the cookie, with or without a session" =
  with_server ~f:(fun ~port ->
    let%bind token = start_session ~port in
    let%bind response = logout ~port ~token () in
    let%bind () = print_response response in
    [%expect
      {|
        ((status 204)
         (set_cookie ("sessionid=deleted; Max-Age=0; path=/; secure; httponly"))) |}];
    let%bind response = logout ~port () in
    let%bind () = print_response response in
    [%expect
      {|
        ((status 204)
         (set_cookie ("sessionid=deleted; Max-Age=0; path=/; secure; httponly"))) |}];
    return ())
;;

let%expect_test "the session cookie names the user over RPC, until logout" =
  with_server ~f:(fun ~port ->
    let print_current_user ?token () =
      let headers = cookie_header token |> Cohttp.Header.of_list in
      let%bind connection =
        Rpc_websocket.Rpc.client
          ~headers
          (Uri.of_string [%string "ws://127.0.0.1:%{port#Int}%{Urls.websocket_path}"])
        >>| ok_exn
      in
      let%bind user = Rpc.Rpc.dispatch_exn Rpcs.Get_current_user.rpc connection () in
      let%map () = Rpc.Connection.close connection in
      print_s [%sexp (user : Rpcs.Get_current_user.Response.t)]
    in
    let%bind token = start_session ~port in
    let%bind () = print_current_user ~token () in
    [%expect {| (Logged_in (username author)) |}];
    (* A connection without the cookie is anonymous. *)
    let%bind () = print_current_user () in
    [%expect {| Not_logged_in |}];
    let%bind _response, body = logout ~port ~token () in
    let%bind () = Cohttp_async.Body.drain body in
    (* The same token no longer names a session. *)
    let%bind () = print_current_user ~token () in
    [%expect {| Not_logged_in |}];
    return ())
;;

let%expect_test "a hidden post is visible only to its author" =
  with_server ~f:(fun ~port ->
    let print_draft ?token () =
      let headers = cookie_header token |> Cohttp.Header.of_list in
      let%bind connection =
        Rpc_websocket.Rpc.client
          ~headers
          (Uri.of_string [%string "ws://127.0.0.1:%{port#Int}%{Urls.websocket_path}"])
        >>| ok_exn
      in
      let%bind post =
        Rpc.Rpc.dispatch_exn Rpcs.Get_post.rpc connection { slug = hidden_post.slug }
      in
      let%map () = Rpc.Connection.close connection in
      print_s
        [%sexp (Option.map post ~f:(fun post -> post.Rpcs.Post.title) : string option)]
    in
    (* Knowing the slug is not enough. *)
    let%bind () = print_draft () in
    [%expect {| () |}];
    let%bind token = start_session ~port in
    let%bind () = print_draft ~token () in
    [%expect {| (Draft) |}];
    return ())
;;
