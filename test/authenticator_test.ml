open! Core
open! Async
open! Import

let () = Log.Global.set_output []
let username = "author"

(* Spaces and an [&] make the round trip through [Uri] encoding part of the test. *)
let password = "my secret & password"

let author : Database_schema.User.t =
  { id = 1
  ; username
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
  ; author_id = author.id
  ; created_at = Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z"
  ; special_post = false
  ; hidden = true
  }
;;

let with_seeded_server ~f =
  Server_test_helpers.with_server
    (Database.For_testing.create_in_memory ~users:[ author ] ~posts:[ hidden_post ] ())
    ~f
;;

let print_response response =
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

let%expect_test "a correct password starts a session" =
  let%bind () =
    with_seeded_server ~f:(fun server ->
      let%map response, _token = Server_test_helpers.login ~username ~password server in
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
  with_seeded_server ~f:(fun server ->
    let%bind response, _token =
      Server_test_helpers.login ~username ~password:"not-the-password" server
    in
    print_response response;
    [%expect {| ((status 401) (set_cookie ())) |}];
    let%bind response, _token =
      Server_test_helpers.login ~username:"nobody" ~password server
    in
    print_response response;
    [%expect {| ((status 401) (set_cookie ())) |}];
    return ())
;;

let%expect_test "a form without both fields is a bad request" =
  with_seeded_server ~f:(fun server ->
    let%bind response, _token = Server_test_helpers.login ~username server in
    print_response response;
    [%expect {| ((status 400) (set_cookie ())) |}];
    let%bind response, _token = Server_test_helpers.login server in
    print_response response;
    [%expect {| ((status 400) (set_cookie ())) |}];
    return ())
;;

let%expect_test "a login from another origin is refused" =
  let%bind () =
    with_seeded_server ~f:(fun server ->
      let%map response, _token =
        Server_test_helpers.login ~origin:"http://evil.example" ~username ~password server
      in
      print_response response)
  in
  [%expect {| ((status 403) (set_cookie ())) |}];
  return ()
;;

let%expect_test "logging out clears the cookie, with or without a session" =
  with_seeded_server ~f:(fun server ->
    let%bind _response, token = Server_test_helpers.login ~username ~password server in
    let%bind response = Server_test_helpers.logout ?token server in
    print_response response;
    [%expect
      {|
        ((status 204)
         (set_cookie ("sessionid=deleted; Max-Age=0; path=/; secure; httponly"))) |}];
    let%bind response = Server_test_helpers.logout server in
    print_response response;
    [%expect
      {|
        ((status 204)
         (set_cookie ("sessionid=deleted; Max-Age=0; path=/; secure; httponly"))) |}];
    return ())
;;

let%expect_test "the session cookie names the user over RPC, until logout" =
  with_seeded_server ~f:(fun server ->
    let print_current_user ?token () =
      Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
        let%map user = Rpc.Rpc.dispatch_exn Rpcs.Get_current_user.rpc connection () in
        print_s [%sexp (user : Rpcs.Get_current_user.Response.t)])
    in
    let%bind _response, token = Server_test_helpers.login ~username ~password server in
    let%bind () = print_current_user ?token () in
    [%expect {| (Logged_in (username author)) |}];
    (* A connection without the cookie is anonymous. *)
    let%bind () = print_current_user () in
    [%expect {| Not_logged_in |}];
    let%bind (_ : Cohttp.Response.t) = Server_test_helpers.logout ?token server in
    (* The same token no longer names a session. *)
    let%bind () = print_current_user ?token () in
    [%expect {| Not_logged_in |}];
    return ())
;;

let%expect_test "a hidden post is visible only to its author" =
  with_seeded_server ~f:(fun server ->
    let print_draft ?token () =
      Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
        let%map post =
          Rpc.Rpc.dispatch_exn Rpcs.Get_post.rpc connection { slug = hidden_post.slug }
        in
        print_s
          [%sexp (Option.map post ~f:(fun post -> post.Rpcs.Post.title) : string option)])
    in
    (* Knowing the slug is not enough. *)
    let%bind () = print_draft () in
    [%expect {| () |}];
    let%bind _response, token = Server_test_helpers.login ~username ~password server in
    let%bind () = print_draft ?token () in
    [%expect {| (Draft) |}];
    return ())
;;

let%expect_test "repeated failures lock a username out" =
  with_seeded_server ~f:(fun server ->
    let wrong_password () =
      let%map response, _token =
        Server_test_helpers.login ~username ~password:"not-the-password" server
      in
      print_response response
    in
    let%bind () =
      List.init Config.default.max_login_attempts ~f:Fn.id
      |> Deferred.List.iter ~how:`Sequential ~f:(fun _ -> wrong_password ())
    in
    [%expect
      {|
      ((status 401) (set_cookie ()))
      ((status 401) (set_cookie ()))
      ((status 401) (set_cookie ()))
      ((status 401) (set_cookie ()))
      ((status 401) (set_cookie ())) |}];
    (* Next attempt never reaches the password check. *)
    let%bind () = wrong_password () in
    [%expect {| ((status 429) (set_cookie ())) |}];
    (* Even the right password is turned away while the count stands. *)
    let%bind response, _token = Server_test_helpers.login ~username ~password server in
    print_response response;
    [%expect {| ((status 429) (set_cookie ())) |}];
    (* Another name has its own count. *)
    let%bind response, _token =
      Server_test_helpers.login ~username:"nobody" ~password:"not-the-password" server
    in
    print_response response;
    [%expect {| ((status 401) (set_cookie ())) |}];
    (* Once the failures have aged out of the window, the name is free again. An attempt
       is swept only once it is strictly older than the window, hence the extra second. *)
    let%bind () =
      Server_test_helpers.advance_clock
        server
        Time_ns.Span.(Config.default.login_attempt_window + of_sec 1.)
    in
    let%bind response, _token = Server_test_helpers.login ~username ~password server in
    print_response response;
    [%expect
      {|
      ((status 204)
       (set_cookie
        ("sessionid=<token of 43 chars>; Max-Age=1209600; path=/; secure; httponly"))) |}];
    return ())
;;
