open! Core
open! Async
open! Import

let () = Log.Global.set_output []
let author = Server_test_helpers.user ~id:1 ~username:"author"
let username = author.username
let password = Server_test_helpers.password
let other_user = Server_test_helpers.user ~id:2 ~username:"other"
let now = Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z"

let post ~id ~slug ~(author : Database_schema.User.t) : Database_schema.Post.t =
  { id
  ; title = String.capitalize slug
  ; slug
  ; content_en = Some "<p>old</p>"
  ; content_ko = None
  ; author_id = author.id
  ; created_at = now
  ; special_post = false
  ; hidden = false
  }
;;

let cv_document =
  {|<!DOCTYPE html><html><head><title>CV</title></head><body><h2>Education</h2></body></html>|}
;;

let with_api_server ?(posts = [ post ~id:1 ~slug:"about" ~author ]) f =
  let db =
    Database.For_testing.create_in_memory ~users:[ author; other_user ] ~posts ()
  in
  let%bind api_token =
    Api.create_token ~db ~label:"cv" ~user_id:author.id ~now |> Deferred.Or_error.ok_exn
  in
  Server_test_helpers.with_server db ~f:(fun server ->
    f ~db ~server ~credential:(`Api_token (Token.to_string api_token)))
;;

let print_response (response, body) =
  print_s
    [%message
      ""
        ~status:(Cohttp.Response.status response |> Cohttp.Code.code_of_status : int)
        ~www_authenticate:
          (Cohttp.Header.get (Cohttp.Response.headers response) "www-authenticate"
           : string option)
        (body : string)]
;;

let print_post ~db ~slug =
  let%map post = Database.Post.find_by_slug db ~slug |> Deferred.Or_error.ok_exn in
  let ({ title; content_en; content_ko; _ } : Database_schema.Post.t) =
    Option.value_exn post
  in
  print_s
    [%message
      "" (title : string) (content_en : string option) (content_ko : string option)]
;;

let%expect_test "the API refuses a missing, wrong, or revoked token, and a session cookie"
  =
  with_api_server (fun ~db ~server ~credential ->
    let put ?credential () =
      Server_test_helpers.put ?credential server ~path:"/api/about" ~body:cv_document
      >>| print_response
    in
    let%bind () = put () in
    [%expect
      {|
      ((status 401) (www_authenticate (Bearer))
       (body "Expected a valid API token")) |}];
    let%bind () = put ~credential:(`Api_token "not-the-token") () in
    [%expect
      {|
      ((status 401) (www_authenticate (Bearer))
       (body "Expected a valid API token")) |}];
    (* Session cookies are for browsers only and do not grant access to APIs *)
    let%bind _response, session = Server_test_helpers.login ~username ~password server in
    let%bind () = put ~credential:(Option.value_exn session) () in
    [%expect
      {|
      ((status 401) (www_authenticate (Bearer))
       (body "Expected a valid API token")) |}];
    let%bind () = Database.Api_token.delete db ~label:"cv" |> Deferred.Or_error.ok_exn in
    let%bind () = put ~credential () in
    [%expect
      {|
      ((status 401) (www_authenticate (Bearer))
       (body "Expected a valid API token")) |}];
    let%bind () = print_post ~db ~slug:"about" in
    [%expect {| ((title About) (content_en (<p>old</p>)) (content_ko ())) |}];
    return ())
;;

let%expect_test "/api/about keeps the body of an HTML document as the English about" =
  with_api_server (fun ~db ~server ~credential ->
    let%bind () =
      Server_test_helpers.put ~credential server ~path:"/api/about" ~body:cv_document
      >>| print_response
    in
    [%expect {| ((status 204) (www_authenticate ()) (body "")) |}];
    let%bind () = print_post ~db ~slug:"about" in
    [%expect {| ((title About) (content_en (<h2>Education</h2>)) (content_ko ())) |}];
    return ())
;;

let%expect_test "/api/about finds the <body> as an HTML parser would" =
  with_api_server (fun ~db ~server ~credential ->
    let%bind () =
      Server_test_helpers.put
        ~credential
        server
        ~path:"/api/about"
        ~body:{|<html><BODY class="cv"><h2>Skills</h2><p>a &lt; b</p></BODY></html>|}
      >>| print_response
    in
    [%expect {| ((status 204) (www_authenticate ()) (body "")) |}];
    let%bind () = print_post ~db ~slug:"about" in
    [%expect
      {|
      ((title About) (content_en ("<h2>Skills</h2><p>a &lt; b</p>"))
       (content_ko ())) |}];
    return ())
;;

let%expect_test "/api/about is refused without a <body>, or the user does not own about" =
  let put_about ?posts body =
    with_api_server ?posts (fun ~db:_ ~server ~credential ->
      Server_test_helpers.put ~credential server ~path:"/api/about" ~body
      >>| print_response)
  in
  let%bind () = put_about "<p>new</p>" in
  [%expect
    {|
    ((status 400) (www_authenticate ())
     (body "Expected an HTML document with a <body>")) |}];
  let%bind () = put_about ~posts:[] cv_document in
  [%expect {| ((status 404) (www_authenticate ()) (body "There is no about post")) |}];
  let%bind () =
    put_about ~posts:[ post ~id:1 ~slug:"about" ~author:other_user ] cv_document
  in
  [%expect
    {|
    ((status 403) (www_authenticate ())
     (body "The about post belongs to another user")) |}];
  return ()
;;

let%expect_test "a path that is not an endpoint gets the site's 404 page" =
  let print_status server ~credential ~path =
    let%map response, (_ : string) =
      Server_test_helpers.put ~credential server ~path ~body:cv_document
    in
    print_s [%sexp (Cohttp.Response.status response |> Cohttp.Code.code_of_status : int)]
  in
  with_api_server (fun ~db:_ ~server ~credential ->
    let%bind () = print_status server ~credential ~path:"/api/about/en" in
    [%expect {| 404 |}];
    let%map () = print_status server ~credential ~path:"/api/post/about/en" in
    [%expect {| 404 |}])
;;

let%expect_test "a PDF becomes /media/cv.pdf, and anything else is refused" =
  with_api_server (fun ~db:_ ~server ~credential ->
    let%bind () =
      Server_test_helpers.put ~credential server ~path:"/api/cv" ~body:"<p>not a pdf</p>"
      >>| print_response
    in
    [%expect {| ((status 415) (www_authenticate ()) (body "Expected a PDF")) |}];
    let%bind () =
      Server_test_helpers.put ~credential server ~path:"/api/cv" ~body:"%PDF-1.7 the cv"
      >>| print_response
    in
    [%expect {| ((status 204) (www_authenticate ()) (body "")) |}];
    let%bind () =
      Server_test_helpers.get server ~path:"/media/cv.pdf" >>| print_response
    in
    [%expect {| ((status 200) (www_authenticate ()) (body "%PDF-1.7 the cv")) |}];
    return ())
;;

let%expect_test "/api/cv is refused when the user does not own about" =
  let%bind () =
    with_api_server ~posts:[] (fun ~db:_ ~server ~credential ->
      let%bind () =
        Server_test_helpers.put ~credential server ~path:"/api/cv" ~body:"%PDF-1.7"
        >>| print_response
      in
      [%expect {| ((status 404) (www_authenticate ()) (body "There is no about post")) |}];
      let%map () =
        Server_test_helpers.get server ~path:"/media/cv.pdf" >>| print_response
      in
      [%expect {| ((status 404) (www_authenticate ()) (body "File not found")) |}])
  in
  with_api_server
    ~posts:[ post ~id:1 ~slug:"about" ~author:other_user ]
    (fun ~db:_ ~server ~credential ->
       let%bind () =
         Server_test_helpers.put ~credential server ~path:"/api/cv" ~body:"%PDF-1.7"
         >>| print_response
       in
       [%expect
         {|
           ((status 403) (www_authenticate ())
            (body "The about post belongs to another user")) |}];
       let%map () =
         Server_test_helpers.get server ~path:"/media/cv.pdf" >>| print_response
       in
       [%expect {| ((status 404) (www_authenticate ()) (body "File not found")) |}])
;;

let%expect_test "an overlong body is refused" =
  let%bind () =
    with_api_server (fun ~db:_ ~server ~credential ->
      Server_test_helpers.put
        ~credential
        server
        ~path:"/api/cv"
        ~body:("%PDF-" ^ String.make (10 * 1024 * 1024) 'a')
      >>| print_response)
  in
  [%expect {| ((status 413) (www_authenticate ()) (body "The body is too large")) |}];
  return ()
;;
