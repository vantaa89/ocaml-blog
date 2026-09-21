open! Core
open! Async
open! Import

let () = Log.Global.set_output []
let zone = Timezone.find_exn "Asia/Seoul"
let start_date = Date.of_string "2026-08-01"

(* Posts are ordered by [created_at], so each one gets a distinct day. *)
let post ~id ~slug ~title ~special_post : Database_schema.Post.t =
  let date = Date.add_days start_date id in
  { id
  ; title
  ; slug
  ; content_en = Some [%string "Content of %{title}"]
  ; content_ko = None
  ; author_id = 1
  ; created_at =
      Time_ns.of_date_ofday
        ~zone:(Timezone.find_exn "Asia/Seoul")
        date
        Time_ns.Ofday.start_of_day
  ; special_post
  ; hidden = false
  }
;;

let posts =
  [ post ~id:1 ~slug:"main" ~title:"main" ~special_post:true
  ; post ~id:2 ~slug:"hello-world" ~title:"Hello world" ~special_post:false
  ]
;;

let tags : Database_schema.Tag.t list = [ { id = 1; name = "OCaml"; slug = "ocaml" } ]
let post_tags : Database_schema.Post_tag.t list = [ { post_id = 2; tag_id = 1 } ]
let default_db () = Database.For_testing.create_in_memory ~posts ~tags ~post_tags ()

let with_client db ~f =
  let config : Config.t =
    { port = 0
    ; static_dir = "static"
    ; media_dir = "media"
    ; max_login_attempts = 5
    ; login_attempt_window = Time_ns.Span.of_min 15.
    }
  in
  let%bind server = Web_server.serve ~time_source:(Time_source.wall_clock ()) db config in
  let port = Cohttp_async.Server.listening_on server in
  let uri = Uri.of_string [%string "ws://127.0.0.1:%{port#Int}%{Urls.websocket_path}"] in
  let%bind connection = Rpc_websocket.Rpc.client uri >>| ok_exn in
  let%bind result = f connection in
  let%bind () = Rpc.Connection.close connection in
  let%bind () = Cohttp_async.Server.close server in
  return result
;;

let%expect_test "the post list excludes special posts" =
  let%bind response =
    with_client (default_db ()) ~f:(fun connection ->
      Rpc.Rpc.dispatch_exn
        Rpcs.Get_post_list.rpc
        connection
        { tag_slug = None; limit = None; offset = None })
  in
  print_s [%sexp (response : Rpcs.Post_summary.t list)];
  [%expect
    {|
    (((title "Hello world") (slug hello-world) (excerpt "Content of Hello world")
      (created_at "2026-08-02 15:00:00Z") (tags (((name OCaml) (slug ocaml))))
      (languages (English)))) |}];
  return ()
;;

let%expect_test "the tag list reports how many posts carry each tag" =
  let%bind response =
    with_client (default_db ()) ~f:(fun connection ->
      Rpc.Rpc.dispatch_exn Rpcs.Get_tags.rpc connection ())
  in
  print_s [%sexp (response : Rpcs.Tag_with_count.t list)];
  [%expect {| (((tag ((name OCaml) (slug ocaml))) (post_count 1))) |}];
  return ()
;;

let%expect_test "a post is fetched by slug, with its tags and languages" =
  let%bind response =
    with_client (default_db ()) ~f:(fun connection ->
      Rpc.Rpc.dispatch_exn Rpcs.Get_post.rpc connection { slug = "hello-world" })
  in
  print_s [%sexp (response : Rpcs.Post.t option)];
  [%expect
    {|
    (((title "Hello world") (slug hello-world)
      (content ((English "Content of Hello world")))
      (created_at "2026-08-02 15:00:00Z") (tags (((name OCaml) (slug ocaml))))
      (special_post false) (hidden false))) |}];
  return ()
;;

let%expect_test "search centers the excerpt on the match and ignores short queries" =
  let%bind matching =
    with_client (default_db ()) ~f:(fun connection ->
      let dispatch query =
        Rpc.Rpc.dispatch_exn Rpcs.Search_posts.rpc connection { query }
      in
      dispatch "world")
  in
  print_s [%sexp (matching : Rpcs.Post_summary.t list)];
  [%expect
    {|
    (((title "Hello world") (slug hello-world) (excerpt "Content of Hello world")
      (created_at "2026-08-02 15:00:00Z") (tags (((name OCaml) (slug ocaml))))
      (languages (English)))) |}];
  return ()
;;

let%expect_test "markdown is rendered server-side" =
  let%bind html =
    with_client (default_db ()) ~f:(fun connection ->
      Rpc.Rpc.dispatch_exn Rpcs.Render_markdown.rpc connection { markdown = "# Title" })
  in
  print_endline html;
  [%expect
    {| <h1 id="title"><a class="anchor" aria-hidden="true" href="#title"></a>Title</h1> |}];
  return ()
;;

let%expect_test
    "the main page shows the [main] post, the four newest ordinary posts, visible \
     publications and news"
  =
  let db =
    Database.For_testing.create_in_memory
      ~posts:
        (posts
         @ [ post ~id:3 ~slug:"third" ~title:"Third" ~special_post:false
           ; post ~id:4 ~slug:"fourth" ~title:"Fourth" ~special_post:false
           ; post ~id:5 ~slug:"fifth" ~title:"Fifth" ~special_post:false
           ; post ~id:6 ~slug:"sixth" ~title:"Sixth" ~special_post:false
           ; post ~id:7 ~slug:"seventh" ~title:"Seventh" ~special_post:false
           ])
      ~tags
      ~post_tags
      ~images:[ { id = 1; filename = "paper.png"; date = Date.of_string "2024-01-15" } ]
      ~publication:
        [ { id = 1
          ; title = "A paper"
          ; image_id = 1
          ; authors = "Your Name"
          ; journal = "A journal"
          ; link = Some "https://example.com/paper"
          ; hidden = false
          }
        ; { id = 2
          ; title = "A retracted paper"
          ; image_id = 1
          ; authors = "Your Name"
          ; journal = "A journal"
          ; link = None
          ; hidden = true
          }
        ]
      ~news:
        [ { id = 1; content = "Older news"; date = Date.of_string "2024-02-01" }
        ; { id = 2; content = "Newer news"; date = Date.of_string "2024-02-20" }
        ]
      ()
  in
  let%bind response =
    with_client db ~f:(fun connection ->
      Rpc.Rpc.dispatch_exn Rpcs.Get_main_page.rpc connection ())
  in
  print_s [%sexp (response : Rpcs.Get_main_page.Response.t)];
  [%expect
    {|
    ((main_post
      (((title main) (slug main) (content ((English "Content of main")))
        (created_at "2026-08-01 15:00:00Z") (tags ()) (special_post true)
        (hidden false))))
     (recent_posts
      (((title Seventh) (slug seventh) (excerpt "Content of Seventh")
        (created_at "2026-08-07 15:00:00Z") (tags ()) (languages (English)))
       ((title Sixth) (slug sixth) (excerpt "Content of Sixth")
        (created_at "2026-08-06 15:00:00Z") (tags ()) (languages (English)))
       ((title Fifth) (slug fifth) (excerpt "Content of Fifth")
        (created_at "2026-08-05 15:00:00Z") (tags ()) (languages (English)))
       ((title Fourth) (slug fourth) (excerpt "Content of Fourth")
        (created_at "2026-08-04 15:00:00Z") (tags ()) (languages (English)))))
     (publications
      (((title "A paper") (image_url /media/2024-01-15/paper.png)
        (authors "Your Name") (journal "A journal")
        (link (https://example.com/paper)))))
     (news
      (((content "Newer news") (date 2024-02-20))
       ((content "Older news") (date 2024-02-01))))) |}];
  return ()
;;
