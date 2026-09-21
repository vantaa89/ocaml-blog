open! Core
open! Async
open! Import

let () = Log.Global.set_output []
let zone = Timezone.find_exn "Asia/Seoul"
let start_date = Date.of_string "2026-08-01"
let password = "password"

let author : Database_schema.User.t =
  { id = 1
  ; username = "author"
  ; email = "author@example.com"
  ; password_hash = Password.hash_exn password
  ; date_joined = start_date
  ; last_login = None
  }
;;

(* Posts are ordered by [created_at], so each one gets a distinct day. *)
let post ~id ~slug ~title ~special_post : Database_schema.Post.t =
  let date = Date.add_days start_date id in
  { id
  ; title
  ; slug
  ; content_en = Some [%string "Content of %{title}"]
  ; content_ko = None
  ; author_id = author.id
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

let with_seeded_server ~f =
  Server_test_helpers.with_server
    (Database.For_testing.create_in_memory ~users:[ author ] ~posts ~tags ~post_tags ())
    ~f
;;

let print_post connection ~slug =
  let%map post = Rpc.Rpc.dispatch_exn Rpcs.Get_post.rpc connection { slug } in
  print_s [%sexp (post : Rpcs.Post.t option)]
;;

let%expect_test "the post list excludes special posts" =
  let%bind response =
    with_seeded_server ~f:(fun server ->
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        Rpc.Rpc.dispatch_exn
          Rpcs.Get_post_list.rpc
          connection
          { tag_slug = None; limit = None; offset = None }))
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
    with_seeded_server ~f:(fun server ->
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        Rpc.Rpc.dispatch_exn Rpcs.Get_tags.rpc connection ()))
  in
  print_s [%sexp (response : Rpcs.Tag_with_count.t list)];
  [%expect {| (((tag ((name OCaml) (slug ocaml))) (post_count 1))) |}];
  return ()
;;

let%expect_test "a post is fetched by slug, with its tags and languages" =
  let%bind response =
    with_seeded_server ~f:(fun server ->
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        Rpc.Rpc.dispatch_exn Rpcs.Get_post.rpc connection { slug = "hello-world" }))
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
  with_seeded_server ~f:(fun server ->
    Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
      let dispatch query =
        Rpc.Rpc.dispatch_exn Rpcs.Search_posts.rpc connection { query }
      in
      let%bind matching = dispatch "world" in
      print_s [%sexp (matching : Rpcs.Post_summary.t list)];
      [%expect
        {|
    (((title "Hello world") (slug hello-world) (excerpt "Content of Hello world")
      (created_at "2026-08-02 15:00:00Z") (tags (((name OCaml) (slug ocaml))))
      (languages (English)))) |}];
      return ()))
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
    Server_test_helpers.with_server db ~f:(fun server ->
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        Rpc.Rpc.dispatch_exn Rpcs.Get_main_page.rpc connection ()))
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

let%expect_test "an anonymous connection cannot write posts" =
  let%bind () =
    with_seeded_server ~f:(fun server ->
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        let form : Rpcs.Post_form.t =
          { title = "Sneaky"
          ; slug = "sneaky"
          ; content = Language.Map.singleton English "Written by nobody"
          ; special_post = false
          }
        in
        let%bind created = Rpc.Rpc.dispatch Rpcs.Create_post.rpc connection form in
        print_s [%sexp (created : Rpcs.Create_post.Response.t Or_error.t)];
        [%expect {| (Ok Not_logged_in) |}];
        let%bind updated =
          Rpc.Rpc.dispatch Rpcs.Update_post.rpc connection { slug = "hello-world"; form }
        in
        print_s [%sexp (updated : Rpcs.Update_post.Response.t Or_error.t)];
        [%expect {| (Ok Not_found) |}];
        let%map posts =
          Rpc.Rpc.dispatch_exn
            Rpcs.Get_post_list.rpc
            connection
            { tag_slug = None; limit = None; offset = None }
        in
        print_s
          [%sexp
            (List.map posts ~f:(fun post -> post.Rpcs.Post_summary.slug) : string list)]))
  in
  [%expect {| (hello-world) |}];
  return ()
;;

let form : Rpcs.Post_form.t =
  { title = "First"
  ; slug = "first"
  ; content = Language.Map.singleton English "Hello"
  ; special_post = false
  }
;;

let%expect_test "the author creates a post and edits it" =
  with_seeded_server ~f:(fun server ->
    let%bind _response, token =
      Server_test_helpers.login ~username:author.username ~password server
    in
    Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
      let%bind created = Rpc.Rpc.dispatch_exn Rpcs.Create_post.rpc connection form in
      print_s [%sexp (created : Rpcs.Create_post.Response.t)];
      [%expect {| Saved |}];
      let%bind () = print_post connection ~slug:"first" in
      [%expect
        {|
        (((title First) (slug first) (content ((English Hello)))
          (created_at "2026-08-01 00:00:00Z") (tags ()) (special_post false)
          (hidden false))) |}];
      let%bind updated =
        Rpc.Rpc.dispatch_exn
          Rpcs.Update_post.rpc
          connection
          { slug = "first"
          ; form = { form with title = "First, revised"; slug = "first-revised" }
          }
      in
      print_s [%sexp (updated : Rpcs.Update_post.Response.t)];
      [%expect {| Saved |}];
      (* Editing the slug moves the post. *)
      let%bind () = print_post connection ~slug:"first" in
      [%expect {| () |}];
      let%bind () = print_post connection ~slug:"first-revised" in
      [%expect
        {|
        (((title "First, revised") (slug first-revised) (content ((English Hello)))
          (created_at "2026-08-01 00:00:00Z") (tags ()) (special_post false)
          (hidden false))) |}];
      return ()))
;;

let%expect_test "a slug belongs to one post" =
  with_seeded_server ~f:(fun server ->
    let%bind _response, token =
      Server_test_helpers.login ~username:author.username ~password server
    in
    Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
      let taken = { form with slug = "hello-world" } in
      let%bind created = Rpc.Rpc.dispatch_exn Rpcs.Create_post.rpc connection taken in
      print_s [%sexp (created : Rpcs.Create_post.Response.t)];
      [%expect {| Duplicate_slug |}];
      (* A post keeping its own slug is not a clash with itself. *)
      let%bind updated =
        Rpc.Rpc.dispatch_exn
          Rpcs.Update_post.rpc
          connection
          { slug = "hello-world"; form = taken }
      in
      print_s [%sexp (updated : Rpcs.Update_post.Response.t)];
      [%expect {| Saved |}];
      return ()))
;;
