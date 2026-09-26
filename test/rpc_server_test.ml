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

let tags : Database_schema.Tag.t list = [ { id = 1; name = "Tag" } ]
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
          { tag = None; limit = None; offset = None }))
  in
  print_s [%sexp (response : Rpcs.Post_summary.t list)];
  [%expect
    {|
    (((title "Hello world") (slug hello-world) (excerpt "Content of Hello world")
      (thumbnail ()) (created_at "2026-08-02 15:00:00Z") (tags (Tag))
      (languages (English)))) |}];
  return ()
;;

let%expect_test "a post card shows the first [![](url)] of the post, English first" =
  let posts =
    [ { (post ~id:1 ~slug:"bilingual" ~title:"Bilingual post" ~special_post:false) with
        content_en = Some "Intro ![](/media/en.png)"
      ; content_ko = Some "![](/media/ko.png)"
      }
    ; { (post ~id:2 ~slug:"korean-only" ~title:"Korean-only post" ~special_post:false) with
        content_en = None
      ; content_ko = Some "![](/media/ko.png)"
      }
    ]
  in
  let%bind summaries =
    Server_test_helpers.with_server
      (Database.For_testing.create_in_memory ~users:[ author ] ~posts ())
      ~f:(fun server ->
        Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
          Rpc.Rpc.dispatch_exn
            Rpcs.Get_post_list.rpc
            connection
            { tag = None; limit = None; offset = None }))
  in
  List.iter summaries ~f:(fun ({ slug; thumbnail; _ } : Rpcs.Post_summary.t) ->
    print_s [%sexp (slug : string), (thumbnail : string option)]);
  [%expect
    {|
    (korean-only (/media/ko.png))
    (bilingual (/media/en.png)) |}];
  return ()
;;

let%expect_test "the page carries the site's name and description" =
  let%bind _response, page =
    with_seeded_server ~f:(fun server -> Server_test_helpers.get server ~path:"/")
  in
  print_s
    [%sexp
      (String.is_substring
         page
         ~substring:[%string "<title>%{Owner_profile.info.name}</title>"]
       : bool)
    , (String.is_substring
         page
         ~substring:[%string {|content="%{Owner_profile.info.description}"|}]
       : bool)
    , (String.is_substring page ~substring:"{{" : bool)];
  [%expect {| (true true false) |}];
  return ()
;;

let%expect_test "the tag list reports how many posts carry each tag" =
  let%bind response =
    with_seeded_server ~f:(fun server ->
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        Rpc.Rpc.dispatch_exn Rpcs.Get_tags.rpc connection ()))
  in
  print_s [%sexp (response : Rpcs.Get_tags.Response.t)];
  [%expect {| ((Tag 1)) |}];
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
      (created_at "2026-08-02 15:00:00Z") (tags (Tag)) (special_post false)
      (hidden false))) |}];
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
      (thumbnail ()) (created_at "2026-08-02 15:00:00Z") (tags (Tag))
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
          ; authors = "An author"
          ; journal = "A journal"
          ; link = Some "https://example.com/paper"
          ; hidden = false
          }
        ; { id = 2
          ; title = "A retracted paper"
          ; image_id = 1
          ; authors = "An author"
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
        (thumbnail ()) (created_at "2026-08-07 15:00:00Z") (tags ())
        (languages (English)))
       ((title Sixth) (slug sixth) (excerpt "Content of Sixth") (thumbnail ())
        (created_at "2026-08-06 15:00:00Z") (tags ()) (languages (English)))
       ((title Fifth) (slug fifth) (excerpt "Content of Fifth") (thumbnail ())
        (created_at "2026-08-05 15:00:00Z") (tags ()) (languages (English)))
       ((title Fourth) (slug fourth) (excerpt "Content of Fourth") (thumbnail ())
        (created_at "2026-08-04 15:00:00Z") (tags ()) (languages (English)))))
     (publications
      (((title "A paper") (image_url /media/2024-01-15/paper.png)
        (authors "An author") (journal "A journal")
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
          ; tags = []
          }
        in
        let%bind created = Rpc.Rpc.dispatch Rpcs.Create_post.rpc connection form in
        print_s [%sexp (created : Rpcs.Create_post.Response.t Or_error.t)];
        [%expect {| (Ok Not_logged_in) |}];
        let%bind updated =
          Rpc.Rpc.dispatch Rpcs.Update_post.rpc connection { slug = "hello-world"; form }
        in
        print_s [%sexp (updated : Rpcs.Update_post.Response.t Or_error.t)];
        [%expect {| (Ok Not_logged_in) |}];
        let%map posts =
          Rpc.Rpc.dispatch_exn
            Rpcs.Get_post_list.rpc
            connection
            { tag = None; limit = None; offset = None }
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
  ; tags = []
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

let%expect_test "saving a post sets its tags, reusing existing ones regardless of case" =
  with_seeded_server ~f:(fun server ->
    let%bind _response, token =
      Server_test_helpers.login ~username:author.username ~password server
    in
    Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
      let print_tags () =
        let%map post =
          Rpc.Rpc.dispatch_exn Rpcs.Get_post.rpc connection { slug = "first" }
        in
        print_s [%sexp (Option.map post ~f:(fun post -> post.tags) : string list option)]
      in
      let%bind (_ : Rpcs.Create_post.Response.t) =
        Rpc.Rpc.dispatch_exn
          Rpcs.Create_post.rpc
          connection
          { form with tags = [ "tag"; "Other tag"; " Padded tag "; "" ] }
      in
      let%bind () = print_tags () in
      [%expect
        {|
        ((Tag "Other tag" "Padded tag")) |}];
      let%bind (_ : Rpcs.Update_post.Response.t) =
        Rpc.Rpc.dispatch_exn
          Rpcs.Update_post.rpc
          connection
          { slug = "first"; form = { form with tags = [ "Other tag" ] } }
      in
      let%bind () = print_tags () in
      [%expect {| (("Other tag")) |}];
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

let png = "\x89PNG\r\n\x1a\nnot really an image"

let print_upload connection contents =
  let%map response = Rpc.Rpc.dispatch_exn Rpcs.Upload_image.rpc connection { contents } in
  print_s [%sexp (response : Rpcs.Upload_image.Response.t)]
;;

let print_media server ~path ~content =
  let%map response, body = Server_test_helpers.get server ~path in
  print_s
    [%sexp
      (Cohttp.Response.status response : Cohttp.Code.status_code)
    , (Cohttp.Header.get (Cohttp.Response.headers response) "content-type"
       : string option)
    , (String.equal body content : bool)]
;;

let%expect_test "an anonymous connection cannot upload images" =
  with_seeded_server ~f:(fun server ->
    let%bind () =
      Server_test_helpers.with_rpc_connection server ~f:(fun connection ->
        print_upload connection png)
    in
    [%expect {| Not_logged_in |}];
    return ())
;;

let%expect_test "an uploaded image is named after its contents and served" =
  with_seeded_server ~f:(fun server ->
    let%bind _response, token =
      Server_test_helpers.login ~username:author.username ~password server
    in
    let%bind () =
      Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
        let%bind () = print_upload connection png in
        [%expect {| (Uploaded (url /media/2026-08-01/a987c0d1fac0abc3.png)) |}];
        (* The same image again on the same day overwrites the same file. *)
        let%bind () = print_upload connection png in
        [%expect {| (Uploaded (url /media/2026-08-01/a987c0d1fac0abc3.png)) |}];
        let%bind () = print_upload connection "GIF89a" in
        [%expect {| (Uploaded (url /media/2026-08-01/610f5ae4d76e3326.gif)) |}];
        let%bind () = print_upload connection "<svg></svg>" in
        [%expect {| Unsupported_format |}];
        let%bind () =
          print_upload connection (png ^ String.make Rpcs.Upload_image.max_size ' ')
        in
        [%expect {| Too_large |}];
        return ())
    in
    let%bind () =
      print_media server ~path:"/media/2026-08-01/a987c0d1fac0abc3.png" ~content:png
    in
    [%expect {| (OK (image/png) true) |}];
    let%bind () =
      print_media server ~path:"/media/2026-08-01/610f5ae4d76e3326.gif" ~content:"GIF89a"
    in
    [%expect {| (OK (image/gif) true) |}];
    return ())
;;

let%expect_test "a directory is not served as a file" =
  let print_status server ~path =
    let%map response, _body = Server_test_helpers.get server ~path in
    print_s [%sexp (Cohttp.Response.status response : Cohttp.Code.status_code)]
  in
  with_seeded_server ~f:(fun server ->
    let%bind _response, token =
      Server_test_helpers.login ~username:author.username ~password server
    in
    let%bind () =
      Server_test_helpers.with_rpc_connection ?token server ~f:(fun connection ->
        print_upload connection png)
    in
    [%expect {| (Uploaded (url /media/2026-08-01/a987c0d1fac0abc3.png)) |}];
    let%bind () = print_status server ~path:"/static" in
    [%expect {| Not_found |}];
    let%bind () = print_status server ~path:"/static/" in
    [%expect {| Not_found |}];
    let%bind () = print_status server ~path:"/media" in
    [%expect {| Not_found |}];
    let%bind () = print_status server ~path:"/media/2026-08-01" in
    [%expect {| Not_found |}];
    return ())
;;
