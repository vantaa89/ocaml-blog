open! Core
open! Import
open Bonsai.Let_syntax

let news_preview_count = 3

let html_node ~tag ~attrs ~html =
  Vdom.Node.inner_html
    ~tag
    ~attrs
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:html
    ()
;;

let home =
  let%sub poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Unit)
      (module Rpcs.Get_main_page.Response)
      Rpcs.Get_main_page.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      (Value.return ())
  in
  let%sub intro_html =
    let%arr poll = poll in
    let markdown =
      let%bind.Option (), response = poll.last_ok_response in
      let%bind.Option post = response.main_post in
      Client_utils.primary_content post.content
    in
    Option.value_map markdown ~default:"" ~f:(fun markdown ->
      Markdown_renderer.render ~markdown)
  in
  let%sub () = Client_utils.rerender_on_change intro_html in
  let%sub news_expanded, set_news_expanded =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%arr poll = poll
  and intro_html = intro_html
  and news_expanded = news_expanded
  and set_news_expanded = set_news_expanded in
  let view =
    let open Vdom in
    let social_media_icon (social_media : Owner_profile.Social_media.t) =
      let file =
        match social_media with
        | Github -> "github.svg"
        | Linkedin -> "linkedin.png"
        | Instagram -> "instagram.svg"
        | Email -> "email.png"
        | Cv -> "resume.png"
      in
      "/static/icons" ^/ file
    in
    let profile_node =
      let ({ name; description; profile_picture; links } : Owner_profile.t) =
        Owner_profile.info
      in
      Node.div
        ~attrs:[ Attr.id "main-profile" ]
        [ Node.div
            ~attrs:[ Attr.class_ "main-profile-text" ]
            [ Node.div
                ~attrs:[ Attr.class_ "main-profile-avatar" ]
                [ Node.img
                    ~attrs:
                      [ Attr.src profile_picture
                      ; Attr.alt [%string "Profile photo of %{name}"]
                      ]
                    ()
                ]
            ; Node.h2 ~attrs:[ Attr.class_ "main-profile-name" ] [ Node.text name ]
            ; Node.h6 ~attrs:[ Attr.id "main-short-desc" ] [ Node.text description ]
            ; Node.div
                ~attrs:[ Attr.id "main-social-icons" ]
                (List.map links ~f:(fun (social_media, url) ->
                   Node.a
                     ~attrs:[ Attr.class_ "social-icon"; Attr.href url ]
                     [ Node.img ~attrs:[ Attr.src (social_media_icon social_media) ] () ]))
            ]
        ]
    in
    let publication_node
          ({ title; image_url; authors; journal; link } : Rpcs.Publication.t)
      =
      let image = Node.img ~attrs:[ Attr.src image_url; Attr.alt title ] () in
      let title_node =
        match link with
        | Some link ->
          html_node
            ~tag:"a"
            ~attrs:[ Attr.href link; Attr.class_ "publication-title" ]
            ~html:(Markdown_renderer.render ~markdown:title)
        | None ->
          html_node
            ~tag:"div"
            ~attrs:[ Attr.class_ "publication-title" ]
            ~html:(Markdown_renderer.render ~markdown:title)
      in
      Node.div
        ~attrs:[ Attr.class_ "publication" ]
        [ Node.div
            ~attrs:[ Attr.class_ "publication-image" ]
            [ (match link with
               | Some link -> Node.a ~attrs:[ Attr.href link ] [ image ]
               | None -> image)
            ]
        ; Node.div
            ~attrs:[ Attr.class_ "publication-text" ]
            [ title_node
            ; html_node
                ~tag:"div"
                ~attrs:[ Attr.class_ "publication-authors" ]
                ~html:(Markdown_renderer.render ~markdown:authors)
            ; html_node
                ~tag:"div"
                ~attrs:[ Attr.class_ "publication-journal" ]
                ~html:(Markdown_renderer.render ~markdown:journal)
            ]
        ]
    in
    let news_node index ({ content; date } : Rpcs.News.t) =
      let month = Date.month date |> Month.to_string in
      let year = Date.year date in
      Node.div
        ~attrs:
          [ Attr.classes
              ("news-entry"
               ::
               (match index >= news_preview_count with
                | true -> [ "extra-news" ]
                | false -> []))
          ]
        [ Node.div
            ~attrs:[ Attr.class_ "news-date" ]
            [ Node.p [ Node.text [%string "%{month}. %{year#Int}"] ] ]
        ; Node.div [ Node.p [ Node.text content ] ]
        ]
    in
    Client_utils.of_poll poll ~f:(fun response ->
      let toggle_button =
        match List.length response.news > news_preview_count with
        | false -> Node.none
        | true ->
          Node.button
            ~attrs:
              [ Attr.id "toggle-news-btn"
              ; Attr.class_ "news-toggle-btn"
              ; Attr.type_ "button"
              ; Attr.create "aria-expanded" (Bool.to_string news_expanded)
              ; Attr.on_click (fun _ -> set_news_expanded (not news_expanded))
              ]
            [ Node.text
                (match news_expanded with
                 | true -> "Show less ↑"
                 | false -> "Show more ↓")
            ]
      in
      Node.div
        ~attrs:[ Attr.classes [ "custom-container"; "page" ]; Attr.id "main" ]
        [ profile_node
        ; html_node ~tag:"div" ~attrs:[ Attr.id "main-intro" ] ~html:intro_html
        ; Node.hr ()
        ; Node.h3 [ Node.text "Publications" ]
        ; Node.div
            ~attrs:[ Attr.id "main-publications" ]
            (List.map response.publications ~f:publication_node)
        ; Node.hr ()
        ; Node.h3 [ Node.text "Recent Posts" ]
        ; Node.div
            ~attrs:[ Attr.id "main-recent-posts" ]
            (List.map response.recent_posts ~f:Client_utils.post_card)
        ; Client_utils.link
            ~attrs:[ Attr.id "more-posts-link"; Attr.class_ "plain-link" ]
            (Posts { tag = None; page = 1 })
            [ Node.text "More Posts →" ]
        ; Node.hr ()
        ; Node.h3 [ Node.text "Updates " ]
        ; Node.div
            ~attrs:
              [ Attr.id "main-news"
              ; Attr.classes
                  [ "news-collapsible"
                  ; (match news_expanded with
                     | true -> "expanded"
                     | false -> "collapsed")
                  ]
              ]
            (List.mapi response.news ~f:news_node)
        ; toggle_button
        ])
  in
  ({ title = None; view } : Page.t)
;;

(** The shared body of the post list pages: heading, optional match count, post cards,
    paginator and the tag sidebar. *)
let post_list_node
      ~(current_user : Rpcs.Get_current_user.Response.t)
      ~heading
      ~match_count
      ~posts
      ~tags
      ~route
      ~page
      ~num_pages
  =
  let open Vdom in
  Node.div
    ~attrs:[ Attr.classes [ "custom-container"; "page" ] ]
    [ Node.h1 [ Node.text heading ]
    ; (match match_count with
       | None -> Node.none
       | Some count ->
         Node.p
           ~attrs:[ Attr.id "match-count" ]
           [ Node.text [%string "%{count#Int} matches"] ])
    ; Node.div
        ~attrs:[ Attr.id "posts" ]
        (List.concat_map posts ~f:(fun post ->
           [ Client_utils.post_card post; Node.hr () ]))
    ; Client_utils.paginator ~route ~page ~num_pages
    ; (match current_user with
       | Not_logged_in -> Node.none
       | Logged_in _ ->
         Node.div
           ~attrs:[ Attr.class_ "author-actions" ]
           [ Client_utils.link
               ~attrs:[ Attr.class_ "new-post-button" ]
               New_post
               [ Node.text "New post" ]
           ])
    ; Node.create
        "aside"
        ~attrs:[ Attr.classes [ "sidebar"; "custom-container" ] ]
        [ Node.h5 [ Node.text "tags" ]
        ; Node.ul
            ~attrs:[ Attr.class_ "tags" ]
            (List.map tags ~f:(fun (tag, post_count) ->
               Node.li
                 [ Client_utils.link
                     ~attrs:[ Attr.class_ "sidebar-tag" ]
                     (Posts { tag = Some tag; page = 1 })
                     [ Node.text [%string "%{tag} "] ]
                 ; Node.span
                     ~attrs:[ Attr.class_ "sidebar-tag-count" ]
                     [ Node.text [%string "(%{post_count#Int})"] ]
                 ]))
        ]
    ]
;;

let posts ~tag ~page =
  let%sub query =
    let%arr tag = tag
    and page = page in
    (* One extra post tells us whether there is a next page. *)
    { Rpcs.Get_post_list.Query.tag
    ; limit = Some (Client_utils.page_size + 1)
    ; offset = Some ((page - 1) * Client_utils.page_size)
    }
  in
  let%sub posts_poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Rpcs.Get_post_list.Query)
      (module Rpcs.Get_post_list.Response)
      Rpcs.Get_post_list.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      query
  in
  let%sub tags_poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Unit)
      (module Rpcs.Get_tags.Response)
      Rpcs.Get_tags.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      (Value.return ())
  in
  let%sub current_user = Session.current_user in
  let%arr posts_poll = posts_poll
  and tags_poll = tags_poll
  and tag = tag
  and page = page
  and current_user = current_user in
  let title =
    match tag with
    | None -> Some "Posts"
    | Some tag -> Some [%string "Posts tagged %{tag}"]
  in
  let view =
    Client_utils.of_poll posts_poll ~f:(fun fetched ->
      let tags =
        match tags_poll.last_ok_response with
        | None -> []
        | Some ((), tags) -> tags
      in
      let posts = List.take fetched Client_utils.page_size in
      let num_pages =
        match List.length fetched > Client_utils.page_size with
        | true -> page + 1
        | false -> page
      in
      let heading, match_count =
        match tag with
        | None -> "posts", None
        | Some tag ->
          let count =
            List.find_map tags ~f:(fun (candidate, post_count) ->
              Option.some_if (String.Caseless.equal candidate tag) post_count)
          in
          [%string "tag: %{tag}"], count
      in
      post_list_node
        ~current_user
        ~heading
        ~match_count
        ~posts
        ~tags
        ~route:(Route.Posts { tag; page })
        ~page
        ~num_pages)
  in
  ({ title; view } : Page.t)
;;

let search ~query ~page =
  let%sub rpc_query =
    let%arr query = query in
    { Rpcs.Search_posts.Query.query }
  in
  let%sub posts_poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Rpcs.Search_posts.Query)
      (module Rpcs.Search_posts.Response)
      Rpcs.Search_posts.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      rpc_query
  in
  let%sub tags_poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Unit)
      (module Rpcs.Get_tags.Response)
      Rpcs.Get_tags.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      (Value.return ())
  in
  let%sub current_user = Session.current_user in
  let%arr posts_poll = posts_poll
  and tags_poll = tags_poll
  and query = query
  and page = page
  and current_user = current_user in
  ({ title = Some [%string "Search: %{query}"]
   ; view =
       Client_utils.of_poll posts_poll ~f:(fun matches ->
         let tags =
           match tags_poll.last_ok_response with
           | None -> []
           | Some ((), tags) -> tags
         in
         let count = List.length matches in
         let num_pages =
           Int.max 1 ((count + Client_utils.page_size - 1) / Client_utils.page_size)
         in
         post_list_node
           ~current_user
           ~heading:[%string "search: %{query}"]
           ~match_count:(Some count)
           ~posts:(Client_utils.paginate matches ~page)
           ~tags
           ~route:(Route.Search { query; page })
           ~page
           ~num_pages)
   }
   : Page.t)
;;

(***** post detail *****)

let post_detail ~slug =
  let%sub query =
    let%arr slug = slug in
    { Rpcs.Get_post.Query.slug }
  in
  let%sub poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Rpcs.Get_post.Query)
      (module Rpcs.Get_post.Response)
      Rpcs.Get_post.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      query
  in
  let%sub language, set_language =
    Bonsai.state (module Language) ~default_model:English
  in
  let%sub html, title =
    let%arr poll = poll
    and language = language in
    match poll.last_ok_response with
    | None -> "", None
    | Some (_query, None) -> "", Some "Not found"
    | Some (_query, Some post) ->
      let html =
        Option.first_some
          (Map.find post.content language)
          (Client_utils.primary_content post.content)
        |> Option.value_map ~default:"" ~f:(fun markdown ->
          Markdown_renderer.render ~markdown)
      in
      html, Some post.title
  in
  let%sub () = Client_utils.rerender_on_change html in
  let%sub current_user = Session.current_user in
  let%sub set_post_hidden =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Set_post_hidden.rpc
      ~where_to_connect:Rpc_client.where_to_connect
  in
  let%arr poll = poll
  and html = html
  and language = language
  and set_language = set_language
  and current_user = current_user
  and set_post_hidden = set_post_hidden
  and slug = slug
  and title = title in
  let open Vdom in
  let language_toggle =
    let button target label =
      Node.button
        ~attrs:
          [ Attr.type_ "button"
          ; Attr.id [%string "lang-%{Language.to_code target}-btn"]
          ; Attr.classes
              ("language-btn"
               ::
               (match Language.equal target language with
                | true -> [ "active" ]
                | false -> []))
          ; Attr.on_click (fun _ -> set_language target)
          ]
        [ Node.text label ]
    in
    Node.div
      ~attrs:[ Attr.class_ "language-toggle-container" ]
      [ Node.div
          ~attrs:[ Attr.class_ "language-toggle"; Attr.id "language-toggle" ]
          [ button English "EN"; button Korean "한" ]
      ]
  in
  let view =
    Client_utils.of_poll poll ~f:(fun post ->
      match post with
      | None -> Client_utils.not_found_node
      | Some { title; content; created_at; tags; hidden; _ } ->
        let read_time =
          Client_utils.primary_content content |> Option.map ~f:Client_utils.read_time
        in
        let languages = Map.keys content in
        let post_time_node =
          let languages =
            List.map languages ~f:Language.to_code |> String.concat ~sep:"/"
          in
          let parts =
            List.filter_opt
              [ Some (Client_utils.format_time created_at)
              ; read_time
              ; Some [%string "🌐︎ %{languages}"]
              ]
          in
          let author_action =
            let hide_toggle =
              Node.button
                ~attrs:
                  [ Attr.class_ "author-action"
                  ; Attr.type_ "button"
                  ; Attr.on_click (fun _ ->
                      Effect.bind
                        (set_post_hidden { slug; hidden = not hidden })
                        ~f:(fun _ -> poll.refresh))
                  ]
                [ Node.text
                    (match hidden with
                     | true -> "unhide"
                     | false -> "hide")
                ]
            in
            match current_user with
            | Not_logged_in -> []
            | Logged_in _ ->
              [ Node.text " · "
              ; hide_toggle
              ; Node.text " · "
              ; Client_utils.link
                  ~attrs:[ Attr.class_ "author-action" ]
                  (Edit_post { slug })
                  [ Node.text "edit" ]
              ]
          in
          Node.p
            ~attrs:[ Attr.class_ "post-time" ]
            (Node.text (String.concat parts ~sep:" · ") :: author_action)
        in
        let language_toggle =
          match List.length languages > 1 with
          | false -> Node.none
          | true -> language_toggle
        in
        Node.div
          ~attrs:[ Attr.classes [ "custom-container"; "page"; "post-detail" ] ]
          [ Node.div
              ~attrs:[ Attr.class_ "post-detail-header" ]
              [ Node.div
                  ~attrs:[ Attr.class_ "post-detail-title" ]
                  [ Node.h1 [ Node.text title ] ]
              ; Node.div
                  ~attrs:[ Attr.class_ "post-detail-meta" ]
                  [ Node.div (post_time_node :: List.map tags ~f:Client_utils.tag_node)
                  ; language_toggle
                  ]
              ]
          ; html_node ~tag:"div" ~attrs:[ Attr.class_ "post-detail-content" ] ~html
          ])
  in
  ({ title; view } : Page.t)
;;

let about =
  let%sub poll =
    Rpc_effect.Rpc.poll_until_ok
      (module Unit)
      (module Rpcs.Get_about_page.Response)
      Rpcs.Get_about_page.rpc
      ~where_to_connect:Rpc_client.where_to_connect
      ~retry_interval:Rpc_client.retry_interval
      (Value.return ())
  in
  (* The about post is written in HTML and shown as it is, not as markdown. *)
  let%sub html =
    let%arr poll = poll in
    match poll.last_ok_response with
    | None -> ""
    | Some ((), post) ->
      Option.bind post ~f:(fun post -> Client_utils.primary_content post.content)
      |> Option.value ~default:""
  in
  let%sub () = Client_utils.rerender_on_change html in
  let%arr poll = poll
  and html = html in
  let view =
    let open Vdom in
    Client_utils.of_poll poll ~f:(fun (_ : Rpcs.Post.t option) ->
      Node.div
        [ Node.h1
            ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"none") ]
            [ Node.text "about" ]
        ; html_node
            ~tag:"div"
            ~attrs:
              [ Vdom.Attr.classes [ "custom-container"; "page"; "about-content" ]
              ; Vdom.Attr.id "about"
              ]
            ~html
        ])
  in
  ({ title = Some "About"; view } : Page.t)
;;

let not_found =
  Bonsai.const ({ title = Some "Not found"; view = Client_utils.not_found_node } : Page.t)
;;
