open! Core
open! Import
open Bonsai.Let_syntax

let name = "Your Name"
let short_desc = "A short line about you"
let linkedin_url = "https://www.linkedin.com/in/your-account/"
let github_url = "https://github.com/your-account/"
let news_preview_count = 3

let html_node ~tag ~attrs ~html =
  Vdom.Node.inner_html
    ~tag
    ~attrs
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:html
    ()
;;

let content_html (post : Rpcs.Post.t option) =
  match Option.bind post ~f:(fun post -> Client_utils.primary_content post.content) with
  | None -> ""
  | Some markdown -> Markdown_renderer.render ~markdown
;;

(***** Home *****)

let profile_node =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.id "main-profile"; Vdom.Attr.class_ "my-3" ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "main-profile-text" ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "main-profile-avatar" ]
            [ Vdom.Node.img
                ~attrs:
                  [ Vdom.Attr.src "/static/profile_pic.jpeg"
                  ; Vdom.Attr.alt [%string "Profile photo of %{name}"]
                  ]
                ()
            ]
        ; Vdom.Node.h2
            ~attrs:[ Vdom.Attr.class_ "main-profile-name" ]
            [ Vdom.Node.text name ]
        ; Vdom.Node.h6
            ~attrs:[ Vdom.Attr.id "main-short-desc" ]
            [ Vdom.Node.text short_desc ]
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.id "main-social-icons" ]
            [ Vdom.Node.a
                ~attrs:[ Vdom.Attr.class_ "social-icon"; Vdom.Attr.href linkedin_url ]
                [ Vdom.Node.img ~attrs:[ Vdom.Attr.src "/static/icons/linkedin.png" ] () ]
            ; Vdom.Node.a
                ~attrs:[ Vdom.Attr.class_ "social-icon"; Vdom.Attr.href github_url ]
                [ Vdom.Node.img ~attrs:[ Vdom.Attr.src "/static/icons/github.svg" ] () ]
            ]
        ]
    ]
;;

let publication_node ({ title; image_url; authors; journal; link } : Rpcs.Publication.t) =
  let image =
    Vdom.Node.img
      ~attrs:
        [ Vdom.Attr.src image_url; Vdom.Attr.class_ "img-fluid"; Vdom.Attr.alt title ]
      ()
  in
  let title_node =
    match link with
    | Some link ->
      html_node
        ~tag:"a"
        ~attrs:[ Vdom.Attr.href link; Vdom.Attr.class_ "publication-title" ]
        ~html:(Markdown_renderer.render ~markdown:title)
    | None ->
      html_node
        ~tag:"div"
        ~attrs:[ Vdom.Attr.class_ "publication-title" ]
        ~html:(Markdown_renderer.render ~markdown:title)
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "row"; "my-2"; "align-items-center" ] ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "col-4"; "col-md-3" ] ]
        [ (match link with
           | Some link -> Vdom.Node.a ~attrs:[ Vdom.Attr.href link ] [ image ]
           | None -> image)
        ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "col-8"; "col-md-9"; "publication-text" ] ]
        [ title_node
        ; html_node
            ~tag:"div"
            ~attrs:[ Vdom.Attr.class_ "publication-authors" ]
            ~html:(Markdown_renderer.render ~markdown:authors)
        ; html_node
            ~tag:"div"
            ~attrs:[ Vdom.Attr.class_ "publication-journal" ]
            ~html:(Markdown_renderer.render ~markdown:journal)
        ]
    ]
;;

let news_node index ({ content; date } : Rpcs.News.t) =
  let month = Date.month date |> Month.to_string in
  let year = Date.year date in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.classes
          ("news-entry"
           :: "row"
           ::
           (match index >= news_preview_count with
            | true -> [ "extra-news" ]
            | false -> []))
      ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "news-date"; "col-sm-4"; "col-md-2" ] ]
        [ Vdom.Node.p [ Vdom.Node.text [%string "%{month}. %{year#Int}"] ] ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "col-sm-8"; "col-md-10" ] ]
        [ Vdom.Node.p [ Vdom.Node.text content ] ]
    ]
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
    match poll.last_ok_response with
    | None -> ""
    | Some ((), response) -> content_html response.main_post
  in
  let%sub () = Client_utils.rerender_math_on_change intro_html in
  let%sub news_expanded, set_news_expanded =
    Bonsai.state (module Bool) ~default_model:false
  in
  let%arr poll = poll
  and intro_html = intro_html
  and news_expanded = news_expanded
  and set_news_expanded = set_news_expanded in
  Client_utils.of_poll poll ~f:(fun (response : Rpcs.Get_main_page.Response.t) ->
    let toggle_button =
      match List.length response.news > news_preview_count with
      | false -> Vdom.Node.none
      | true ->
        Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.id "toggle-news-btn"
            ; Vdom.Attr.class_ "news-toggle-btn"
            ; Vdom.Attr.type_ "button"
            ; Vdom.Attr.create "aria-expanded" (Bool.to_string news_expanded)
            ; Vdom.Attr.on_click (fun _ -> set_news_expanded (not news_expanded))
            ]
          [ Vdom.Node.text
              (match news_expanded with
               | true -> "Show less ↑"
               | false -> "Show more ↓")
          ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "custom-container"; "my-3" ]; Vdom.Attr.id "main" ]
      [ profile_node
      ; html_node
          ~tag:"div"
          ~attrs:[ Vdom.Attr.id "main-intro"; Vdom.Attr.class_ "my-3" ]
          ~html:intro_html
      ; Vdom.Node.hr ()
      ; Vdom.Node.h3 ~attrs:[ Vdom.Attr.class_ "my-3" ] [ Vdom.Node.text "Publications" ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.id "main-publications" ]
          (List.map response.publications ~f:publication_node)
      ; Vdom.Node.hr ()
      ; Vdom.Node.h3 ~attrs:[ Vdom.Attr.class_ "my-3" ] [ Vdom.Node.text "Recent Posts" ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.id "main-recent-posts" ]
          (List.map response.recent_posts ~f:Client_utils.post_card)
      ; Client_utils.link
          ~attrs:[ Vdom.Attr.id "more-posts-link" ]
          (Posts { tag_slug = None; page = 1 })
          [ Vdom.Node.text "More Posts →" ]
      ; Vdom.Node.hr ()
      ; Vdom.Node.h3 [ Vdom.Node.text "Updates " ]
      ; Vdom.Node.div
          ~attrs:
            [ Vdom.Attr.id "main-news"
            ; Vdom.Attr.classes
                [ "news-collapsible"
                ; (match news_expanded with
                   | true -> "expanded"
                   | false -> "collapsed")
                ]
            ]
          (List.mapi response.news ~f:news_node)
      ; toggle_button
      ])
;;

(***** post list *****)

let sidebar_node tags =
  Vdom.Node.create
    "aside"
    ~attrs:[ Vdom.Attr.classes [ "sidebar"; "custom-container" ] ]
    [ Vdom.Node.h5 [ Vdom.Node.text "tags" ]
    ; Vdom.Node.ul
        ~attrs:[ Vdom.Attr.class_ "tags" ]
        (List.map tags ~f:(fun ({ tag; post_count } : Rpcs.Tag_with_count.t) ->
           Vdom.Node.li
             [ Client_utils.link
                 ~attrs:[ Vdom.Attr.class_ "sidebar-tag" ]
                 (Posts { tag_slug = Some tag.slug; page = 1 })
                 [ Vdom.Node.text [%string "%{tag.name} "] ]
             ; Vdom.Node.span
                 ~attrs:[ Vdom.Attr.class_ "sidebar-tag-count" ]
                 [ Vdom.Node.text [%string "(%{post_count#Int})"] ]
             ]))
    ]
;;

(** The shared body of [posts.html]: heading, optional match count, post cards,
    paginator and the tag sidebar. *)
let post_list_node ~heading ~match_count ~posts ~tags ~route ~page ~num_pages =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "custom-container"; "my-3" ] ]
    [ Vdom.Node.h1 [ Vdom.Node.text heading ]
    ; (match match_count with
       | None -> Vdom.Node.none
       | Some count ->
         Vdom.Node.p
           ~attrs:[ Vdom.Attr.id "match-count" ]
           [ Vdom.Node.text [%string "%{count#Int} matches"] ])
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.id "posts" ]
        (List.concat_map posts ~f:(fun post ->
           [ Client_utils.post_card post; Vdom.Node.hr () ]))
    ; Client_utils.paginator ~route ~page ~num_pages
    ; sidebar_node tags
    ]
;;

let posts ~tag_slug ~page =
  let%sub query =
    let%arr tag_slug = tag_slug
    and page = page in
    (* One extra post tells us whether there is a next page. *)
    { Rpcs.Get_post_list.Query.tag_slug
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
  let%arr posts_poll = posts_poll
  and tags_poll = tags_poll
  and tag_slug = tag_slug
  and page = page in
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
      match tag_slug with
      | None -> "posts", None
      | Some tag_slug ->
        let count =
          List.find tags ~f:(fun ({ tag; _ } : Rpcs.Tag_with_count.t) ->
            String.equal tag.slug tag_slug)
          |> Option.map ~f:(fun ({ post_count; _ } : Rpcs.Tag_with_count.t) -> post_count)
        in
        [%string "tag: %{tag_slug}"], count
    in
    post_list_node
      ~heading
      ~match_count
      ~posts
      ~tags
      ~route:(Route.Posts { tag_slug; page })
      ~page
      ~num_pages)
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
  let%arr posts_poll = posts_poll
  and tags_poll = tags_poll
  and query = query
  and page = page in
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
      ~heading:[%string "search: %{query}"]
      ~match_count:(Some count)
      ~posts:(Client_utils.paginate matches ~page)
      ~tags
      ~route:(Route.Search { query; page })
      ~page
      ~num_pages)
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
    Bonsai.state (module Language) ~default_model:Language.English
  in
  let%sub html =
    let%arr poll = poll
    and language = language in
    match poll.last_ok_response with
    | None -> ""
    | Some (_query, None) -> ""
    | Some (_query, Some (post : Rpcs.Post.t)) ->
      let markdown =
        match Map.find post.content language with
        | Some markdown -> Some markdown
        | None -> Client_utils.primary_content post.content
      in
      (match markdown with
       | None -> ""
       | Some markdown -> Markdown_renderer.render ~markdown)
  in
  let%sub () = Client_utils.rerender_math_on_change html in
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
  and slug = slug in
  Client_utils.of_poll poll ~f:(fun post ->
    match post with
    | None -> Client_utils.not_found_node
    | Some
        ({ title; slug = _; content; created_at; tags; special_post = _; hidden } :
          Rpcs.Post.t) ->
      let read_time =
        Client_utils.primary_content content |> Option.map ~f:Client_utils.read_time
      in
      let languages = Map.keys content in
      let post_time_node =
        let languages =
          List.map languages ~f:Language.to_code |> String.concat ~sep:", "
        in
        let parts =
          List.filter_opt
            [ Some (Client_utils.format_time created_at)
            ; read_time
            ; Some [%string "🌐︎ %{languages}"]
            ]
        in
        let author_action =
          match current_user with
          | Not_logged_in -> []
          | Logged_in _ ->
            [ Vdom.Node.text " · "
            ; Vdom.Node.button
                ~attrs:
                  [ Vdom.Attr.class_ "author-action"
                  ; Vdom.Attr.type_ "button"
                  ; Vdom.Attr.on_click (fun _ ->
                      let%bind.Effect (_ : unit Or_error.t) =
                        set_post_hidden { slug; hidden = not hidden }
                      in
                      poll.refresh)
                  ]
                [ Vdom.Node.text
                    (match hidden with
                     | true -> "unhide"
                     | false -> "hide")
                ]
            ]
        in
        Vdom.Node.p
          ~attrs:[ Vdom.Attr.class_ "post-time" ]
          (Vdom.Node.text (String.concat parts ~sep:" · ") :: author_action)
      in
      let language_toggle =
        match List.length languages > 1 with
        | false -> Vdom.Node.none
        | true ->
          let button (target : Language.t) label =
            Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.type_ "button"
                ; Vdom.Attr.id [%string "lang-%{Language.to_code target}-btn"]
                ; Vdom.Attr.classes
                    ("language-btn"
                     ::
                     (match Language.equal target language with
                      | true -> [ "active" ]
                      | false -> []))
                ; Vdom.Attr.on_click (fun _ -> set_language target)
                ]
              [ Vdom.Node.text label ]
          in
          Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "language-toggle-container" ]
            [ Vdom.Node.div
                ~attrs:
                  [ Vdom.Attr.class_ "language-toggle"; Vdom.Attr.id "language-toggle" ]
                [ button English "EN"; button Korean "한" ]
            ]
      in
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.classes [ "container"; "custom-container"; "py-3"; "post-detail" ] ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "post-detail-header" ]
            [ Vdom.Node.div
                ~attrs:
                  [ Vdom.Attr.style
                      Css_gen.(
                        Css_gen.create ~field:"display" ~value:"flex"
                        @> Css_gen.create ~field:"flex-direction" ~value:"row")
                  ]
                [ Vdom.Node.h1 [ Vdom.Node.text title ] ]
            ; Vdom.Node.div
                ~attrs:
                  [ Vdom.Attr.style
                      Css_gen.(
                        Css_gen.create ~field:"display" ~value:"flex"
                        @> Css_gen.create ~field:"flex-direction" ~value:"row"
                        @> Css_gen.create ~field:"align-items" ~value:"center"
                        @> Css_gen.create ~field:"justify-content" ~value:"space-between")
                  ]
                [ Vdom.Node.div (post_time_node :: List.map tags ~f:Client_utils.tag_node)
                ; language_toggle
                ]
            ]
        ; html_node ~tag:"div" ~attrs:[ Vdom.Attr.class_ "post-detail-content" ] ~html
        ])
;;

(***** about *****)

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
  let%sub html =
    let%arr poll = poll in
    match poll.last_ok_response with
    | None -> ""
    | Some ((), post) -> content_html post
  in
  let%sub () = Client_utils.rerender_math_on_change html in
  let%arr poll = poll
  and html = html in
  Client_utils.of_poll poll ~f:(fun (_ : Rpcs.Post.t option) ->
    Vdom.Node.div
      [ Vdom.Node.h1
          ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"none") ]
          [ Vdom.Node.text "about" ]
      ; html_node
          ~tag:"div"
          ~attrs:
            [ Vdom.Attr.classes
                [ "container"; "custom-container"; "py-3"; "about-content" ]
            ; Vdom.Attr.id "about"
            ]
          ~html
      ])
;;
