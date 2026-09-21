open! Core
open! Import
open Bonsai.Let_syntax

let navbar ~search_trigger =
  Vdom.Node.create
    "nav"
    ~attrs:
      [ Vdom.Attr.classes [ "navbar"; "navbar-expand-lg"; "navbar-light"; "bg-light" ] ]
    [ Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.classes [ "container-fluid"; "container"; "custom-container" ] ]
        [ Client_utils.link
            ~attrs:[ Vdom.Attr.class_ "navbar-brand" ]
            Home
            [ Vdom.Node.create "b" [ Vdom.Node.text "Your" ]; Vdom.Node.text " Name" ]
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.class_ "navbar-toggler"
              ; Vdom.Attr.type_ "button"
              ; Vdom.Attr.style (Css_gen.create ~field:"border" ~value:"none")
              ; Vdom.Attr.create "data-bs-toggle" "collapse"
              ; Vdom.Attr.create "data-bs-target" "#navbarNavDropdown"
              ; Vdom.Attr.create "aria-controls" "navbarNavDropdown"
              ; Vdom.Attr.create "aria-expanded" "false"
              ; Vdom.Attr.create "aria-label" "Toggle navigation"
              ]
            [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "navbar-toggler-icon" ] [] ]
        ; Vdom.Node.div
            ~attrs:
              [ Vdom.Attr.classes [ "collapse"; "navbar-collapse" ]
              ; Vdom.Attr.id "navbarNavDropdown"
              ]
            [ Vdom.Node.ul
                ~attrs:[ Vdom.Attr.class_ "navbar-nav" ]
                [ Vdom.Node.li
                    ~attrs:[ Vdom.Attr.class_ "nav-item" ]
                    [ Client_utils.link
                        ~attrs:[ Vdom.Attr.class_ "nav-link" ]
                        Home
                        [ Vdom.Node.text "home" ]
                    ]
                ; Vdom.Node.li
                    ~attrs:[ Vdom.Attr.class_ "nav-item" ]
                    [ Client_utils.link
                        ~attrs:[ Vdom.Attr.class_ "nav-link" ]
                        (Posts { tag_slug = None; page = 1 })
                        [ Vdom.Node.text "posts" ]
                    ]
                ; Vdom.Node.li
                    ~attrs:[ Vdom.Attr.class_ "nav-item" ]
                    [ Client_utils.link
                        ~attrs:[ Vdom.Attr.class_ "nav-link" ]
                        About
                        [ Vdom.Node.text "about" ]
                    ]
                ]
            ; Vdom.Node.span
                ~attrs:[ Vdom.Attr.classes [ "nav-item"; "ms-auto" ] ]
                [ Vdom.Node.ul
                    ~attrs:[ Vdom.Attr.class_ "navbar-nav" ]
                    [ Vdom.Node.li
                        ~attrs:
                          [ Vdom.Attr.class_ "nav-item"
                          ; Vdom.Attr.style
                              Css_gen.(
                                Css_gen.create ~field:"display" ~value:"flex"
                                @> Css_gen.create ~field:"align-items" ~value:"center")
                          ]
                        [ search_trigger ]
                    ]
                ]
            ]
        ]
    ]
;;

let footer ~year ~(current_user : Rpcs.Get_current_user.Response.t) ~log_out =
  let session_node =
    match current_user with
    | Not_logged_in ->
      Client_utils.link
        ~attrs:[ Vdom.Attr.class_ "nav-link" ]
        Login
        [ Vdom.Node.text "login" ]
    | Logged_in { username } ->
      Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.class_ "logout-button"
          ; Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"block")
          ; Vdom.Attr.on_click (fun _ -> log_out)
          ]
        [ Vdom.Node.text [%string "%{username} (logout)"] ]
  in
  Vdom.Node.create
    "nav"
    ~attrs:[ Vdom.Attr.class_ "footer" ]
    [ Vdom.Node.p [ Vdom.Node.text [%string "© 2024 - %{year#Int} Your Name"] ]
    ; session_node
    ]
;;

let set_title =
  Effect.of_sync_fun (fun title ->
    Js_of_ocaml.Dom_html.document##.title := Js_of_ocaml.Js.string title)
;;

let component =
  let%sub search_trigger, search_modal = Search_modal.component in
  let%sub () =
    Bonsai.Edge.on_change
      (module Route)
      Navigation.current
      ~callback:(Value.return (fun route -> set_title (Route.title route)))
  in
  let%sub page =
    match%sub Navigation.current with
    | Home -> Pages.home
    | About -> Pages.about
    | Post { slug } -> Pages.post_detail ~slug
    | New_post -> Editor.new_post
    | Edit_post { slug } -> Editor.edit_post ~slug
    | Posts { tag_slug; page } -> Pages.posts ~tag_slug ~page
    | Search { query; page } -> Pages.search ~query ~page
    | Login -> Login_page.component
  in
  let%sub now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_hr 1.) in
  let%sub current_user = Session.current_user in
  let%arr page = page
  and search_trigger = search_trigger
  and search_modal = search_modal
  and now = now
  and current_user = current_user in
  let year = Time_ns.to_date now ~zone:Client_utils.zone |> Date.year in
  let log_out =
    match%bind.Effect Session.log_out () with
    | Ok () -> Session.reload_home ()
    | Error _ -> Effect.Ignore
  in
  Vdom.Node.div
    [ navbar ~search_trigger; page; footer ~year ~current_user ~log_out; search_modal ]
;;
