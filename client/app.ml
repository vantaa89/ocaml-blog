open! Core
open! Import
open Bonsai.Let_syntax

let navbar ~search_trigger ~menu_open ~set_menu_open =
  let open Vdom in
  Node.create
    "nav"
    ~attrs:[ Attr.class_ "navbar" ]
    [ Node.div
        ~attrs:[ Attr.classes [ "custom-container"; "navbar-inner" ] ]
        [ Client_utils.link
            ~attrs:[ Attr.class_ "navbar-brand" ]
            Home
            (match String.lsplit2 Owner_profile.info.name ~on:' ' with
             | None -> [ Node.create "b" [ Node.text Owner_profile.info.name ] ]
             | Some (first, rest) ->
               (* The first word in bold *)
               [ Node.create "b" [ Node.text first ]; Node.text [%string " %{rest}"] ])
        ; Node.button
            ~attrs:
              [ Attr.class_ "navbar-toggler"
              ; Attr.type_ "button"
              ; Attr.create "aria-controls" "navbar-menu"
              ; Attr.create "aria-expanded" (Bool.to_string menu_open)
              ; Attr.create "aria-label" "Toggle navigation"
              ; Attr.on_click (fun _ -> set_menu_open (not menu_open))
              ]
            [ Node.span ~attrs:[ Attr.class_ "navbar-toggler-icon" ] [] ]
        ; Node.div
            ~attrs:
              [ Attr.classes
                  ("navbar-menu"
                   ::
                   (match menu_open with
                    | true -> [ "open" ]
                    | false -> []))
              ; Attr.id "navbar-menu"
              ]
            [ Node.ul
                ~attrs:[ Attr.class_ "navbar-nav" ]
                [ Node.li
                    ~attrs:[ Attr.class_ "nav-item" ]
                    [ Client_utils.link
                        ~attrs:[ Attr.class_ "nav-link" ]
                        Home
                        [ Node.text "home" ]
                    ]
                ; Node.li
                    ~attrs:[ Attr.class_ "nav-item" ]
                    [ Client_utils.link
                        ~attrs:[ Attr.class_ "nav-link" ]
                        (Posts { tag = None; page = 1 })
                        [ Node.text "posts" ]
                    ]
                ; Node.li
                    ~attrs:[ Attr.class_ "nav-item" ]
                    [ Client_utils.link
                        ~attrs:[ Attr.class_ "nav-link" ]
                        About
                        [ Node.text "about" ]
                    ]
                ]
            ; Node.span
                ~attrs:[ Attr.classes [ "nav-item"; "navbar-search" ] ]
                [ Node.ul
                    ~attrs:[ Attr.class_ "navbar-nav" ]
                    [ Node.li
                        ~attrs:
                          [ Attr.class_ "nav-item"
                          ; Attr.style
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
  let open Vdom in
  let session_node =
    match current_user with
    | Not_logged_in ->
      Client_utils.link ~attrs:[ Attr.class_ "nav-link" ] Login [ Node.text "login" ]
    | Logged_in { username } ->
      Node.button
        ~attrs:
          [ Attr.class_ "logout-button"
          ; Attr.style (Css_gen.create ~field:"display" ~value:"block")
          ; Attr.on_click (fun _ -> log_out)
          ]
        [ Node.text [%string "%{username} (logout)"] ]
  in
  Node.create
    "nav"
    ~attrs:[ Attr.class_ "footer" ]
    [ Node.p [ Node.text [%string "© 2024–%{year#Int} %{Owner_profile.info.name}"] ]
    ; session_node
    ]
;;

let set_title =
  Effect.of_sync_fun (fun title ->
    Js_of_ocaml.Dom_html.document##.title := Js_of_ocaml.Js.string title)
;;

let component =
  let%sub search_trigger, search_modal = Search_modal.component in
  let%sub page =
    match%sub Navigation.current with
    | Home -> Pages.home
    | About -> Pages.about
    | Post { slug } -> Pages.post_detail ~slug
    | New_post -> Editor.new_post
    | Edit_post { slug } -> Editor.edit_post ~slug
    | Posts { tag; page } -> Pages.posts ~tag ~page
    | Search { query; page } -> Pages.search ~query ~page
    | Login -> Login_page.component
    | Not_found _ -> Pages.not_found
  in
  let%sub title =
    let%arr page = page in
    match page.title with
    | None -> Owner_profile.info.name
    | Some title -> [%string "%{title} | %{Owner_profile.info.name}"]
  in
  let%sub () =
    Bonsai.Edge.on_change (module String) title ~callback:(Value.return set_title)
  in
  let%sub now = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_hr 1.) in
  let%sub current_user = Session.current_user in
  let%sub menu_open, set_menu_open = Bonsai.state (module Bool) ~default_model:false in
  let%arr { view; _ } = page
  and search_trigger = search_trigger
  and menu_open = menu_open
  and set_menu_open = set_menu_open
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
    [ navbar ~search_trigger ~menu_open ~set_menu_open
    ; view
    ; footer ~year ~current_user ~log_out
    ; search_modal
    ]
;;
