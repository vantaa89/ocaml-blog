open! Core
open! Import
open Bonsai.Let_syntax

let words_per_minute = 150
let zone = Time_float.Zone.of_utc_offset_explicit_name ~name:"KST" ~hours:9

let format_time time =
  (* Display only the date *)
  let date = Time_ns.to_date time ~zone in
  let month = Date.month date |> Month.to_string in
  let day = Date.day date in
  let year = Date.year date in
  [%string "%{month} %{day#Int}, %{year#Int}"]
;;

let read_time markdown =
  let words =
    String.split_on_chars markdown ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.count ~f:(Fn.non String.is_empty)
  in
  let minutes = Int.max 1 ((words + words_per_minute - 1) / words_per_minute) in
  [%string "%{minutes#Int} min read"]
;;

let link ?(attrs = []) route children =
  Vdom.Node.a
    ~attrs:
      (Vdom.Attr.href (Route.to_string route)
       :: Vdom.Attr.on_click (fun _ ->
         Vdom.Effect.Many [ Vdom.Effect.Prevent_default; Navigation.go_to route ])
       :: attrs)
    children
;;

let placeholder_thumbnail = "/static/placeholder.jpeg"

let tag_node tag =
  link
    ~attrs:[ Vdom.Attr.class_ "tag" ]
    (Posts { tag = Some tag; page = 1 })
    [ Vdom.Node.img
        ~attrs:
          [ Vdom.Attr.src "/static/icons/tag.png"
          ; Vdom.Attr.style Css_gen.(height (`Px 12) @> width (`Px 12))
          ]
        ()
    ; Vdom.Node.text [%string " %{tag}"]
    ]
;;

let post_card
      ({ title; slug; excerpt = _; thumbnail; created_at; tags; languages } :
        Rpcs.Post_summary.t)
  =
  let route : Route.t = Post { slug } in
  let languages = List.map languages ~f:Language.to_code |> String.concat ~sep:"/" in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "post-card" ]
    ([ link
         route
         [ Vdom.Node.div
             ~attrs:[ Vdom.Attr.class_ "post-card-image" ]
             [ Vdom.Node.img
                 ~attrs:
                   [ Vdom.Attr.src (Option.value thumbnail ~default:placeholder_thumbnail)
                   ]
                 ()
             ]
         ]
     ; link ~attrs:[ Vdom.Attr.class_ "post-title" ] route [ Vdom.Node.text title ]
     ; Vdom.Node.p
         ~attrs:[ Vdom.Attr.class_ "post-time" ]
         [ Vdom.Node.text [%string "%{format_time created_at} · 🌐︎ %{languages}"] ]
     ]
     @ List.map tags ~f:tag_node)
;;

let not_found_node =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "custom-container my-3" ]
    [ Vdom.Node.h1 [ Vdom.Node.text "Not found" ] ]
;;

let of_poll (poll : (_, 'response) Rpc_effect.Poll_result.t) ~f =
  match poll.last_ok_response with
  | None ->
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "custom-container my-3" ]
      [ Vdom.Node.p [ Vdom.Node.text "Loading…" ] ]
  | Some (_query, response) -> f response
;;

let paginator ~route ~page ~num_pages =
  (* A missing arrow keeps its place, so that the page number does not move. *)
  let arrow ~target ~label text =
    match target >= 1 && target <= num_pages with
    | false ->
      Vdom.Node.li
        ~attrs:[ Vdom.Attr.classes [ "paginator-item"; "hidden" ] ]
        [ Vdom.Node.text text ]
    | true ->
      Vdom.Node.li
        ~attrs:[ Vdom.Attr.class_ "paginator-item" ]
        [ link
            ~attrs:[ Vdom.Attr.class_ "page-link"; Vdom.Attr.create "aria-label" label ]
            (Route.with_page route target)
            [ Vdom.Node.text text ]
        ]
  in
  let current =
    Vdom.Node.li
      ~attrs:[ Vdom.Attr.classes [ "paginator-item"; "active" ] ]
      [ Vdom.Node.text (Int.to_string page) ]
  in
  match num_pages > 1 with
  | false -> Vdom.Node.none
  | true ->
    Vdom.Node.create
      "nav"
      [ Vdom.Node.ul
          ~attrs:[ Vdom.Attr.class_ "paginator justify-content-center" ]
          [ arrow ~target:(page - 1) ~label:"Previous page" "◂"
          ; current
          ; arrow ~target:(page + 1) ~label:"Next page" "▸"
          ]
      ]
;;

let render_math_in_body () =
  let render_math_in_element = Js_of_ocaml.Js.Unsafe.global##.renderMathInElement in
  match Js_of_ocaml.Js.Optdef.test render_math_in_element with
  | false -> ()
  | true ->
    let delimiter ~left ~right ~display =
      Js_of_ocaml.Js.Unsafe.obj
        [| "left", Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string left)
         ; "right", Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.string right)
         ; "display", Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.bool display)
        |]
    in
    let options =
      Js_of_ocaml.Js.Unsafe.obj
        [| ( "delimiters"
           , Js_of_ocaml.Js.Unsafe.inject
               (Js_of_ocaml.Js.array
                  [| delimiter ~left:"\\[" ~right:"\\]" ~display:true
                   ; delimiter ~left:"\\(" ~right:"\\)" ~display:false
                  |]) )
         ; "throwOnError", Js_of_ocaml.Js.Unsafe.inject (Js_of_ocaml.Js.bool false)
        |]
    in
    ignore
      (Js_of_ocaml.Js.Unsafe.fun_call
         render_math_in_element
         [| Js_of_ocaml.Js.Unsafe.inject Js_of_ocaml.Dom_html.document##.body
          ; Js_of_ocaml.Js.Unsafe.inject options
         |]
       : Js_of_ocaml.Js.Unsafe.any)
;;

let highlight_code_in_body () =
  (* Refer to highlight.js as described in
     https://highlightjs.readthedocs.io/en/latest/api.html *)
  let open Js_of_ocaml in
  let hljs = Js.Unsafe.global##.hljs in
  match Js.Optdef.test hljs with
  | false -> ()
  | true ->
    Dom_html.document##querySelectorAll (Js.string "pre code")
    |> Dom.list_of_nodeList
    |> List.iter ~f:(fun element ->
      ignore (Js.Unsafe.meth_call hljs "highlightElement" [| Js.Unsafe.inject element |]))
;;

let rerender_on_change html =
  let%sub rendered, set_rendered = Bonsai.state_opt (module String) in
  let%sub callback =
    let%arr html = html
    and rendered = rendered
    and set_rendered = set_rendered in
    match [%equal: string option] rendered (Some html) with
    | true -> None
    | false ->
      Some
        (let%bind.Effect () = set_rendered (Some html) in
         Effect.Many
           [ Effect.of_sync_fun render_math_in_body ()
           ; Effect.of_sync_fun highlight_code_in_body ()
           ])
  in
  Bonsai.Edge.after_display' callback
;;

(** [Post.content] is a map from language to markdown; English is the default. *)
let primary_content (content : string Map.M(Language).t) =
  match Map.find content English with
  | Some content -> Some content
  | None -> Map.data content |> List.hd
;;

let page_size = 10

let paginate items ~page =
  List.drop items ((page - 1) * page_size) |> Fn.flip List.take page_size
;;
