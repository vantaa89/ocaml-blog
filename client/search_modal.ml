open! Core
open! Import
open Bonsai.Let_syntax
open! Js_of_ocaml

let debounce_span = Time_ns.Span.of_ms 300.
let sleep = Effect.of_deferred_fun Async_kernel.Clock_ns.after
let minimum_query_length = 2

module Model = struct
  type t =
    { query : string
    ; generation : int
    ; debounced : string
    }
  [@@deriving sexp, equal]

  let default = { query = ""; generation = 0; debounced = "" }
end

module Action = struct
  type t =
    | Set_query of string
    | Commit of int
  [@@deriving sexp_of]
end

let is_apple_device () =
  let matches string =
    List.exists [ "Mac"; "iPhone"; "iPad"; "iPod" ] ~f:(fun pattern ->
      String.is_substring string ~substring:pattern)
  in
  matches (Js.to_string Dom_html.window##.navigator##.platform)
  || matches (Js.to_string Dom_html.window##.navigator##.userAgent)
;;

let shortcut_text =
  lazy
    (match is_apple_device () with
     | true -> "⌘K"
     | false -> "Ctrl+K")
;;

let highlight text ~query =
  let open Vdom in
  match String.is_empty query with
  | true -> [ Node.text text ]
  | false ->
    let haystack = String.lowercase text in
    let needle = String.lowercase query in
    let needle_length = String.length needle in
    let rec loop pos acc =
      match String.substr_index haystack ~pos ~pattern:needle with
      | None -> List.rev (Node.text (String.subo text ~pos) :: acc)
      | Some index ->
        let before = String.sub text ~pos ~len:(index - pos) in
        let matched = String.sub text ~pos:index ~len:needle_length in
        loop
          (index + needle_length)
          (Node.create "mark" [ Node.text matched ] :: Node.text before :: acc)
    in
    loop 0 []
;;

let result_node
      ({ title; slug; excerpt; thumbnail = _; created_at; tags; languages = _ } :
        Rpcs.Post_summary.t)
      ~query
      ~selected
      ~on_select
  =
  let open Vdom in
  Node.div
    ~attrs:
      [ Attr.classes
          ("search-result-item"
           ::
           (match selected with
            | true -> [ "selected" ]
            | false -> []))
      ; Attr.create "data-url" (Route.to_string (Post { slug }))
      ; Attr.on_click (fun _ -> on_select)
      ]
    [ Node.div ~attrs:[ Attr.class_ "search-result-title" ] (highlight title ~query)
    ; Node.div ~attrs:[ Attr.class_ "search-result-excerpt" ] (highlight excerpt ~query)
    ; Node.div
        ~attrs:[ Attr.class_ "search-result-meta" ]
        (Node.span
           ~attrs:[ Attr.class_ "search-result-date" ]
           [ Node.text (Client_utils.format_time created_at) ]
         :: List.map tags ~f:(fun tag ->
           Node.span ~attrs:[ Attr.class_ "search-result-tag" ] [ Node.text tag ]))
    ]
;;

let results_computation ~debounced =
  let%sub should_search =
    let%arr debounced = debounced in
    String.length debounced >= minimum_query_length
  in
  match%sub should_search with
  | false -> Bonsai.const None
  | true ->
    let%sub query =
      let%arr debounced = debounced in
      { Rpcs.Search_posts.Query.query = debounced }
    in
    let%sub poll =
      Rpc_effect.Rpc.poll_until_ok
        (module Rpcs.Search_posts.Query)
        (module Rpcs.Search_posts.Response)
        Rpcs.Search_posts.rpc
        ~where_to_connect:Rpc_client.where_to_connect
        ~retry_interval:Rpc_client.retry_interval
        query
    in
    let%arr poll = poll in
    Option.map poll.last_ok_response ~f:snd
;;

(* The input element is always in the DOM (the modal is hidden with [display: none]), so
   opening the modal has to move focus explicitly. *)
let focus_search_input =
  let focus () =
    Js.Opt.iter
      (Dom_html.document##getElementById (Js.string "search-input"))
      (fun element ->
         Js.Opt.iter (Dom_html.CoerceTo.input element) (fun input ->
           input##focus;
           input##select))
  in
  (* Wait for the browser to paint, since [focus] is a no-op on a hidden element. *)
  Effect.of_deferred_fun (fun () ->
    Async_kernel.Deferred.map
      (Async_kernel.Clock_ns.after (Time_ns.Span.of_ms 1.))
      ~f:focus)
;;

let component =
  let%sub is_open, set_is_open = Bonsai.state (module Bool) ~default_model:false in
  let%sub model, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)
      ~default_model:Model.default
      ~apply_action:(fun ~inject ~schedule_event model action ->
        match action with
        | Set_query query ->
          let generation = model.generation + 1 in
          schedule_event
            (let%bind.Effect () = sleep debounce_span in
             inject (Commit generation));
          { model with query; generation }
        | Commit generation ->
          (match Int.equal generation model.generation with
           | true -> { model with debounced = model.query }
           | false -> model))
  in
  let%sub selected, set_selected = Bonsai.state (module Int) ~default_model:(-1) in
  let%sub debounced =
    let%arr model = model in
    model.debounced
  in
  let%sub () =
    Bonsai.Edge.on_change
      (module String)
      debounced
      ~callback:
        (let%map set_selected = set_selected in
         fun (_ : string) -> set_selected (-1))
  in
  let%sub results = results_computation ~debounced in
  let%sub open_modal =
    let%arr set_is_open = set_is_open in
    Effect.Many [ set_is_open true; focus_search_input () ]
  in
  let%sub close_modal =
    let%arr set_is_open = set_is_open
    and inject = inject
    and set_selected = set_selected in
    Effect.Many [ set_is_open false; inject (Set_query ""); set_selected (-1) ]
  in
  let%sub () =
    let%sub on_activate =
      let%arr open_modal = open_modal
      and close_modal = close_modal in
      Effect.of_sync_fun
        (fun () ->
           let (_ : Dom_html.event_listener_id) =
             Dom_html.addEventListener
               Dom_html.document
               Dom_html.Event.keydown
               (Dom_html.handler (fun event ->
                  let key =
                    Js.Optdef.to_option event##.key |> Option.map ~f:Js.to_string
                  in
                  let modifier =
                    Js.to_bool event##.ctrlKey || Js.to_bool event##.metaKey
                  in
                  match key, modifier with
                  | Some ("k" | "K"), true ->
                    Dom.preventDefault event;
                    Effect.Expert.handle_non_dom_event_exn open_modal;
                    Js._false
                  | Some "Escape", _ ->
                    Effect.Expert.handle_non_dom_event_exn close_modal;
                    Js._false
                  | _, _ -> Js._true))
               Js._false
           in
           ())
        ()
    in
    Bonsai.Edge.lifecycle ~on_activate ()
  in
  let%arr is_open = is_open
  and model = model
  and selected = selected
  and results = results
  and inject = inject
  and set_selected = set_selected
  and open_modal = open_modal
  and close_modal = close_modal in
  let shortcut = force shortcut_text in
  let visible_results = Option.value results ~default:[] in
  let close_and_go_to route = Effect.Many [ close_modal; Navigation.go_to route ] in
  let query_length =
    match String.length model.debounced with
    | 0 -> `Empty
    | length ->
      (match length < minimum_query_length with
       | true -> `Too_short
       | false -> `Long_enough)
  in
  let results_node =
    let open Vdom in
    match query_length, results with
    | `Empty, _ ->
      [ Node.div
          ~attrs:[ Attr.class_ "search-placeholder" ]
          [ Node.text "Start typing to search..." ]
      ]
    | `Too_short, _ ->
      [ Node.div
          ~attrs:[ Attr.class_ "search-placeholder" ]
          [ Node.text "Type at least 2 characters..." ]
      ]
    | `Long_enough, None ->
      [ Node.div ~attrs:[ Attr.class_ "search-loading" ] [ Node.text "Searching..." ] ]
    | `Long_enough, Some [] ->
      [ Node.div
          ~attrs:[ Attr.class_ "search-no-results" ]
          [ Node.text "No results found" ]
      ]
    | `Long_enough, Some posts ->
      List.mapi posts ~f:(fun index (post : Rpcs.Post_summary.t) ->
        result_node
          post
          ~query:model.debounced
          ~selected:(Int.equal index selected)
          ~on_select:(close_and_go_to (Post { slug = post.slug })))
      @ [ Node.div
            ~attrs:[ Attr.class_ "search-show-all" ]
            [ Client_utils.link
                ~attrs:[ Attr.class_ "plain-link" ]
                (Search { query = model.debounced; page = 1 })
                [ Node.text "Show all results →" ]
            ]
        ]
  in
  let on_input_keydown event =
    let key = Js.Optdef.to_option event##.key |> Option.map ~f:Js.to_string in
    let count = List.length visible_results in
    match key with
    | Some "ArrowDown" ->
      Effect.Many
        [ Effect.Prevent_default; set_selected (Int.min (selected + 1) (count - 1)) ]
    | Some "ArrowUp" ->
      Effect.Many [ Effect.Prevent_default; set_selected (Int.max (selected - 1) (-1)) ]
    | Some "Enter" ->
      let target =
        match List.nth visible_results selected with
        | Some (post : Rpcs.Post_summary.t) -> Some (Route.Post { slug = post.slug })
        | None ->
          (match String.is_empty (String.strip model.query) with
           | true -> None
           | false -> Some (Route.Search { query = String.strip model.query; page = 1 }))
      in
      Effect.Many
        (Effect.Prevent_default
         ::
         (match target with
          | None -> []
          | Some route -> [ close_and_go_to route ]))
    | _ -> Effect.Ignore
  in
  let modal =
    let open Vdom in
    Node.div
      ~attrs:
        [ Attr.id "search-modal"
        ; Attr.class_ "search-modal"
        ; Attr.style
            (Css_gen.create
               ~field:"display"
               ~value:
                 (match is_open with
                  | true -> "flex"
                  | false -> "none"))
        ; Attr.on_click (fun event ->
            (* Only a click on the backdrop itself closes the modal. *)
            let clicked_backdrop =
              Js.Opt.case
                event##.target
                (fun () -> false)
                (fun target ->
                   Js.Opt.case
                     event##.currentTarget
                     (fun () -> false)
                     (fun current -> phys_equal target current))
            in
            match clicked_backdrop with
            | true -> close_modal
            | false -> Effect.Ignore)
        ]
      [ Node.div
          ~attrs:[ Attr.class_ "search-modal-content" ]
          [ Node.div
              ~attrs:[ Attr.class_ "search-input-container" ]
              [ Node.input
                  ~attrs:
                    [ Attr.type_ "text"
                    ; Attr.id "search-input"
                    ; Attr.placeholder "Search posts..."
                    ; Attr.value_prop model.query
                    ; Attr.autofocus true
                    ; Attr.on_input (fun _ query -> inject (Set_query query))
                    ; Attr.on_keydown on_input_keydown
                    ]
                  ()
              ; Node.span
                  ~attrs:[ Attr.class_ "search-shortcut"; Attr.id "modal-shortcut" ]
                  [ Node.text shortcut ]
              ]
          ; Node.div
              ~attrs:[ Attr.id "search-results"; Attr.class_ "search-results" ]
              results_node
          ]
      ]
  in
  let trigger =
    let open Vdom in
    Node.button
      ~attrs:
        [ Attr.class_ "nav-link search-trigger"
        ; Attr.style (Css_gen.create ~field:"cursor" ~value:"pointer")
        ; Attr.on_click (fun _ -> open_modal)
        ]
      [ Node.img
          ~attrs:
            [ Attr.src "/static/icons/magnifier.png"
            ; Attr.style
                Css_gen.(height (`Px 16) @> width (`Px 16) @> margin_right (`Px 6))
            ]
          ()
      ; Node.span ~attrs:[ Attr.class_ "search-text-desktop" ] [ Node.text "Press " ]
      ; Node.span
          ~attrs:[ Attr.class_ "search-shortcut-nav"; Attr.id "nav-shortcut" ]
          [ Node.text shortcut ]
      ; Node.span ~attrs:[ Attr.class_ "search-text-desktop" ] [ Node.text " to search" ]
      ; Node.span ~attrs:[ Attr.class_ "search-text-mobile" ] [ Node.text "Search" ]
      ]
  in
  trigger, modal
;;
