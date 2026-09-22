open! Core
open! Import
open Bonsai.Let_syntax

module Mode = struct
  type t =
    | New
    | Edit of { slug : string }
end

module Status = struct
  type t =
    | Editing
    | Saving
    | Failed of string
  [@@deriving sexp, equal]
end

module Model = struct
  type t =
    { form : Rpcs.Post_form.t option
      (* [None] until the author changes something, so that the form follows the post it
         was opened on. *)
    ; slug_edited : bool
    ; language : Language.t option
    ; status : Status.t
    ; uploading : bool
    }
  [@@deriving sexp, equal]

  let default =
    { form = None
    ; slug_edited = false
    ; language = None
    ; status = Editing
    ; uploading = false
    }
  ;;
end

module Action = struct
  type t =
    | Set_title of string
    | Set_slug of string
    | Set_content of string
    | Set_special_post of bool
    | Select_language of Language.t
    | Set_status of Status.t
    | Set_uploading of bool
    | Reset
  [@@deriving sexp_of]
end

let empty_form : Rpcs.Post_form.t =
  { title = ""; slug = ""; content = Language.Map.empty; special_post = false }
;;

let form_of_post ({ title; slug; content; special_post; _ } : Rpcs.Post.t)
  : Rpcs.Post_form.t
  =
  { title; slug; content; special_post }
;;

(* Non-ASCII bytes are kept, so a Korean title still yields a readable slug. *)
let slugify title =
  String.lowercase title
  |> String.map ~f:(fun char ->
    match Char.is_alphanum char || Char.to_int char >= 128 with
    | true -> char
    | false -> '-')
  |> String.split ~on:'-'
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:"-"
;;

let current_form (model : Model.t) ~initial = Option.value model.form ~default:initial

let current_language (model : Model.t) ~(initial : Rpcs.Post_form.t) : Language.t =
  match model.language with
  | Some language -> language
  | None ->
    (match Map.min_elt initial.content with
     | None -> English
     | Some (language, _) -> language)
;;

let apply_action
      ~inject:_
      ~schedule_event:_
      (input : (Mode.t * Rpcs.Post_form.t) Bonsai.Computation_status.t)
      (model : Model.t)
      (action : Action.t)
  : Model.t
  =
  match input, action with
  | _, Reset -> Model.default
  | _, Set_uploading uploading -> { model with uploading }
  | Inactive, _ -> model
  | Active (mode, initial), action ->
    let form = current_form model ~initial in
    let edit (form : Rpcs.Post_form.t) : Model.t =
      let status : Status.t =
        match model.status with
        | Failed _ -> Editing
        | (Editing | Saving) as status -> status
      in
      { model with form = Some form; status }
    in
    (match action with
     | Set_title title ->
       let slug =
         match mode, model.slug_edited with
         | New, false -> slugify title
         | New, true | Edit _, _ -> form.slug
       in
       edit { form with title; slug }
     | Set_slug slug -> { (edit { form with slug }) with slug_edited = true }
     | Set_content text ->
       let language = current_language model ~initial in
       let content =
         match String.is_empty text with
         | true -> Map.remove form.content language
         | false -> Map.set form.content ~key:language ~data:text
       in
       edit { form with content }
     | Set_special_post special_post -> edit { form with special_post }
     | Select_language language -> { model with language = Some language }
     | Set_status status -> { model with status }
     | Set_uploading uploading -> { model with uploading }
     | Reset -> Model.default)
;;

let problem (form : Rpcs.Post_form.t) =
  match
    ( String.is_empty (String.strip form.title)
    , String.is_empty form.slug
    , Map.is_empty form.content )
  with
  | true, _, _ -> Some "The post needs a title."
  | false, true, _ -> Some "The post needs a slug."
  | false, false, true -> Some "Write the post in at least one language."
  | false, false, false -> None
;;

let unreachable = "Could not reach the server. Please try again."
let expired = "Your session has expired. Log in again, then save."
let duplicate_slug = "Another post already uses this slug."

let upload_failure_message : Image_upload.Failure.t -> string = function
  | Too_large ->
    let megabytes = Rpcs.Upload_image.max_size / 1024 / 1024 in
    [%string "An image can be at most %{megabytes#Int} MB."]
  | Unsupported_format -> "Only PNG, JPEG, GIF and WebP images can be uploaded."
  | Unreadable { name } -> [%string "Could not read %{name}."]
  | Not_logged_in -> "Your session has expired. Log in again, then upload."
  | Unreachable -> unreachable
;;

let textarea_id = "editor-input"
let go_back = Effect.of_sync_fun (fun () -> Js_of_ocaml.Dom_html.window##.history##back)

let language_toggle ~(language : Language.t) ~inject =
  let button (target : Language.t) label =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.type_ "button"
        ; Vdom.Attr.classes
            ("language-btn"
             ::
             (match Language.equal target language with
              | true -> [ "active" ]
              | false -> []))
        ; Vdom.Attr.on_click (fun _ -> inject (Action.Select_language target))
        ]
      [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "language-toggle" ]
    [ button English "EN"; button Korean "한" ]
;;

let view
      ~(model : Model.t)
      ~(form : Rpcs.Post_form.t)
      ~(language : Language.t)
      ~preview
      ~save
      ~upload
      ~inject
  =
  let text = Map.find form.content language |> Option.value ~default:"" in
  let on_keydown (event : Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t) =
    let command =
      Js_of_ocaml.Js.to_bool event##.ctrlKey || Js_of_ocaml.Js.to_bool event##.metaKey
    in
    match command, Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
    | true, KeyS -> Effect.Many [ Effect.Prevent_default; save ]
    | _, _ -> Effect.Ignore
  in
  let saving =
    match model.status with
    | Saving -> true
    | Editing | Failed _ -> false
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "editor"; Vdom.Attr.on_keydown on_keydown ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "editor-compose" ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.type_ "text"
              ; Vdom.Attr.class_ "editor-title"
              ; Vdom.Attr.placeholder "Title"
              ; Vdom.Attr.value_prop form.title
              ; Vdom.Attr.on_input (fun _ title -> inject (Action.Set_title title))
              ]
            ()
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "editor-meta" ]
            [ Vdom.Node.label
                ~attrs:[ Vdom.Attr.class_ "editor-slug" ]
                [ Vdom.Node.text "/post/"
                ; Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.type_ "text"
                      ; Vdom.Attr.placeholder "slug"
                      ; Vdom.Attr.value_prop form.slug
                      ; Vdom.Attr.on_input (fun _ slug -> inject (Action.Set_slug slug))
                      ]
                    ()
                ]
            ; Vdom.Node.label
                ~attrs:[ Vdom.Attr.class_ "editor-check" ]
                [ Vdom.Node.input
                    ~attrs:
                      [ Vdom.Attr.type_ "checkbox"
                      ; Vdom.Attr.bool_property "checked" form.special_post
                      ; Vdom.Attr.on_change (fun _ _ ->
                          inject (Action.Set_special_post (not form.special_post)))
                      ]
                    ()
                ; Vdom.Node.text "special post"
                ]
            ; Image_upload.button ~uploading:model.uploading ~upload
            ; Vdom.Node.div
                ~attrs:[ Vdom.Attr.class_ "editor-actions" ]
                [ language_toggle ~language ~inject ]
            ]
        ; (match model.status with
           | Failed message ->
             Vdom.Node.p
               ~attrs:[ Vdom.Attr.class_ "editor-error" ]
               [ Vdom.Node.text message ]
           | Editing | Saving -> Vdom.Node.none)
        ; Vdom.Node.textarea
            ~attrs:
              [ Vdom.Attr.class_ "editor-input"
              ; Vdom.Attr.id textarea_id
              ; Vdom.Attr.placeholder
                  (match language with
                   | English -> "Write in English (markdown)"
                   | Korean -> "한국어로 쓰기 (markdown)")
              ; Vdom.Attr.create "spellcheck" "false"
              ; Vdom.Attr.value_prop text
              ; Vdom.Attr.on_input (fun _ text -> inject (Action.Set_content text))
              ; Image_upload.drop_target ~upload
              ]
            []
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "editor-footer" ]
            [ Vdom.Node.button
                ~attrs:
                  [ Vdom.Attr.type_ "button"
                  ; Vdom.Attr.class_ "editor-back"
                  ; Vdom.Attr.on_click (fun _ -> go_back ())
                  ]
                [ Vdom.Node.text "← Back" ]
            ; Vdom.Node.button
                ~attrs:
                  ([ Vdom.Attr.type_ "button"
                   ; Vdom.Attr.class_ "editor-save"
                   ; Vdom.Attr.on_click (fun _ -> save)
                   ]
                   @
                   match saving with
                   | true -> [ Vdom.Attr.disabled ]
                   | false -> [])
                [ Vdom.Node.text
                    (match saving with
                     | true -> "Saving…"
                     | false -> "Save")
                ]
            ]
        ]
    ; Vdom.Node.inner_html
        ~tag:"div"
        ~attrs:[ Vdom.Attr.classes [ "editor-preview"; "post-detail-content" ] ]
        ~this_html_is_sanitized_and_is_totally_safe_trust_me:preview
        ()
    ]
;;

let editor ~(mode : Mode.t Value.t) ~(initial : Rpcs.Post_form.t Value.t) =
  let%sub input =
    let%arr mode = mode
    and initial = initial in
    mode, initial
  in
  let%sub model, inject =
    Bonsai.state_machine1
      (module Model)
      (module Action)
      ~default_model:Model.default
      ~apply_action
      input
  in
  let%sub form =
    let%arr model = model
    and initial = initial in
    current_form model ~initial
  in
  let%sub language =
    let%arr model = model
    and initial = initial in
    current_language model ~initial
  in
  let%sub preview =
    let%arr form = form
    and language = language in
    match Map.find form.content language with
    | None -> ""
    | Some markdown -> Markdown_renderer.render ~markdown
  in
  let%sub () = Client_utils.rerender_math_on_change preview in
  let%sub create_post =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Create_post.rpc
      ~where_to_connect:Rpc_client.where_to_connect
  in
  let%sub update_post =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Update_post.rpc
      ~where_to_connect:Rpc_client.where_to_connect
  in
  let%sub upload_image =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Upload_image.rpc
      ~where_to_connect:Rpc_client.where_to_connect
  in
  let%arr model = model
  and inject = inject
  and form = form
  and language = language
  and preview = preview
  and mode = mode
  and create_post = create_post
  and update_post = update_post
  and upload_image = upload_image in
  let submit : (unit, string) Result.t Effect.t =
    match mode with
    | New ->
      let%map.Effect response = create_post form in
      (match response with
       | Ok Saved -> Ok ()
       | Ok Duplicate_slug -> Error duplicate_slug
       | Ok Not_logged_in -> Error expired
       | Error (_ : Error.t) -> Error unreachable)
    | Edit { slug } ->
      let%map.Effect response = update_post { slug; form } in
      (match response with
       | Ok Saved -> Ok ()
       | Ok Duplicate_slug -> Error duplicate_slug
       | Ok Not_logged_in -> Error expired
       | Ok Not_found -> Error "This post no longer exists."
       | Error (_ : Error.t) -> Error unreachable)
  in
  let save =
    match model.status, problem form with
    | Saving, _ -> Effect.Ignore
    | (Editing | Failed _), Some problem -> inject (Set_status (Failed problem))
    | (Editing | Failed _), None ->
      let%bind.Effect () = inject (Set_status Saving) in
      (match%bind.Effect submit with
       | Ok () ->
         let%bind.Effect () = inject Reset in
         Navigation.go_to (Post { slug = form.slug })
       | Error message -> inject (Set_status (Failed message)))
  in
  let upload files =
    match model.uploading with
    | true -> Effect.Ignore
    | false ->
      let%bind.Effect () = inject (Set_uploading true) in
      let%bind.Effect results =
        Effect.all (List.map files ~f:(Image_upload.upload ~dispatch:upload_image))
      in
      let urls, failures = List.partition_result results in
      let%bind.Effect () =
        match urls with
        | [] -> Effect.Ignore
        | urls ->
          (match%bind.Effect Image_upload.insert ~textarea_id ~urls with
           | None -> Effect.Ignore
           | Some text -> inject (Set_content text))
      in
      let%bind.Effect () =
        match failures with
        | [] -> Effect.Ignore
        | failure :: _ -> inject (Set_status (Failed (upload_failure_message failure)))
      in
      inject (Set_uploading false)
  in
  view ~model ~form ~language ~preview ~save ~upload ~inject
;;

let for_author page =
  let%sub current_user = Session.current_user in
  match%sub current_user with
  | Logged_in _ -> page
  | Not_logged_in ->
    Bonsai.const
      (Vdom.Node.div
         ~attrs:[ Vdom.Attr.classes [ "custom-container"; "my-3" ] ]
         [ Vdom.Node.p
             [ Vdom.Node.text "Only the author can write posts. "
             ; Client_utils.link Login [ Vdom.Node.text "Log in" ]
             ]
         ])
;;

let new_post =
  for_author (editor ~mode:(Value.return Mode.New) ~initial:(Value.return empty_form))
;;

let edit_post ~slug =
  for_author
    (let%sub poll =
       let%sub query =
         let%arr slug = slug in
         { Rpcs.Get_post.Query.slug }
       in
       Rpc_effect.Rpc.poll_until_ok
         (module Rpcs.Get_post.Query)
         (module Rpcs.Get_post.Response)
         Rpcs.Get_post.rpc
         ~where_to_connect:Rpc_client.where_to_connect
         ~retry_interval:Rpc_client.retry_interval
         query
     in
     let%sub post =
       let%arr poll = poll in
       Option.map poll.last_ok_response ~f:snd
     in
     match%sub post with
     | None ->
       let%arr poll = poll in
       Client_utils.of_poll poll ~f:(fun (_ : Rpcs.Get_post.Response.t) -> Vdom.Node.none)
     | Some None -> Bonsai.const Client_utils.not_found_node
     | Some (Some post) ->
       let%sub mode =
         let%arr slug = slug in
         Mode.Edit { slug }
       in
       let%sub initial =
         let%arr post = post in
         form_of_post post
       in
       Bonsai.scope_model (module String) ~on:slug (editor ~mode ~initial))
;;
