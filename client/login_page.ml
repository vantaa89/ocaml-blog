open! Core
open! Import
open Bonsai.Let_syntax

module Model = struct
  type t =
    { username : string
    ; password : string
    ; error : string option
    }
  [@@deriving sexp, equal]

  let empty = { username = ""; password = ""; error = None }
end

module Action = struct
  type t =
    | Set_username of string
    | Set_password of string
    | Failed of string
  [@@deriving sexp_of]
end

let field ~id ~label ~type_ ~value ~on_input =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "mb-3" ]
    [ Vdom.Node.label
        ~attrs:[ Vdom.Attr.class_ "form-label"; Vdom.Attr.for_ id ]
        [ Vdom.Node.text label ]
    ; Vdom.Node.input
        ~attrs:
          [ Vdom.Attr.type_ type_
          ; Vdom.Attr.class_ "form-control"
          ; Vdom.Attr.id id
          ; Vdom.Attr.name id
          ; Vdom.Attr.value_prop value
          ; Vdom.Attr.on_input (fun _ value -> on_input value)
          ]
        ()
    ]
;;

let component =
  let%sub model, inject =
    Bonsai.state_machine0
      (module Model)
      (module Action)
      ~default_model:Model.empty
      ~apply_action:(fun ~inject:_ ~schedule_event:_ model action ->
        match action with
        | Set_username username -> { model with username; error = None }
        | Set_password password -> { model with password; error = None }
        | Failed error -> { model with error = Some error })
  in
  let%arr model = model
  and inject = inject in
  let submit =
    match%bind.Effect
      Session.log_in ~username:model.username ~password:model.password
    with
    | `Logged_in -> Session.reload_home ()
    | `Rejected -> inject (Failed "Invalid username or password")
    | `Too_many_attempts ->
      inject (Failed "Too many login attempts. Please try again later.")
    | `Failed (_ : Error.t) -> inject (Failed "Could not log in. Please try again.")
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "container"; "custom-container"; "py-3" ] ]
    [ Vdom.Node.h1 [ Vdom.Node.text "Log in" ]
    ; Vdom.Node.create
        "form"
        ~attrs:
          [ Vdom.Attr.on_submit (fun _ -> Effect.Many [ Effect.Prevent_default; submit ])
          ]
        [ field
            ~id:"username"
            ~label:"Username"
            ~type_:"text"
            ~value:model.username
            ~on_input:(fun username -> inject (Set_username username))
        ; field
            ~id:"password"
            ~label:"Password"
            ~type_:"password"
            ~value:model.password
            ~on_input:(fun password -> inject (Set_password password))
        ; (match model.error with
           | None -> Vdom.Node.none
           | Some error ->
             Vdom.Node.div
               ~attrs:[ Vdom.Attr.classes [ "alert"; "alert-danger" ] ]
               [ Vdom.Node.text error ])
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.type_ "submit"; Vdom.Attr.classes [ "btn"; "btn-primary" ] ]
            [ Vdom.Node.text "Log in" ]
        ]
    ]
;;
