open! Core
open! Import
open Bonsai.Let_syntax

module Model = struct
  type t =
    { username : string
    ; password : string
    ; rejected : bool
    }
  [@@deriving sexp, equal]

  let empty = { username = ""; password = ""; rejected = false }
end

module Action = struct
  type t =
    | Set_username of string
    | Set_password of string
    | Rejected
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
        | Set_username username -> { model with username; rejected = false }
        | Set_password password -> { model with password; rejected = false }
        | Rejected -> { model with rejected = true })
  in
  let%arr model = model
  and inject = inject in
  let submit =
    match%bind.Effect
      Session.log_in ~username:model.username ~password:model.password
    with
    | Ok () -> Session.reload_home ()
    | Error (_ : Error.t) -> inject Rejected
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
        ; (match model.rejected with
           | false -> Vdom.Node.none
           | true ->
             Vdom.Node.div
               ~attrs:[ Vdom.Attr.classes [ "alert"; "alert-danger" ] ]
               [ Vdom.Node.text "Invalid username or password" ])
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.type_ "submit"; Vdom.Attr.classes [ "btn"; "btn-primary" ] ]
            [ Vdom.Node.text "Log in" ]
        ]
    ]
;;
