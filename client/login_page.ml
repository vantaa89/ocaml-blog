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

let field ~id ~label ~type_ ~autocomplete ~value ~on_input =
  let open Vdom in
  Node.div
    ~attrs:[ Attr.class_ "login-field" ]
    [ Node.label ~attrs:[ Attr.for_ id ] [ Node.text label ]
    ; Node.input
        ~attrs:
          [ Attr.type_ type_
          ; Attr.id id
          ; Attr.name id
          ; Attr.create "autocomplete" autocomplete
          ; Attr.value_prop value
          ; Attr.on_input (fun _ value -> on_input value)
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
  let view =
    let open Vdom in
    Client_utils.status_node
      ~heading:"Log in"
      [ Node.create
          "form"
          ~attrs:
            [ Attr.class_ "login-form"
            ; Attr.on_submit (fun _ -> Effect.Many [ Effect.Prevent_default; submit ])
            ]
          [ field
              ~id:"username"
              ~label:"Username"
              ~type_:"text"
              ~autocomplete:"username"
              ~value:model.username
              ~on_input:(fun username -> inject (Set_username username))
          ; field
              ~id:"password"
              ~label:"Password"
              ~type_:"password"
              ~autocomplete:"current-password"
              ~value:model.password
              ~on_input:(fun password -> inject (Set_password password))
          ; (match model.error with
             | None -> Node.none
             | Some error ->
               Node.div
                 ~attrs:[ Attr.class_ "login-error"; Attr.create "role" "alert" ]
                 [ Node.text error ])
          ; Node.button
              ~attrs:[ Attr.type_ "submit"; Attr.class_ "login-submit" ]
              [ Node.text "Log in" ]
          ]
      ]
  in
  ({ title = Some "Log in"; view } : Page.t)
;;
