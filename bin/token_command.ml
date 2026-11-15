open! Core
open! Async
open! Blog_server

let label_flag ~doc =
  Command.Param.flag "-label" (Command.Param.required Command.Param.string) ~doc
;;

let create_token_command =
  Command.async_or_error
    ~summary:"Issue an API token, printed only this once"
    (let%map_open.Command username =
       flag "-username" (required string) ~doc:"NAME the user the token acts as"
     and label = label_flag ~doc:"LABEL what the token is for" in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         match%bind Database.User.find_by_username db ~username with
         | None -> Deferred.Or_error.error_s [%message "No such user" (username : string)]
         | Some { id; _ } ->
           let%map token =
             Api.create_token ~db ~label ~user_id:id ~now:(Time_ns.now ())
           in
           (* The notice goes to stderr, so that stdout can be redirected to a file
              holding the token alone. *)
           prerr_endline
             [%string
               "Created the API token %{label} for %{username}. It is shown only this \
                once and never expires; revoke it with [token revoke -label %{label}]."];
           print_endline (Token.to_string token)))
;;

let list_tokens_command =
  Command.async_or_error
    ~summary:"List the API tokens by label; the tokens themselves are not kept"
    (let%map_open.Command () = return () in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         let%bind tokens = Database.Api_token.list db in
         Deferred.Or_error.List.iter
           (Map.data tokens)
           ~how:`Sequential
           ~f:(fun { label; user_id; created_at; _ } ->
             let%map user = Database.User.find_by_id db ~id:user_id in
             let username =
               Option.value_map
                 user
                 ~f:(fun user -> user.username)
                 ~default:"<unknown user>"
             in
             print_s
               [%message
                 ""
                   (label : string)
                   (username : string)
                   (created_at : Time_ns.Alternate_sexp.t)])))
;;

let revoke_token_command =
  Command.async_or_error
    ~summary:"Revoke an API token"
    (let%map_open.Command label = label_flag ~doc:"LABEL the token to revoke" in
     fun () -> Database.with_connection ~f:(fun db -> Database.Api_token.delete db ~label))
;;

let command =
  Command.group
    ~summary:"Manage the API tokens"
    [ "create", create_token_command
    ; "list", list_tokens_command
    ; "revoke", revoke_token_command
    ]
;;
