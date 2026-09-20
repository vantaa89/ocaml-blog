open! Core
open! Async
open! Blog_server

let read_new_password () =
  let read_password ~prompt =
    print_string prompt;
    let%bind () = Writer.flushed (force Writer.stdout) in
    let terminal =
      match Core_unix.isatty Core_unix.stdin with
      | false -> None
      | true -> Some (Core_unix.Terminal_io.tcgetattr Core_unix.stdin)
    in
    let set_echo echo =
      Option.iter terminal ~f:(fun terminal ->
        Core_unix.Terminal_io.tcsetattr
          { terminal with c_echo = echo }
          Core_unix.stdin
          ~mode:TCSANOW)
    in
    set_echo false;
    let%map line = Reader.read_line (force Reader.stdin) in
    set_echo true;
    print_endline "";
    match line with
    | `Ok password -> password
    | `Eof -> failwith "Expected a password"
  in
  let%bind password = read_password ~prompt:"Password: " in
  let%map confirmation = read_password ~prompt:"Password (again): " in
  match String.equal password confirmation with
  | false -> Or_error.error_s [%message "The passwords do not match"]
  | true -> Ok password
;;

let username_flag ~doc =
  Command.Param.flag "-username" (Command.Param.required Command.Param.string) ~doc
;;

let create_user_command =
  Command.async_or_error
    ~summary:"Create a user that can log in"
    (let%map_open.Command username = username_flag ~doc:"NAME login name"
     and email = flag "-email" (required string) ~doc:"EMAIL contact address" in
     fun () ->
       let%bind.Deferred.Or_error password = read_new_password () in
       Database.with_connection ~f:(fun db ->
         let%map.Deferred.Or_error (_ : Database_schema.User.t) =
           Database.User.create
             db
             ~username
             ~email
             ~password_hash:(Password.hash_exn password)
             ~date_joined:(Date.today ~zone:(force Timezone.local))
         in
         print_s [%message "Created user" (username : string)]))
;;

let list_users_command =
  Command.async_or_error
    ~summary:"List every user"
    (let%map_open.Command () = return () in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let%map.Deferred.Or_error users = Database.User.list db in
         List.iter users ~f:(fun (user : Database_schema.User.t) ->
           print_s
             [%message
               ""
                 ~username:(user.username : string)
                 ~email:(user.email : string)
                 ~date_joined:(user.date_joined : Date.t)
                 ~last_login:(user.last_login : Time_ns.Alternate_sexp.t option)])))
;;

let set_password_command =
  Command.async_or_error
    ~summary:"Change a user's password"
    (let%map_open.Command username = username_flag ~doc:"NAME the user to change" in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         match%bind Database.User.find_by_username db ~username with
         | None -> Deferred.Or_error.error_s [%message "No such user" (username : string)]
         | Some (_ : Database_schema.User.t) ->
           let%bind password = read_new_password () in
           Database.User.set_password_hash
             db
             ~username
             ~password_hash:(Password.hash_exn password)))
;;

let delete_user_command =
  Command.async_or_error
    ~summary:"Delete a user, and with them their sessions"
    (let%map_open.Command username = username_flag ~doc:"NAME the user to delete" in
     fun () ->
       Database.with_connection ~f:(fun db ->
         match%bind.Deferred.Or_error Database.User.find_by_username db ~username with
         | None -> Deferred.Or_error.error_s [%message "No such user" (username : string)]
         | Some (_ : Database_schema.User.t) -> Database.User.delete db ~username))
;;

let command =
  Command.group
    ~summary:"Manage the users who can log in"
    [ "create", create_user_command
    ; "list", list_users_command
    ; "set-password", set_password_command
    ; "delete", delete_user_command
    ]
;;
