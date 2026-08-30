open! Core
open! Async
open! Blog_server

let serve_command =
  Command.async
    ~summary:"Serve the blog backend"
    (let%map_open.Command config = Config.param in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let%bind server = Web_server.serve db config in
         Log.Global.info_s
           [%message "Listening" ~port:(Cohttp_async.Server.listening_on server : int)];
         Cohttp_async.Server.close_finished server))
;;

let create_tables_command =
  Command.async_or_error
    ~summary:"Create the blog tables if they do not exist"
    (let%map_open.Command () = return () in
     fun () -> Database.with_connection ~f:Database.create_tables)
;;

let command =
  Command.group
    ~summary:"Blog server"
    [ "serve", serve_command; "create-tables", create_tables_command ]
;;

let () = Command_unix.run command
