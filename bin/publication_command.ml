open! Core
open! Async
open! Blog_server

let add_publication_command =
  Command.async_or_error
    ~summary:"Add a publication, shown on the front page"
    (let%map_open.Command title = flag "-title" (required string) ~doc:"TEXT the title"
     and authors =
       flag "-authors" (required string) ~doc:"TEXT the authors, as they are shown"
     and journal = flag "-journal" (required string) ~doc:"TEXT the journal or conference"
     and link = flag "-link" (optional string) ~doc:"URL where the paper can be read"
     and image_id =
       flag
         "-image-id"
         (required (Command.Arg_type.map int ~f:Ids.Image_id.of_int_exn))
         ~doc:"ID the thumbnail, as [image list] shows"
     in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         match%bind Database.Image.find_by_id db ~id:image_id with
         | None ->
           Deferred.Or_error.error_s
             [%message "No such image" (image_id : Ids.Image_id.t)]
         | Some (_ : Database_schema.Image.t) ->
           let%map publication =
             Database.Publication.create db ~title ~image_id ~authors ~journal ~link
           in
           print_s
             [%message "Created publication" ~id:(publication.id : Ids.Publication_id.t)]))
;;

let list_publications_command =
  Command.async_or_error
    ~summary:"List every publication, hidden ones included"
    (let%map_open.Command () = return () in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         let%bind publications = Database.Publication.list db ~include_hidden:true () in
         Deferred.Or_error.List.iter
           publications
           ~how:`Sequential
           ~f:(fun (publication : Database_schema.Publication.t) ->
             let%map image = Database.Image.find_by_id db ~id:publication.image_id in
             print_s
               [%message
                 ""
                   ~id:(publication.id : Ids.Publication_id.t)
                   ~title:(publication.title : string)
                   ~authors:(publication.authors : string)
                   ~journal:(publication.journal : string)
                   ~link:(publication.link : (string option[@sexp.option]))
                   ~image:(Option.map image ~f:Database_schema.Image.url : string option)
                   ~hidden:(publication.hidden : bool)])))
;;

let id_flag =
  Command.Param.flag
    "-id"
    (Command.Param.required
       (Command.Arg_type.map Command.Param.int ~f:Ids.Publication_id.of_int_exn))
    ~doc:"ID the publication, as [list] shows"
;;

let set_hidden_command ~summary ~hidden =
  Command.async_or_error
    ~summary
    (let%map_open.Command id = id_flag in
     fun () ->
       Database.with_connection ~f:(fun db ->
         Database.Publication.set_hidden db ~id ~hidden))
;;

let delete_publication_command =
  Command.async_or_error
    ~summary:"Delete a publication"
    (let%map_open.Command id = id_flag in
     fun () -> Database.with_connection ~f:(fun db -> Database.Publication.delete db ~id))
;;

let command =
  Command.group
    ~summary:"Manage the publications shown on the front page"
    [ "add", add_publication_command
    ; "list", list_publications_command
    ; "hide", set_hidden_command ~summary:"Hide a publication" ~hidden:true
    ; ( "unhide"
      , set_hidden_command ~summary:"Show a hidden publication again" ~hidden:false )
    ; "delete", delete_publication_command
    ]
;;
