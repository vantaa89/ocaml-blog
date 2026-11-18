open! Core
open! Async
open! Blog_server

let media_dir_flag =
  Command.Param.flag
    "-media-dir"
    (Command.Param.optional_with_default Config.default.media_dir Command.Param.string)
    ~doc:
      [%string
        "DIR directory holding uploaded images (default %{Config.default.media_dir})"]
;;

let add_image_command =
  Command.async_or_error
    ~summary:"Upload an image, as the editor does"
    (let%map_open.Command path =
       flag "-file" (required Filename_unix.arg_type) ~doc:"FILE the image to upload"
     and media_dir = media_dir_flag in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind content =
         Deferred.Or_error.try_with (fun () -> Reader.file_contents path)
       in
       let date = Date.today ~zone:Config.default.zone in
       match%bind Image_store.save ~media_dir ~date ~content with
       | `Too_large -> Deferred.Or_error.error_s [%message "Image too large" path]
       | `Unsupported_format ->
         Deferred.Or_error.error_s [%message "Unsupported image format" path]
       | `Saved_as filename ->
         Database.with_connection ~f:(fun db ->
           let%map image = Database.Image.create db ~filename ~date in
           print_s
             [%message
               "Created image"
                 ~id:(image.id : Ids.Image_id.t)
                 ~url:(Database_schema.Image.url image : string)]))
;;

let list_images_command =
  Command.async_or_error
    ~summary:"List every image with how many posts and publications use it"
    (let%map_open.Command () = return () in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         let%bind images = Database.Image.list db in
         Deferred.Or_error.List.iter images ~how:`Sequential ~f:(fun image ->
           let%map references = Database.Image.reference_count db image in
           print_s
             [%message
               ""
                 ~id:(image.id : Ids.Image_id.t)
                 ~url:(Database_schema.Image.url image : string)
                 (references : int)])))
;;

let delete_image_command =
  Command.async_or_error
    ~summary:"Delete an image that no post or publication uses, with its file"
    (let%map_open.Command id =
       flag
         "-id"
         (required (Command.Arg_type.map int ~f:Ids.Image_id.of_int_exn))
         ~doc:"ID the image, as [list] shows"
     and media_dir = media_dir_flag in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let open Deferred.Or_error.Let_syntax in
         let%bind images = Database.Image.list db in
         match List.find images ~f:(fun image -> Ids.Image_id.equal image.id id) with
         | None ->
           Deferred.Or_error.error_s [%message "No such image" (id : Ids.Image_id.t)]
         | Some image ->
           let%bind () = Database.Image.delete db ~id in
           (* Uploading the same file twice on one day gives two images sharing a file. *)
           (match
              List.exists images ~f:(fun other ->
                (not (Ids.Image_id.equal other.id id))
                && String.equal
                     (Database_schema.Image.url other)
                     (Database_schema.Image.url image))
            with
            | true -> return ()
            | false ->
              Image_store.delete ~media_dir ~date:image.date ~filename:image.filename)))
;;

let command =
  Command.group
    ~summary:"Manage uploaded images"
    [ "add", add_image_command
    ; "list", list_images_command
    ; "delete", delete_image_command
    ]
;;
