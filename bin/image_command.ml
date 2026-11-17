open! Core
open! Async
open! Blog_server

let add_image_command =
  Command.async_or_error
    ~summary:"Upload an image, as the editor does"
    (let%map_open.Command path =
       flag "-file" (required Filename_unix.arg_type) ~doc:"FILE the image to upload"
     and media_dir =
       flag
         "-media-dir"
         (optional_with_default Config.default.media_dir string)
         ~doc:
           [%string
             "DIR directory holding uploaded images (default %{Config.default.media_dir})"]
     in
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
                 ~id:(image.id : int)
                 ~url:(Database_schema.Image.url image : string)]))
;;

let list_images_command =
  Command.async_or_error
    ~summary:"List every image"
    (let%map_open.Command () = return () in
     fun () ->
       Database.with_connection ~f:(fun db ->
         let%map.Deferred.Or_error images = Database.Image.list db in
         List.iter images ~f:(fun image ->
           print_s
             [%message
               "" ~id:(image.id : int) ~url:(Database_schema.Image.url image : string)])))
;;

let command =
  Command.group
    ~summary:"Manage uploaded images"
    [ "add", add_image_command; "list", list_images_command ]
;;
