open! Core
open! Async
open! Import

let save ~media_dir ~date ~content =
  match String.length content > Rpcs.Upload_image.max_size with
  | true -> Deferred.Or_error.return `Too_large
  | false ->
    let has ?(pos = 0) signature =
      String.length content >= pos + String.length signature
      && String.is_substring_at content ~pos ~substring:signature
    in
    (* Check magic number to identify the extension, rather than relying on the file name
       suffix. *)
    let extension =
      List.find_map
        [ has "\x89PNG\r\n\x1a\n", "png"
        ; has "\xff\xd8\xff", "jpg"
        ; has "GIF87a" || has "GIF89a", "gif"
        ; has "RIFF" && has ~pos:8 "WEBP", "webp"
        ]
        ~f:(fun (matches, extension) -> Option.some_if matches extension)
    in
    (match extension with
     | None -> Deferred.Or_error.return `Unsupported_format
     | Some extension ->
       let hash =
         Digestif.SHA256.digest_string content
         |> Digestif.SHA256.to_hex
         |> fun hex -> String.prefix hex 16
       in
       let filename = [%string "%{hash}.%{extension}"] in
       Deferred.Or_error.try_with (fun () ->
         let directory = media_dir ^/ Date.to_string date in
         let%bind () = Unix.mkdir ~p:() directory in
         let%map () = Writer.save (directory ^/ filename) ~contents:content in
         `Saved_as filename))
;;
