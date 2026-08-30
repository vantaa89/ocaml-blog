open! Core

type t =
  { port : int
  ; static_dir : string
  ; media_dir : string
  }

let param =
  let%map_open.Command port =
    flag "-port" (optional_with_default 8080 int) ~doc:"PORT port to listen on"
  and static_dir =
    flag
      "-static-dir"
      (optional_with_default "static" string)
      ~doc:"DIR directory holding the built frontend assets"
  and media_dir =
    flag
      "-media-dir"
      (optional_with_default "media" string)
      ~doc:"DIR directory holding uploaded images"
  in
  { port; static_dir; media_dir }
;;
