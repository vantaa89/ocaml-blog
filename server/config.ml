open! Core

type t =
  { port : int
  ; static_dir : string
  ; media_dir : string
  ; max_login_attempts : int
  ; login_attempt_window : Time_ns.Span.t
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
  and max_login_attempts =
    flag
      "-max-login-attempts"
      (optional_with_default 5 int)
      ~doc:"N failed logins a username may make within -login-attempt-window"
  and login_attempt_window =
    flag
      "-login-attempt-window"
      (optional_with_default (Time_ns.Span.of_min 15.) Time_ns.Span.arg_type)
      ~doc:"SPAN how long a failed login is counted against its username"
  in
  { port; static_dir; media_dir; max_login_attempts; login_attempt_window }
;;
