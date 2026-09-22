open! Core

type t =
  { port : int
  ; static_dir : string
  ; media_dir : string
  ; zone : Timezone.t
  ; max_login_attempts : int
  ; login_attempt_window : Time_ns.Span.t
  }

let default =
  { port = 8080
  ; static_dir = "static"
  ; media_dir = "media"
  ; zone = Timezone.find_exn "Asia/Seoul"
  ; max_login_attempts = 5
  ; login_attempt_window = Time_ns.Span.of_min 15.
  }
;;

let param =
  let%map_open.Command port =
    flag
      "-port"
      (optional_with_default default.port int)
      ~doc:[%string "PORT port to listen on (default %{default.port#Int})"]
  and static_dir =
    flag
      "-static-dir"
      (optional_with_default default.static_dir string)
      ~doc:
        [%string
          "DIR directory holding the built frontend assets (default \
           %{default.static_dir})"]
  and media_dir =
    flag
      "-media-dir"
      (optional_with_default default.media_dir string)
      ~doc:
        [%string "DIR directory holding uploaded images (default %{default.media_dir})"]
  and zone =
    flag
      "-zone"
      (optional_with_default default.zone (Command.Arg_type.create Timezone.find_exn))
      ~doc:
        [%string
          "ZONE time zone naming the date directories of uploaded images (default \
           %{Timezone.to_string default.zone})"]
  and max_login_attempts =
    flag
      "-max-login-attempts"
      (optional_with_default default.max_login_attempts int)
      ~doc:
        [%string
          "N failed logins a username may make within -login-attempt-window (default \
           %{default.max_login_attempts#Int})"]
  and login_attempt_window =
    flag
      "-login-attempt-window"
      (optional_with_default default.login_attempt_window Time_ns.Span.arg_type)
      ~doc:
        [%string
          "SPAN how long a failed login is counted against its username (default \
           %{default.login_attempt_window#Time_ns.Span})"]
  in
  { port; static_dir; media_dir; zone; max_login_attempts; login_attempt_window }
;;
