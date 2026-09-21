open! Core

type t =
  { port : int
  ; static_dir : string
  ; media_dir : string
  ; max_login_attempts : int
    (*** Failed logins are counted per username, so brute-forcing the one account this
      blog has runs out of attempts. *)
  ; login_attempt_window : Time_ns.Span.t
  }

val param : t Command.Param.t
