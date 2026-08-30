open! Core

type t =
  { port : int
  ; static_dir : string
  ; media_dir : string
  }

val param : t Command.Param.t
