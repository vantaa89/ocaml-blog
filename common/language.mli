open! Core

type t =
  | English
  | Korean

include Hashable.S_plain with type t := t

val to_code : t -> string
val of_code_exn : string -> t
