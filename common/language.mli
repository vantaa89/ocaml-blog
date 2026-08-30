open! Core

type t =
  | English
  | Korean
[@@deriving bin_io, sexp_of]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t

val to_code : t -> string
val of_code_exn : string -> t
