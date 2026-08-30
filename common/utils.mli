open! Core

(** Returns [None] if [list] is empty, [Some x] if [list] contains exactly one
    element [x], and raises with [error_message list] otherwise. *)
val expect_at_most_one : 'a list -> error_message:('a list -> Sexp.t) -> 'a option
