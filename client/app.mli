open! Core
open! Import

(** The whole page: the navbar and footer, the search modal, and
    whichever page the current route selects. *)

val component : Vdom.Node.t Computation.t
