open! Core
open! Import

(** The site's mark: the 4×4 Hadamard matrix in sequency (Walsh) order. +1 is drawn as a
    rounded square and −1 is left empty. Every 3 seconds it turns over and comes straight
    back. *)
val mark : Vdom.Node.t

(** Turns over the first matrix inside the element (−H) while the pointer is on the
    element, its cells growing or shrinking in a wave spreading from the pointer. The
    pointer's position relative to that matrix is kept in the CSS variables [--pointer-x]
    and [--pointer-y], and the turned state in [data-turned]. *)
val turn_on_hover : Vdom.Attr.t
