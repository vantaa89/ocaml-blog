open! Core
open! Import

(** The Ctrl+K search modal from Django's [base.html]. *)

val component : (Vdom.Node.t * Vdom.Node.t) Computation.t
