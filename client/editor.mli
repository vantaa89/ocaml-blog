open! Core
open! Import

(** The author's pages for writing a new post and for revising an existing one, covering
    the whole viewport: every input on the left half and the rendering alone on the right.
    [Ctrl]/[Cmd]+[S] saves. *)

val new_post : Vdom.Node.t Computation.t
val edit_post : slug:string Value.t -> Vdom.Node.t Computation.t
