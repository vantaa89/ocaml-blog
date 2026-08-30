open! Core
open! Import

(** One computation per Django template, each fetching its own data and rendering the
    template's DOM. *)

val home : Vdom.Node.t Computation.t

val posts
  :  tag_slug:string option Value.t
  -> page:int Value.t
  -> Vdom.Node.t Computation.t

val search : query:string Value.t -> page:int Value.t -> Vdom.Node.t Computation.t
val post_detail : slug:string Value.t -> Vdom.Node.t Computation.t
val about : Vdom.Node.t Computation.t
