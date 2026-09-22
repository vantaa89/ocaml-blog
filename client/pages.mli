open! Core
open! Import

(** One computation per page, each fetching its own data. *)

val home : Vdom.Node.t Computation.t
val posts : tag:string option Value.t -> page:int Value.t -> Vdom.Node.t Computation.t
val search : query:string Value.t -> page:int Value.t -> Vdom.Node.t Computation.t
val post_detail : slug:string Value.t -> Vdom.Node.t Computation.t
val about : Vdom.Node.t Computation.t
