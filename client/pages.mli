open! Core
open! Import

(** One computation per page, each fetching its own data. *)

val home : Page.t Computation.t
val posts : tag:string option Value.t -> page:int Value.t -> Page.t Computation.t
val search : query:string Value.t -> page:int Value.t -> Page.t Computation.t
val post_detail : slug:string Value.t -> Page.t Computation.t
val about : Page.t Computation.t
val not_found : Page.t Computation.t
