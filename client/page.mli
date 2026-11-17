open! Core
open! Import

(** What a page shows and its own title (e.g., post title for post pages). *)
type t =
  { view : Vdom.Node.t
  ; title : string option
  }
