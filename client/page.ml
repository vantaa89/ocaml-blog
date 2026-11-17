open! Core
open! Import

type t =
  { view : Vdom.Node.t
  ; title : string option
  }
