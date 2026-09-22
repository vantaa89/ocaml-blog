open! Core
open! Import

(** The set of pages the client can display, and the mapping between them and the
    browser's URL. *)

type t =
  | Home
  | Posts of
      { tag : string option
      ; page : int
      }
  | Post of { slug : string }
  | New_post
  | Edit_post of { slug : string }
  | About
  | Search of
      { query : string
      ; page : int
      }
  | Login

include Bonsai_web_ui_url_var.S with type t := t

val to_string : t -> string
val with_page : t -> int -> t
val title : t -> string
