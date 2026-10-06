open! Core

(** The set of pages the client can display, and the mapping between them and URLs. The
    server uses it too, to answer 404 for a path that is no page. *)

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
  | Not_found of { path : string }
[@@deriving sexp, equal]

(** [path] has its segments percent-encoded, and empty segments are ignored. *)
val of_url : path:string -> query:string list String.Map.t -> t

(** The path, without a leading slash, and the query of [t]. *)
val to_url : t -> string * string list String.Map.t

val to_string : t -> string
val with_page : t -> int -> t
val title : t -> string
