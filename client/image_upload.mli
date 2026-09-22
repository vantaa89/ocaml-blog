open! Core
open! Import
open Js_of_ocaml

(** Getting images into the post editor's markdown: picked with a button, pasted, or
    dropped onto the textarea. *)

module Failure : sig
  type t =
    | Too_large
    | Unsupported_format
    | Unreadable of { name : string }
    | Not_logged_in
    | Unreachable
end

(** Uploads [file], returning its URL. *)
val upload
  :  dispatch:
       (Rpcs.Upload_image.Query.t -> Rpcs.Upload_image.Response.t Or_error.t Effect.t)
  -> File.file Js.t
  -> (string, Failure.t) Result.t Effect.t

(** Replaces the selection of the textarea [textarea_id] with markdown showing [urls],
    moves the caret past it, and returns the textarea's new value. *)
val insert : textarea_id:string -> urls:string list -> string option Effect.t

(** Handlers for the textarea that pass pasted and dropped image files to [upload]. *)
val drop_target : upload:(File.file Js.t list -> unit Effect.t) -> Vdom.Attr.t

val button
  :  uploading:bool
  -> upload:(File.file Js.t list -> unit Effect.t)
  -> Vdom.Node.t
