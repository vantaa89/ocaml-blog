open! Core
open! Async
open! Import

(** Stores at [<media_dir>/<YYYY-MM-DD>/<filename>], as [Database_schema.Image.url]
    expects. *)
val save
  :  media_dir:string
  -> date:Date.t
  -> content:string
  -> [ `Saved_as of string | `Too_large | `Unsupported_format ] Deferred.Or_error.t
