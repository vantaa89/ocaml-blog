open! Core
open! Async
open! Import

type t

val with_connection : f:(t -> 'a Deferred.t) -> 'a Deferred.t
val create_tables : t -> unit Or_error.t Deferred.t

module Post : sig
  val create
    :  t
    -> title:string
    -> slug:string
    -> content_en:string option
    -> content_ko:string option
    -> author_id:int
    -> special_post:bool
    -> Database_schema.Post.t Or_error.t Deferred.t

  val find_by_slug : t -> slug:string -> Database_schema.Post.t option Or_error.t Deferred.t
  val find_by_id : t -> id:int -> Database_schema.Post.t option Or_error.t Deferred.t
  val find_by_title : t -> title:string -> Database_schema.Post.t option Or_error.t Deferred.t

  val list
    :  t
    -> ?include_hidden:bool
    -> ?limit:int
    -> ?offset:int
    -> unit
    -> Database_schema.Post.t list Or_error.t Deferred.t

  val list_by_tag_slug
    :  t
    -> slug:string
    -> ?include_hidden:bool
    -> ?limit:int
    -> ?offset:int
    -> unit
    -> Database_schema.Post.t list Or_error.t Deferred.t

  val search
    :  t
    -> query:string
    -> ?limit:int
    -> unit
    -> Database_schema.Post.t list Or_error.t Deferred.t

  val set_hidden : t -> id:int -> hidden:bool -> unit Or_error.t Deferred.t
end

module User : sig
  val create
    :  t
    -> username:string
    -> email:string
    -> password_hash:string
    -> date_joined:Date.t
    -> Database_schema.User.t Or_error.t Deferred.t

  val find_by_username
    :  t
    -> username:string
    -> Database_schema.User.t option Or_error.t Deferred.t
end

module Image : sig
  val create
    :  t
    -> filename:string
    -> date:Date.t
    -> Database_schema.Image.t Or_error.t Deferred.t
end

module Tag : sig
  val find_by_slug : t -> slug:string -> Database_schema.Tag.t option Or_error.t Deferred.t

  val find_or_create
    :  t
    -> name:string
    -> slug:string
    -> Database_schema.Tag.t Or_error.t Deferred.t

  val list_with_post_counts
    :  t
    -> (Database_schema.Tag.t * int) list Or_error.t Deferred.t
end

module Post_tag : sig
  val tags_for_post : t -> post_id:int -> Database_schema.Tag.t list Or_error.t Deferred.t
  val set_tags : t -> post_id:int -> tag_ids:int list -> unit Or_error.t Deferred.t
end

module Publication : sig
  val create
    :  t
    -> title:string
    -> image_id:int
    -> authors:string
    -> journal:string
    -> link:string option
    -> Database_schema.Publication.t Or_error.t Deferred.t

  val list
    :  t
    -> ?include_hidden:bool
    -> unit
    -> Database_schema.Publication.t list Or_error.t Deferred.t
end

module News : sig
  val create
    :  t
    -> content:string
    -> date:Date.t
    -> Database_schema.News.t Or_error.t Deferred.t

  val list : t -> ?limit:int -> unit -> Database_schema.News.t list Or_error.t Deferred.t
end

module For_testing : sig
  val create : unit -> t
end
