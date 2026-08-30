open! Core
open! Async
open! Import

type t

val with_connection : f:(t -> 'a Deferred.t) -> 'a Deferred.t
val create_tables : t -> unit Deferred.Or_error.t

module Post : sig
  val create
    :  t
    -> title:string
    -> slug:string
    -> content_en:string option
    -> content_ko:string option
    -> author_id:int
    -> special_post:bool
    -> Database_schema.Post.t Deferred.Or_error.t

  val find_by_slug : t -> slug:string -> Database_schema.Post.t option Deferred.Or_error.t
  val find_by_id : t -> id:int -> Database_schema.Post.t option Deferred.Or_error.t

  (** Returns list of posts. Special posts are excluded *)
  val list
    :  t
    -> ?include_hidden:bool (* default: [false] *)
    -> ?limit:int
    -> ?offset:int (* default: [0] *)
    -> unit
    -> Database_schema.Post.t list Deferred.Or_error.t

  (** Returns list of posts. Special posts are excluded *)
  val list_by_tag_slug
    :  t
    -> slug:string
    -> ?include_hidden:bool
    -> ?limit:int
    -> ?offset:int (* default: [0] *)
    -> unit
    -> Database_schema.Post.t list Deferred.Or_error.t

  val search
    :  t
    -> query:string
    -> ?limit:int
    -> unit
    -> Database_schema.Post.t list Deferred.Or_error.t

  val set_hidden : t -> id:int -> hidden:bool -> unit Deferred.Or_error.t
end

module User : sig
  val create
    :  t
    -> username:string
    -> email:string
    -> password_hash:string
    -> date_joined:Date.t
    -> Database_schema.User.t Deferred.Or_error.t

  val find_by_username
    :  t
    -> username:string
    -> Database_schema.User.t option Deferred.Or_error.t
end

module Image : sig
  val create
    :  t
    -> filename:string
    -> date:Date.t
    -> Database_schema.Image.t Deferred.Or_error.t

  val find_by_id : t -> id:int -> Database_schema.Image.t option Deferred.Or_error.t
end

module Tag : sig
  val find_by_slug : t -> slug:string -> Database_schema.Tag.t option Deferred.Or_error.t

  val find_or_create
    :  t
    -> name:string
    -> slug:string
    -> Database_schema.Tag.t Deferred.Or_error.t

  val list_with_post_counts : t -> (Database_schema.Tag.t * int) list Deferred.Or_error.t
end

module Post_tag : sig
  val tags_for_post : t -> post_id:int -> Database_schema.Tag.t list Deferred.Or_error.t
  val set_tags : t -> post_id:int -> tag_ids:int list -> unit Deferred.Or_error.t
end

module Publication : sig
  val create
    :  t
    -> title:string
    -> image_id:int
    -> authors:string
    -> journal:string
    -> link:string option
    -> Database_schema.Publication.t Deferred.Or_error.t

  val list
    :  t
    -> ?include_hidden:bool (* default: [false] *)
    -> unit
    -> Database_schema.Publication.t list Deferred.Or_error.t
end

module News : sig
  val create
    :  t
    -> content:string
    -> date:Date.t
    -> Database_schema.News.t Deferred.Or_error.t

  val list : t -> Database_schema.News.t list Deferred.Or_error.t
end

module For_testing : sig
  val create_in_memory
    :  ?posts:Database_schema.Post.t list
    -> ?users:Database_schema.User.t list
    -> ?images:Database_schema.Image.t list
    -> ?tags:Database_schema.Tag.t list
    -> ?post_tags:Database_schema.Post_tag.t list
    -> ?publication:Database_schema.Publication.t list
    -> ?news:Database_schema.News.t list
    -> unit
    -> t

  val with_test_connection : f:(t -> 'a Deferred.t) -> 'a Deferred.t
end
