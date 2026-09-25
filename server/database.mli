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
    -> now:Time_ns.t
    -> Database_schema.Post.t Deferred.Or_error.t

  val find_by_slug : t -> slug:string -> Database_schema.Post.t option Deferred.Or_error.t
  val find_by_id : t -> id:int -> Database_schema.Post.t option Deferred.Or_error.t

  (** Returns list of posts. Special posts are excluded, and so are the hidden posts of
      everyone but [viewer], the user reading them. *)
  val list
    :  t
    -> viewer:int option
    -> ?limit:int
    -> ?offset:int (* default: [0] *)
    -> unit
    -> Database_schema.Post.t list Deferred.Or_error.t

  (** Like [list], restricted to the posts carrying [tag], regardless of case. *)
  val list_by_tag
    :  t
    -> tag:string
    -> viewer:int option
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

  (** Updates the post with [id]. Fails if the slug is already used by other post. *)
  val update
    :  t
    -> id:int
    -> title:string
    -> slug:string
    -> content_en:string option
    -> content_ko:string option
    -> special_post:bool
    -> unit Deferred.Or_error.t
end

module User : sig
  val create
    :  t
    -> username:string
    -> email:string
    -> password_hash:string
    -> date_joined:Date.t
    -> Database_schema.User.t Deferred.Or_error.t

  val find_by_id : t -> id:int -> Database_schema.User.t option Deferred.Or_error.t
  val list : t -> Database_schema.User.t list Deferred.Or_error.t

  val set_password_hash
    :  t
    -> username:string
    -> password_hash:string
    -> unit Deferred.Or_error.t

  val set_last_login
    :  t
    -> username:string
    -> last_login:Time_ns.t
    -> unit Deferred.Or_error.t

  (** The user's sessions go with them, since [session.user_id] cascades. Posts do not, so
      deleting an author who still has posts fails. *)
  val delete : t -> username:string -> unit Deferred.Or_error.t

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
  (** Tag names are compared regardless of case. *)
  val find_by_name : t -> name:string -> Database_schema.Tag.t option Deferred.Or_error.t

  (** An existing tag keeps its own casing of the name. *)
  val find_or_create : t -> name:string -> Database_schema.Tag.t Deferred.Or_error.t

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

  (** Fails if there is no news with [id]. *)
  val delete : t -> id:int -> unit Deferred.Or_error.t
end

module Session : sig
  val create
    :  t
    -> token_hash:string
    -> user_id:int
    -> expires_at:Time_ns.t
    -> Database_schema.Session.t Deferred.Or_error.t

  (** Returns the session even when it has already expired, so callers must check
      [expires_at] themselves. *)
  val find_by_token_hash
    :  t
    -> token_hash:string
    -> Database_schema.Session.t option Deferred.Or_error.t

  val delete : t -> token_hash:string -> unit Deferred.Or_error.t
  val delete_expired : t -> now:Time_ns.t -> unit Deferred.Or_error.t
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
    -> ?sessions:Database_schema.Session.t list
    -> unit
    -> t

  val with_test_connection : f:(t -> 'a Deferred.t) -> 'a Deferred.t
end
