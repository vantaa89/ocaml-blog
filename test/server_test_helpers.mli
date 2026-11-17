open! Core
open! Async
open! Import

(** Runs a [Web_server] over an in-memory database, and talks to it as a client would,
    presenting a [Credential.t]. *)

val user : id:int -> username:string -> Database_schema.User.t

(** The password of every user [user] makes *)
val password : string

val post
  :  ?title:string (* default: [slug], capitalized *)
  -> ?content_en:
       string (* default: ["Content of <title>"]. The Korean content is [None]. *)
  -> ?created_at:Time_ns.t (* default: 2026-08-01 00:00:00Z, when [with_server] starts *)
  -> ?special_post:bool (* default: [false] *)
  -> ?hidden:bool (* default: [false] *)
  -> id:int
  -> slug:string
  -> author:Database_schema.User.t
  -> unit
  -> Database_schema.Post.t

type t

(** What a client presents to be recognized: an API token as [Authorization: Bearer], or
    a session token as the cookie *)
module Credential : sig
  type t =
    [ `Api_token of string
    | `Session_token of string
    ]
end

val with_server : Database.t -> f:(t -> 'a Deferred.t) -> 'a Deferred.t
val advance_clock : t -> Time_ns.Span.t -> unit Deferred.t

(** Posts a login form holding only the fields given, and returns the response along with
    the session it starts, if any. *)
val login
  :  ?origin:string (* default: the server's own, [http://127.0.0.1:<port>] *)
  -> ?username:string
  -> ?password:string
  -> t
  -> (Cohttp.Response.t * Credential.t option) Deferred.t

val logout : ?credential:Credential.t -> t -> Cohttp.Response.t Deferred.t

val get
  :  ?credential:Credential.t
  -> t
  -> path:string
  -> (Cohttp.Response.t * string) Deferred.t

(** Puts [body] at [path] the way an API client would, without [Origin] *)
val put
  :  ?credential:Credential.t
  -> t
  -> path:string
  -> body:string
  -> (Cohttp.Response.t * string) Deferred.t

val with_rpc_connection
  :  ?credential:Credential.t
  -> t
  -> f:(Rpc.Connection.t -> 'a Deferred.t)
  -> 'a Deferred.t
