open! Core
open! Async
open! Import

(** Session-cookie based authentication. This is served over HTTP rather than RPC, so that the token stays in an [HttpOnly] cookie that page scripts cannot read. *)

val handle_login
  :  Database.t
  -> Config.t
  -> body:Cohttp_async.Body.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response Deferred.t

val handle_logout
  :  Database.t
  -> Config.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response Deferred.t

(** The session token a request carries, if any. *)
val session_token : Cohttp.Request.t -> string option

val current_user
  :  Database.t
  -> session_token:string option
  -> Database_schema.User.t option Deferred.Or_error.t
