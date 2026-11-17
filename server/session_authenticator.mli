open! Core
open! Async
open! Import

(** Authentication by session: logging in with a password starts a session, whose token
    the client keeps in an [HttpOnly] cookie that page scripts cannot read, which is why
    this is served over HTTP rather than RPC. Login attempts are rate-limited. *)

type t

val create : config:Config.t -> time_source:Time_source.t -> t

val login
  :  t
  -> db:Database.t
  -> now:Time_ns.t
  -> body:Cohttp_async.Body.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response Deferred.t

val logout : db:Database.t -> Cohttp.Request.t -> Cohttp_async.Server.response Deferred.t

(** The session token a request carries, if any. *)
val token : Cohttp.Request.t -> Token.t option

val current_user_id
  :  db:Database.t
  -> now:Time_ns.t
  -> session_token:Token.t option
  -> User_id.t option Deferred.Or_error.t
