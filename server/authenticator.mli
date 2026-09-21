open! Core
open! Async
open! Import

(** Session-cookie based authentication. This is served over HTTP rather than RPC, so that the token stays in an [HttpOnly] cookie that page scripts cannot read. Login attempts are rate-limited. *)

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
val session_token : Cohttp.Request.t -> string option

val current_user_id
  :  db:Database.t
  -> now:Time_ns.t
  -> session_token:string option
  -> int option Deferred.Or_error.t
