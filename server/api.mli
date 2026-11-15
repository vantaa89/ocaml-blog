open! Core
open! Async
open! Import

module Endpoint : sig
  type t =
    | About (** An HTML document *)
    | Cv
  [@@deriving sexp_of]
end

(** The token is shown only this once: the database keeps only its digest. *)
val create_token
  :  db:Database.t
  -> label:string
  -> user_id:int
  -> now:Time_ns.t
  -> Token.t Deferred.Or_error.t

(** Answers [endpoint] only for a request presenting a valid API token as
    [Authorization: Bearer]; a session does not grant access. *)
val respond
  :  db:Database.t
  -> media_dir:string
  -> body:Cohttp_async.Body.t
  -> Endpoint.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response Deferred.t
