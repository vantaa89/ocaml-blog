open! Core
open! Async
open! Import

(** Runs a [Web_server] over an in-memory database, and talks to it the way a browser
    would. *)

type t

val with_server : Database.t -> f:(t -> 'a Deferred.t) -> 'a Deferred.t
val advance_clock : t -> Time_ns.Span.t -> unit Deferred.t

(** Posts a login form holding only the fields given, and returns the response along with
    the session token it sets, if any. *)
val login
  :  ?origin:string (* default: the server's own, [http://127.0.0.1:<port>] *)
  -> ?username:string
  -> ?password:string
  -> t
  -> (Cohttp.Response.t * string option) Deferred.t

val logout : ?token:string -> t -> Cohttp.Response.t Deferred.t

val with_rpc_connection
  :  ?token:string
  -> t
  -> f:(Rpc.Connection.t -> 'a Deferred.t)
  -> 'a Deferred.t
