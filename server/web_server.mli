open! Core
open! Async
open! Import

(** The blog's single listening port, which serves two things at once:

  1) the RPCs of [Rpcs], over a websocket, implemented by [Rpc_implementations];
  2) the frontend assets, over plain HTTP, routed by [Http_route].

  Both share one [Cohttp_async] server, because a websocket connection is itself
  established by an HTTP upgrade request. *)

module Http_route : sig
  type t =
    | Media of { path : string }
    | Static of { path : string }
    | Page (* Single-page application at [index.html] *)
    | Login
    | Logout
    | Not_found (* [index.html] with status 404 *)

  val of_request : db:Database.t -> now:Time_ns.t -> Cohttp.Request.t -> t Deferred.t
end

(** Starts listening. Pass a port of 0 to bind an arbitrary free port, which
    [Cohttp_async.Server.listening_on] then reports. *)
val serve
  :  time_source:Time_source.t
  -> Database.t
  -> Config.t
  -> (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t
