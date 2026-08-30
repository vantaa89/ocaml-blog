open! Core
open! Async
open! Import

(** The blog's single listening port, which serves two things at once:

  1) the RPCs of {!Rpcs}, over a websocket, implemented by {!Rpc_implementations};
  2) the frontend assets, over plain HTTP, routed by {!Http_route}.

  Both share one [Cohttp_async] server, because a websocket connection is itself
  established by an HTTP upgrade request. *)

module Http_route : sig
  type t =
    | Media of { path : string }
    | Static of { path : string }
    | Index (* Single-page application *)
    | Not_found

  val of_request : meth:Cohttp.Code.meth -> path:string -> t
end

(** Starts listening. Pass a port of 0 to bind an arbitrary free port, which
    {!Cohttp_async.Server.listening_on} then reports. *)
val serve
  :  Database.t
  -> Config.t
  -> (Socket.Address.Inet.t, int) Cohttp_async.Server.t Deferred.t
