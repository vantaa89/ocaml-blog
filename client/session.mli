open! Core
open! Import

(** The client's side of authentication. Credentials go over HTTP rather than rpc, so that
    the session token lands in an [HttpOnly] cookie that page scripts cannot read. *)

val reload_home : unit -> unit Effect.t
val log_in : username:string -> password:string -> unit Or_error.t Effect.t
val log_out : unit -> unit Or_error.t Effect.t

(** Who the server says this connection belongs to. Reads as [Not_logged_in] until the
    first response arrives. *)
val current_user : Rpcs.Get_current_user.Response.t Computation.t
