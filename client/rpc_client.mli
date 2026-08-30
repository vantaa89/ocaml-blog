open! Core
open! Import

(** How the client reaches the server's rpc websocket. *)

val where_to_connect : Rpc_effect.Where_to_connect.t
val retry_interval : Time_ns.Span.t
