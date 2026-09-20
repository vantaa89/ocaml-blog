open! Core

(** The URL layout the client and the server have to agree on. *)

(** Where the RPC websocket is served. *)
val websocket_path : string

val media_path : string
val login_path : string
val logout_path : string
