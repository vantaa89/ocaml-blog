open! Core
open! Async
open! Import

module Connection_state : sig
  type t =
    { db : Database.t
    ; session_token : string option
    }
end

(** Implements the RPCs described in {!Rpcs} *)
val implementations : Connection_state.t Rpc.Implementations.t
