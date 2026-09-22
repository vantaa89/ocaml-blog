open! Core
open! Async
open! Import

module Connection_state : sig
  type t = { session_token : string option }
end

(** Implements the RPCs described in {!Rpcs} *)
val implementations
  :  db:Database.t
  -> time_source:Time_source.t
  -> Config.t
  -> Connection_state.t Rpc.Implementations.t
