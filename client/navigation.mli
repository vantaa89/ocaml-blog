open! Core
open! Import

(** Where the user is in the app, and how to send them somewhere else. *)

(** The route the address bar currently describes. *)
val current : Route.t Value.t

(** Navigates to [route], pushing an entry onto the browser's history stack. Used by
    {!Client_utils.link} for in-app [<a>] clicks and by the search modal. *)
val go_to : Route.t -> unit Effect.t
