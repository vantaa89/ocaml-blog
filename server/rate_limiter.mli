open! Core
open! Async

(** In-memory counter to rate-limit activities. Currently used only for login attempts *)

type t

val create : max_attempts:int -> window:Time_ns.Span.t -> time_source:Time_source.t -> t
val record_attempt : t -> key:string -> [ `Allowed | `Too_many ]
val clear : t -> key:string -> unit
