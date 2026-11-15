open! Core
open! Import

(** The bearer-side credentials of sessions and API tokens. The client holds the token itself
    and the database keeps only its digest, so a leaked table does not hand out usable
    credentials. *)

type t

val generate : unit -> t

include Stringable.S with type t := t

module Hash : sig
  type t [@@deriving equal, sexp_of]

  include Stringable.S with type t := t
end

(** The SHA-256 digest of a token, in hex *)
val hash : t -> Hash.t
