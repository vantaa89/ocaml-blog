open! Core

(** The blog's owner, as written in [owner_profile.sexp] when this was built. *)

module Social_media : sig
  type t =
    | Github
    | Linkedin
    | Instagram
    | Email
    | Cv
end

type t =
  { name : string
  ; description : string
  ; profile_picture : string
  ; links : (Social_media.t * string) list
  }
[@@deriving of_sexp]

val info : t
