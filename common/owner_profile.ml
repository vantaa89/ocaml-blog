open! Core

module Social_media = struct
  type t =
    | Github
    | Linkedin
    | Instagram
    | Email
    | Cv
  [@@deriving of_sexp]
end

type t =
  { name : string
  ; description : string
  ; profile_picture : string
  ; links : (Social_media.t * string) list
  }
[@@deriving of_sexp]

let info = Sexp.of_string [%blob "../owner_profile.sexp"] |> t_of_sexp
