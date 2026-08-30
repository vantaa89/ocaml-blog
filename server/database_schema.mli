open! Core
open! Async
open! Import

module User : sig
  type t =
    { id : int
    ; username : string
    ; email : string
    ; password_hash : string
    ; date_joined : Date.t
    ; last_login : Time_ns.t option
    }
  [@@deriving compare]

  val table : string
  val create_sql : string
  val columns : string list
  val of_row : Pgx.Value.t list -> t
end

module Image : sig
  type t =
    { id : int
    ; filename : string
    ; date : Date.t
    }
  [@@deriving compare]

  val table : string
  val create_sql : string
  val columns : string list
  val of_row : Pgx.Value.t list -> t
  val url : t -> media_url:string -> string
end

module Tag : sig
  type t =
    { id : int
    ; name : string
    ; slug : string
    }
  [@@deriving compare]

  val table : string
  val create_sql : string
  val columns : string list
  val of_row : Pgx.Value.t list -> t
end

module Post : sig
  type t =
    { id : int
    ; title : string
    ; slug : string
    ; content_en : string option
    ; content_ko : string option
    ; author_id : int
    ; created_at : Time_ns.t
    ; special_post : bool
    ; hidden : bool
    }
  [@@deriving compare]

  val table : string
  val create_sql : string
  val columns : string list
  val available_languages : t -> Language.t list
  val of_row : Pgx.Value.t list -> t
end

module Publication : sig
  type t =
    { id : int
    ; title : string
    ; image_id : int
    ; authors : string
    ; journal : string
    ; link : string option
    ; hidden : bool
    }
  [@@deriving compare]

  val table : string
  val create_sql : string
  val columns : string list
  val of_row : Pgx.Value.t list -> t
end

module News : sig
  type t =
    { id : int
    ; content : string
    ; date : Date.t
    }
  [@@deriving compare]

  val table : string
  val create_sql : string
  val columns : string list
  val of_row : Pgx.Value.t list -> t
end

module Post_tag : sig
  type t =
    { post_id : int
    ; tag_id : int
    }
  [@@deriving compare]

  val table : string
  val create_sql : string list
end

val create_sql : string list
