open! Core
open! Async
open! Import

(* [Pgx_async.Value]'s own [to_time] and [of_time] go through [Time_float], and parse
   offset-less timestamps in the local zone. Postgres sends and accepts timestamps as
   text, so convert that text to and from [Time_ns] directly instead. *)
let time_ns_of_value_exn value =
  Pgx_async.Value.to_string_exn value |> Time_ns.of_string_with_utc_offset
;;

let value_of_time_ns time = Time_ns.to_string_utc time |> Pgx_async.Value.of_string

module User = struct
  type t =
    { id : int
    ; username : string
    ; email : string
    ; password_hash : string
    ; date_joined : Date.t
    ; last_login : Time_ns.t option
    }
  [@@deriving fields, compare]

  let table = "users"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; username; email; password_hash; date_joined; last_login ] ->
      { id = Value.to_int_exn id
      ; username = Value.to_string_exn username
      ; email = Value.to_string_exn email
      ; password_hash = Value.to_string_exn password_hash
      ; date_joined = Value.to_date_exn date_joined
      ; last_login =
          (match last_login with
           | None -> None
           | Some _ -> Some (time_ns_of_value_exn last_login))
      }
    | _ -> raise_s [%message "Unexpected row shape for User" (row : Value.t list)]
  ;;

  let create_sql =
    [%string
      {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      username TEXT NOT NULL UNIQUE,
      email TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      date_joined DATE NOT NULL,
      last_login TIMESTAMPTZ
    )
    |sql}]
  ;;
end

module Image = struct
  type t =
    { id : int
    ; filename : string
    ; date : Date.t
    }
  [@@deriving fields, compare]

  let url t ~media_url = media_url ^/ Date.to_string t.date ^/ t.filename
  let table = "image"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; filename; date ] ->
      { id = Value.to_int_exn id
      ; filename = Value.to_string_exn filename
      ; date = Value.to_date_exn date
      }
    | _ -> raise_s [%message "Unexpected row shape for Image" (row : Value.t list)]
  ;;

  let create_sql =
    [%string
      {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      filename TEXT NOT NULL,
      date DATE NOT NULL
    )
    |sql}]
  ;;
end

module Tag = struct
  type t =
    { id : int
    ; name : string
    ; slug : string
    }
  [@@deriving fields, compare]

  let table = "tag"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; name; slug ] ->
      { id = Value.to_int_exn id
      ; name = Value.to_string_exn name
      ; slug = Value.to_string_exn slug
      }
    | _ -> raise_s [%message "Unexpected row shape for Tag" (row : Value.t list)]
  ;;

  let create_sql =
    [%string
      {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      slug TEXT NOT NULL UNIQUE
    )
    |sql}]
  ;;
end

module Post = struct
  type t =
    { id : int
    ; title : string
    ; slug : string
    ; content_en : string option
    ; content_ko : string option
    ; author_id : int
    ; created_at : Time_ns.t
    ; special_post : bool (* pinned to the about/main pages *)
    ; hidden : bool (* soft delete *)
    }
  [@@deriving fields, compare]

  let table = "post"

  let content_by_language t : string Language.Map.t =
    [ Language.English, t.content_en; Korean, t.content_ko ]
    |> List.filter_map ~f:(fun (language, content) ->
      Option.map content ~f:(fun content -> language, content))
    |> Language.Map.of_alist_exn
  ;;

  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id
      ; title
      ; slug
      ; content_en
      ; content_ko
      ; author_id
      ; created_at
      ; special_post
      ; hidden
      ] ->
      { id = Value.to_int_exn id
      ; title = Value.to_string_exn title
      ; slug = Value.to_string_exn slug
      ; content_en = Value.to_string content_en
      ; content_ko = Value.to_string content_ko
      ; author_id = Value.to_int_exn author_id
      ; created_at = time_ns_of_value_exn created_at
      ; special_post = Value.to_bool_exn special_post
      ; hidden = Value.to_bool_exn hidden
      }
    | _ -> raise_s [%message "Unexpected row shape for Post" (row : Value.t list)]
  ;;

  let create_sql =
    [%string
      {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      title TEXT NOT NULL,
      slug TEXT NOT NULL UNIQUE,
      content_en TEXT,
      content_ko TEXT,
      author_id INT NOT NULL REFERENCES %{User.table} (id),
      created_at TIMESTAMPTZ NOT NULL,
      special_post BOOLEAN NOT NULL,
      hidden BOOLEAN NOT NULL
    )
    |sql}]
  ;;
end

module Publication = struct
  type t =
    { id : int
    ; title : string
    ; image_id : int
    ; authors : string
    ; journal : string
    ; link : string option
    ; hidden : bool
    }
  [@@deriving fields, compare]

  let table = "publication"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; title; image_id; authors; journal; link; hidden ] ->
      { id = Value.to_int_exn id
      ; title = Value.to_string_exn title
      ; image_id = Value.to_int_exn image_id
      ; authors = Value.to_string_exn authors
      ; journal = Value.to_string_exn journal
      ; link = Value.to_string link
      ; hidden = Value.to_bool_exn hidden
      }
    | _ -> raise_s [%message "Unexpected row shape for Publication" (row : Value.t list)]
  ;;

  let create_sql =
    [%string
      {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      title TEXT NOT NULL,
      image_id INT NOT NULL REFERENCES %{Image.table} (id),
      authors TEXT NOT NULL,
      journal TEXT NOT NULL,
      link TEXT,
      hidden BOOLEAN NOT NULL
    )
    |sql}]
  ;;
end

module News = struct
  type t =
    { id : int
    ; content : string
    ; date : Date.t
    }
  [@@deriving fields, compare]

  let table = "news"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; content; date ] ->
      { id = Value.to_int_exn id
      ; content = Value.to_string_exn content
      ; date = Value.to_date_exn date
      }
    | _ -> raise_s [%message "Unexpected row shape for News" (row : Value.t list)]
  ;;

  let create_sql =
    [%string
      {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      content TEXT NOT NULL,
      date DATE NOT NULL
    )
    |sql}]
  ;;
end

module Session = struct
  type t =
    { token_hash : string (* SHA-256 of the token in the session cookie *)
    ; user_id : int
    ; expires_at : Time_ns.t
    }
  [@@deriving fields, compare]

  let table = "session"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ token_hash; user_id; expires_at ] ->
      { token_hash = Value.to_string_exn token_hash
      ; user_id = Value.to_int_exn user_id
      ; expires_at = time_ns_of_value_exn expires_at
      }
    | _ -> raise_s [%message "Unexpected row shape for Session" (row : Value.t list)]
  ;;

  (* Needs secondary index based on [expires_at] to sweep expired sessions *)
  let create_sql =
    [ [%string
        {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      token_hash TEXT PRIMARY KEY,
      user_id INT NOT NULL REFERENCES %{User.table} (id) ON DELETE CASCADE,
      expires_at TIMESTAMPTZ NOT NULL
    )
    |sql}]
    ; [%string
        "CREATE INDEX IF NOT EXISTS %{table}_expires_at_idx ON %{table} (expires_at)"]
    ]
  ;;
end

module Post_tag = struct
  type t =
    { post_id : int
    ; tag_id : int
    }
  [@@deriving compare]

  let table = "post_tags"

  (* Needs secondary index based on [tag_id] *)
  let create_sql =
    [ [%string
        {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      post_id INT NOT NULL REFERENCES %{Post.table} (id),
      tag_id INT NOT NULL REFERENCES %{Tag.table} (id),
      PRIMARY KEY (post_id, tag_id)
    )
    |sql}]
    ; [%string "CREATE INDEX IF NOT EXISTS %{table}_tag_id_idx ON %{table} (tag_id)"]
    ]
  ;;
end

let create_sql =
  [ User.create_sql
  ; Image.create_sql
  ; Tag.create_sql
  ; Post.create_sql
  ; Publication.create_sql
  ; News.create_sql
  ]
  @ Session.create_sql
  @ Post_tag.create_sql
;;
