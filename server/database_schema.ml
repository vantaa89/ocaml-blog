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

let token_hash_of_value_exn value =
  Pgx_async.Value.to_string_exn value |> Token.Hash.of_string
;;

let value_of_token_hash hash = Token.Hash.to_string hash |> Pgx_async.Value.of_string

module type Table = sig
  type t

  val table : string
  val columns : string list
  val create_sql : string list
  val of_row : Pgx.Value.t list -> t
  val to_row : t -> Pgx.Value.t list
end

module User = struct
  type t =
    { id : User_id.t
    ; username : string
    ; email : string
    ; password_hash : string
    ; date_joined : Date.t
    ; last_login : Time_ns.t option
    }
  [@@deriving fields]

  let table = "users"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; username; email; password_hash; date_joined; last_login ] ->
      { id = User_id.of_pgx_async_value_exn id
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

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~id:(value User_id.to_pgx_async_value)
      ~username:(value Value.of_string)
      ~email:(value Value.of_string)
      ~password_hash:(value Value.of_string)
      ~date_joined:(value Value.of_date)
      ~last_login:(value (Value.opt value_of_time_ns))
  ;;

  let create_sql =
    [ [%string
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
    ]
  ;;
end

module Image = struct
  type t =
    { id : Image_id.t
    ; filename : string
    ; date : Date.t
    }
  [@@deriving fields]

  let url t = Urls.media_path ^/ Date.to_string t.date ^/ t.filename
  let table = "image"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; filename; date ] ->
      { id = Image_id.of_pgx_async_value_exn id
      ; filename = Value.to_string_exn filename
      ; date = Value.to_date_exn date
      }
    | _ -> raise_s [%message "Unexpected row shape for Image" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~id:(value Image_id.to_pgx_async_value)
      ~filename:(value Value.of_string)
      ~date:(value Value.of_date)
  ;;

  let create_sql =
    [ [%string
        {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      filename TEXT NOT NULL,
      date DATE NOT NULL
    )
    |sql}]
    ]
  ;;
end

module Tag = struct
  type t =
    { id : Tag_id.t
    ; name : string
    }
  [@@deriving fields]

  let table = "tag"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; name ] ->
      { id = Tag_id.of_pgx_async_value_exn id; name = Value.to_string_exn name }
    | _ -> raise_s [%message "Unexpected row shape for Tag" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list ~id:(value Tag_id.to_pgx_async_value) ~name:(value Value.of_string)
  ;;

  let create_sql =
    [ [%string
        {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL
    )
    |sql}]
    ; [%string
        "CREATE UNIQUE INDEX IF NOT EXISTS %{table}_lower_name_key ON %{table} \
         (lower(name))"]
    ]
  ;;
end

module Post = struct
  type t =
    { id : Post_id.t
    ; title : string
    ; slug : string
    ; content_en : string option
    ; content_ko : string option
    ; author_id : User_id.t
    ; created_at : Time_ns.t
    ; special_post : bool (* pinned to the about/main pages *)
    ; hidden : bool (* soft delete *)
    }
  [@@deriving fields]

  let table = "post"

  let content_by_language t : string Language.Map.t =
    Language.Map.of_alist_exn [ English, t.content_en; Korean, t.content_ko ]
    |> Map.filter_map ~f:Fn.id
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
      { id = Post_id.of_pgx_async_value_exn id
      ; title = Value.to_string_exn title
      ; slug = Value.to_string_exn slug
      ; content_en = Value.to_string content_en
      ; content_ko = Value.to_string content_ko
      ; author_id = User_id.of_pgx_async_value_exn author_id
      ; created_at = time_ns_of_value_exn created_at
      ; special_post = Value.to_bool_exn special_post
      ; hidden = Value.to_bool_exn hidden
      }
    | _ -> raise_s [%message "Unexpected row shape for Post" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~id:(value Post_id.to_pgx_async_value)
      ~title:(value Value.of_string)
      ~slug:(value Value.of_string)
      ~content_en:(value (Value.opt Value.of_string))
      ~content_ko:(value (Value.opt Value.of_string))
      ~author_id:(value User_id.to_pgx_async_value)
      ~created_at:(value value_of_time_ns)
      ~special_post:(value Value.of_bool)
      ~hidden:(value Value.of_bool)
  ;;

  let create_sql =
    [ [%string
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
    ]
  ;;
end

module Publication = struct
  type t =
    { id : Publication_id.t
    ; title : string
    ; image_id : Image_id.t
    ; authors : string
    ; journal : string
    ; link : string option
    ; hidden : bool
    }
  [@@deriving fields]

  let table = "publication"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; title; image_id; authors; journal; link; hidden ] ->
      { id = Publication_id.of_pgx_async_value_exn id
      ; title = Value.to_string_exn title
      ; image_id = Image_id.of_pgx_async_value_exn image_id
      ; authors = Value.to_string_exn authors
      ; journal = Value.to_string_exn journal
      ; link = Value.to_string link
      ; hidden = Value.to_bool_exn hidden
      }
    | _ -> raise_s [%message "Unexpected row shape for Publication" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~id:(value Publication_id.to_pgx_async_value)
      ~title:(value Value.of_string)
      ~image_id:(value Image_id.to_pgx_async_value)
      ~authors:(value Value.of_string)
      ~journal:(value Value.of_string)
      ~link:(value (Value.opt Value.of_string))
      ~hidden:(value Value.of_bool)
  ;;

  let create_sql =
    [ [%string
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
    ]
  ;;
end

module News = struct
  type t =
    { id : News_id.t
    ; content : string
    ; date : Date.t
    }
  [@@deriving fields]

  let table = "news"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ id; content; date ] ->
      { id = News_id.of_pgx_async_value_exn id
      ; content = Value.to_string_exn content
      ; date = Value.to_date_exn date
      }
    | _ -> raise_s [%message "Unexpected row shape for News" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~id:(value News_id.to_pgx_async_value)
      ~content:(value Value.of_string)
      ~date:(value Value.of_date)
  ;;

  let create_sql =
    [ [%string
        {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      id SERIAL PRIMARY KEY,
      content TEXT NOT NULL,
      date DATE NOT NULL
    )
    |sql}]
    ]
  ;;
end

module Session = struct
  type t =
    { token_hash : Token.Hash.t (* of the token in the session cookie *)
    ; user_id : User_id.t
    ; expires_at : Time_ns.t
    }
  [@@deriving fields]

  let table = "session"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ token_hash; user_id; expires_at ] ->
      { token_hash = token_hash_of_value_exn token_hash
      ; user_id = User_id.of_pgx_async_value_exn user_id
      ; expires_at = time_ns_of_value_exn expires_at
      }
    | _ -> raise_s [%message "Unexpected row shape for Session" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~token_hash:(value value_of_token_hash)
      ~user_id:(value User_id.to_pgx_async_value)
      ~expires_at:(value value_of_time_ns)
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

module Api_token = struct
  type t =
    { label : string
    ; token_hash : Token.Hash.t (* of the token in the [Authorization] header *)
    ; user_id : User_id.t
    ; created_at : Time_ns.t
    }
  [@@deriving fields]

  let table = "api_token"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ label; token_hash; user_id; created_at ] ->
      { label = Value.to_string_exn label
      ; token_hash = token_hash_of_value_exn token_hash
      ; user_id = User_id.of_pgx_async_value_exn user_id
      ; created_at = time_ns_of_value_exn created_at
      }
    | _ -> raise_s [%message "Unexpected row shape for Api_token" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~label:(value Value.of_string)
      ~token_hash:(value value_of_token_hash)
      ~user_id:(value User_id.to_pgx_async_value)
      ~created_at:(value value_of_time_ns)
  ;;

  let create_sql =
    [ [%string
        {sql|
    CREATE TABLE IF NOT EXISTS %{table} (
      label TEXT PRIMARY KEY,
      token_hash TEXT NOT NULL UNIQUE,
      user_id INT NOT NULL REFERENCES %{User.table} (id) ON DELETE CASCADE,
      created_at TIMESTAMPTZ NOT NULL
    )
    |sql}]
    ]
  ;;
end

module Post_tag = struct
  type t =
    { post_id : Post_id.t
    ; tag_id : Tag_id.t
    }
  [@@deriving fields]

  let table = "post_tags"
  let columns = Fields.names

  let of_row row : t =
    let module Value = Pgx_async.Value in
    match row with
    | [ post_id; tag_id ] ->
      { post_id = Post_id.of_pgx_async_value_exn post_id
      ; tag_id = Tag_id.of_pgx_async_value_exn tag_id
      }
    | _ -> raise_s [%message "Unexpected row shape for Post_tag" (row : Value.t list)]
  ;;

  let to_row t =
    let module Value = Pgx_async.Value in
    let value to_value field = to_value (Field.get field t) in
    Fields.to_list
      ~post_id:(value Post_id.to_pgx_async_value)
      ~tag_id:(value Tag_id.to_pgx_async_value)
  ;;

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
  List.concat
    [ User.create_sql
    ; Image.create_sql
    ; Tag.create_sql
    ; Post.create_sql
    ; Publication.create_sql
    ; News.create_sql
    ; Session.create_sql
    ; Api_token.create_sql
    ; Post_tag.create_sql
    ]
;;
