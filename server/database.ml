open! Core
open! Async
open! Import

type t =
  | Real of { connection : Pgx_async.t }
  | Mock

let with_connection ~f =
  Pgx_async.with_conn ~database:"blogdb" (fun connection ->
    let t = Real { connection } in
    f t)
;;

let create_tables t =
  match t with
  | Mock -> Deferred.return (Ok ())
  | Real { connection } ->
    Deferred.Or_error.try_with (fun () ->
      Deferred.List.iter Database_schema.create_sql ~how:`Sequential ~f:(fun sql ->
        Pgx_async.execute_unit connection sql))
;;

module Post = struct
  let find_by_slug t ~slug =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string slug ]
            [%string
              "SELECT %{Database_schema.Post.columns} FROM blogapp_post WHERE slug = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          sprintf "Expected at most one post for slug %s" slug)
        |> Option.map ~f:Database_schema.Post.of_row)
  ;;

  let find_by_id t ~id =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int id ]
            [%string
              "SELECT %{Database_schema.Post.columns} FROM blogapp_post WHERE id = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          sprintf "Expected at most one post for id %d" id)
        |> Option.map ~f:Database_schema.Post.of_row)
  ;;

  let list t ?(include_hidden = false) ?limit ?(offset = 0) () =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let where =
          match include_hidden with
          | true -> ""
          | false -> "WHERE NOT hidden"
        in
        let limit_clause =
          match limit with
          | None -> ""
          | Some limit -> [%string "LIMIT %{limit#Int}"]
        in
        let%map rows =
          Pgx_async.execute
            connection
            [%string
              "SELECT %{Database_schema.Post.columns} FROM blogapp_post %{where} ORDER \
               BY created_at DESC %{limit_clause} OFFSET %{offset#Int}"]
        in
        List.map rows ~f:Database_schema.Post.of_row)
  ;;

  let find_by_title t ~title =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string title ]
            [%string
              "SELECT %{Database_schema.Post.columns} FROM blogapp_post WHERE title = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          sprintf "Expected at most one post for title %s" title)
        |> Option.map ~f:Database_schema.Post.of_row)
  ;;

  let list_by_tag_slug t ~slug ?(include_hidden = false) ?limit ?(offset = 0) () =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let where =
          match include_hidden with
          | true -> ""
          | false -> "AND NOT p.hidden"
        in
        let limit_clause =
          match limit with
          | None -> ""
          | Some limit -> [%string "LIMIT %{limit#Int}"]
        in
        let columns =
          Database_schema.Post.columns
          |> String.split ~on:','
          |> List.map ~f:(fun column -> [%string "p.%{String.strip column}"])
          |> String.concat ~sep:", "
        in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string slug ]
            [%string
              "SELECT %{columns} FROM blogapp_post p JOIN blogapp_post_tags pt ON \
               p.id = pt.post_id JOIN tag ON pt.tag_id = tag.id WHERE tag.slug = $1 \
               %{where} ORDER BY p.created_at DESC %{limit_clause} OFFSET %{offset#Int}"]
        in
        List.map rows ~f:Database_schema.Post.of_row)
  ;;

  let search t ~query ?(limit = 8) () =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let pattern = [%string "%%%{query}%%"] in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string pattern; Value.of_int limit ]
            [%string
              "SELECT %{Database_schema.Post.columns} FROM blogapp_post WHERE NOT \
               special_post AND NOT hidden AND (title ILIKE $1 OR content_en ILIKE $1 \
               OR content_ko ILIKE $1) ORDER BY created_at DESC LIMIT $2"]
        in
        List.map rows ~f:Database_schema.Post.of_row)
  ;;

  let set_hidden t ~id ~hidden =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot update the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.execute_unit
          connection
          ~params:[ Value.of_bool hidden; Value.of_int id ]
          "UPDATE blogapp_post SET hidden = $1 WHERE id = $2")
  ;;

  let create t ~title ~slug ~content_en ~content_ko ~author_id ~special_post =
    match t with
    | Mock ->
      (* TODO: implement post creation against this *)
      Deferred.Or_error.error_string "cannot create a post against the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let created_at = Time_ns.now () in
        let hidden = false in
        let params =
          [ Value.of_string title
          ; Value.of_string slug
          ; Value.opt Value.of_string content_en
          ; Value.opt Value.of_string content_ko
          ; Value.of_int author_id
          ; Value.of_time (Time_ns.to_time_float_round_nearest_microsecond created_at)
          ; Value.of_bool special_post
          ; Value.of_bool hidden
          ]
        in
        let%map result =
          Pgx_async.execute
            connection
            ~params
            {sql|
          INSERT INTO blogapp_post
            (title, slug, content_en, content_ko, author_id, created_at, special_post, hidden)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
          RETURNING id
          |sql}
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value
           ; title
           ; slug
           ; content_en
           ; content_ko
           ; author_id
           ; created_at
           ; special_post
           ; hidden
           }
           : Database_schema.Post.t)
        | ([] | [ _ :: _ ] | _ :: _) as rows ->
          raise_s
            [%message "Unexpected result from Post insert" (rows : Value.t list list)])
  ;;
end

module User = struct
  let create t ~username ~email ~password_hash ~date_joined =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot create a user against the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map result =
          Pgx_async.execute
            connection
            ~params:
              [ Value.of_string username
              ; Value.of_string email
              ; Value.of_string password_hash
              ; Value.of_date date_joined
              ]
            {sql|
            INSERT INTO users (username, email, password_hash, date_joined)
            VALUES ($1, $2, $3, $4)
            RETURNING id
            |sql}
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value
           ; username
           ; email
           ; password_hash
           ; date_joined
           ; last_login = None
           }
           : Database_schema.User.t)
        | rows ->
          raise_s
            [%message "Unexpected result from User insert" (rows : Value.t list list)])
  ;;

  let find_by_username t ~username =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string username ]
            [%string
              "SELECT %{Database_schema.User.columns} FROM users WHERE username = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          sprintf "Expected at most one user for username %s" username)
        |> Option.map ~f:Database_schema.User.of_row)
  ;;
end

module Image = struct
  let create t ~filename ~date =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot create an image against the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map result =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string filename; Value.of_date date ]
            {sql|
            INSERT INTO blogapp_image (filename, date)
            VALUES ($1, $2)
            RETURNING id
            |sql}
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value; filename; date } : Database_schema.Image.t)
        | rows ->
          raise_s
            [%message "Unexpected result from Image insert" (rows : Value.t list list)])
  ;;
end

module Tag = struct
  let find_by_slug t ~slug =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string slug ]
            [%string "SELECT %{Database_schema.Tag.columns} FROM tag WHERE slug = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          sprintf "Expected at most one tag for slug %s" slug)
        |> Option.map ~f:Database_schema.Tag.of_row)
  ;;

  let find_or_create t ~name ~slug =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot create a tag against the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string name; Value.of_string slug ]
            {sql|
            INSERT INTO tag (name, slug)
            VALUES ($1, $2)
            ON CONFLICT (slug) DO UPDATE SET name = EXCLUDED.name
            RETURNING id, name, slug
            |sql}
        in
        match rows with
        | [ row ] -> Database_schema.Tag.of_row row
        | rows ->
          raise_s
            [%message "Unexpected result from Tag upsert" (rows : Value.t list list)])
  ;;

  (** Tags that are attached to at least one post, most-used first. *)
  let list_with_post_counts t =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            {sql|
            SELECT tag.id, tag.name, tag.slug, COUNT(*)
            FROM tag
            JOIN blogapp_post_tags ON tag.id = blogapp_post_tags.tag_id
            GROUP BY tag.id
            ORDER BY COUNT(*) DESC
            |sql}
        in
        List.map rows ~f:(fun row ->
          match row with
          | [ id; name; slug; count ] ->
            Database_schema.Tag.of_row [ id; name; slug ], Value.to_int_exn count
          | _ -> raise_s [%message "Unexpected row shape for Tag count" (row : Value.t list)]))
  ;;
end

module Post_tag = struct
  let tags_for_post t ~post_id =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int post_id ]
            [%string
              "SELECT %{Database_schema.Tag.columns} FROM tag JOIN blogapp_post_tags ON \
               tag.id = blogapp_post_tags.tag_id WHERE blogapp_post_tags.post_id = $1"]
        in
        List.map rows ~f:Database_schema.Tag.of_row)
  ;;

  let set_tags t ~post_id ~tag_ids =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot update the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.with_transaction connection (fun connection ->
          let%bind () =
            Pgx_async.execute_unit
              connection
              ~params:[ Value.of_int post_id ]
              "DELETE FROM blogapp_post_tags WHERE post_id = $1"
          in
          Deferred.List.iter tag_ids ~how:`Sequential ~f:(fun tag_id ->
            Pgx_async.execute_unit
              connection
              ~params:[ Value.of_int post_id; Value.of_int tag_id ]
              "INSERT INTO blogapp_post_tags (post_id, tag_id) VALUES ($1, $2)")))
  ;;
end

module Publication = struct
  let create t ~title ~image_id ~authors ~journal ~link =
    match t with
    | Mock ->
      Deferred.Or_error.error_string "cannot create a publication against the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let hidden = false in
        let%map result =
          Pgx_async.execute
            connection
            ~params:
              [ Value.of_string title
              ; Value.of_int image_id
              ; Value.of_string authors
              ; Value.of_string journal
              ; Value.opt Value.of_string link
              ; Value.of_bool hidden
              ]
            {sql|
            INSERT INTO publication (title, image_id, authors, journal, link, hidden)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING id
            |sql}
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value
           ; title
           ; image_id
           ; authors
           ; journal
           ; link
           ; hidden
           }
           : Database_schema.Publication.t)
        | rows ->
          raise_s
            [%message "Unexpected result from Publication insert" (rows : Value.t list list)])
  ;;

  let list t ?(include_hidden = false) () =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let where =
          match include_hidden with
          | true -> ""
          | false -> "WHERE NOT hidden"
        in
        let%map rows =
          Pgx_async.execute
            connection
            [%string
              "SELECT %{Database_schema.Publication.columns} FROM publication %{where}"]
        in
        List.map rows ~f:Database_schema.Publication.of_row)
  ;;
end

module News = struct
  let create t ~content ~date =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot create news against the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map result =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string content; Value.of_date date ]
            {sql|
            INSERT INTO news (content, date)
            VALUES ($1, $2)
            RETURNING id
            |sql}
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value; content; date } : Database_schema.News.t)
        | rows ->
          raise_s
            [%message "Unexpected result from News insert" (rows : Value.t list list)])
  ;;

  let list t ?limit () =
    match t with
    | Mock -> Deferred.Or_error.error_string "cannot query the mock database"
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let limit_clause =
          match limit with
          | None -> ""
          | Some limit -> [%string "LIMIT %{limit#Int}"]
        in
        let%map rows =
          Pgx_async.execute
            connection
            [%string
              "SELECT %{Database_schema.News.columns} FROM news ORDER BY date DESC \
               %{limit_clause}"]
        in
        List.map rows ~f:Database_schema.News.of_row)
  ;;
end

module For_testing = struct
  let create () = Mock
end
