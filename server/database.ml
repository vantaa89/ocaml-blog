open! Core
open! Async
open! Import

type t =
  | Real of { connection : Pgx_async.t }
  | Mock of
      { posts : Database_schema.Post.t list ref
      ; users : Database_schema.User.t list ref
      ; images : Database_schema.Image.t list ref
      ; tags : Database_schema.Tag.t list ref
      ; post_tags : Database_schema.Post_tag.t list ref
      ; publication : Database_schema.Publication.t list ref
      ; news : Database_schema.News.t list ref
      ; last_news_id : int ref
      ; sessions : Database_schema.Session.t list ref
      }

let is_unique rows ~field ~value =
  not (List.exists rows ~f:(fun row -> String.equal (field row) value))
;;

let next_id items ~id =
  List.fold items ~init:0 ~f:(fun highest item -> Int.max highest (id item)) + 1
;;

let with_connection ~f =
  Pgx_async.with_conn ~database:"blogdb" (fun connection ->
    let t = Real { connection } in
    f t)
;;

let create_tables t =
  match t with
  | Mock _ ->
    Deferred.Or_error.error_s
      [%message "Cannot create tables using mock database connection"]
  | Real { connection } ->
    Deferred.Or_error.try_with (fun () ->
      Deferred.List.iter Database_schema.create_sql ~how:`Sequential ~f:(fun sql ->
        Pgx_async.execute_unit connection sql))
;;

module Post = struct
  let find_by_slug t ~slug =
    Deferred.Or_error.try_with (fun () ->
      let%map matching_posts =
        match t with
        | Mock { posts; _ } ->
          Deferred.return
            (List.filter !posts ~f:(fun post -> String.equal post.slug slug))
        | Real { connection } ->
          let module Value = Pgx_async.Value in
          let columns = String.concat ~sep:", " Database_schema.Post.columns in
          let%map rows =
            Pgx_async.execute
              connection
              ~params:[ Value.of_string slug ]
              [%string
                "SELECT %{columns} FROM %{Database_schema.Post.table} WHERE slug = $1"]
          in
          List.map rows ~f:Database_schema.Post.of_row
      in
      Utils.expect_at_most_one matching_posts ~error_message:(fun _ ->
        [%message "Expected at most one post for slug" (slug : string)]))
  ;;

  let find_by_id t ~id =
    match t with
    | Mock { posts; _ } ->
      List.find !posts ~f:(fun post -> post.id = id) |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.Post.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int id ]
            [%string "SELECT %{columns} FROM %{Database_schema.Post.table} WHERE id = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          [%message "Expected at most one post for id" (id : int)])
        |> Option.map ~f:Database_schema.Post.of_row)
  ;;

  (* A hidden post belongs to its author alone. *)
  let visible_to ~viewer (post : Database_schema.Post.t) =
    (not post.hidden) || [%equal: int option] viewer (Some post.author_id)
  ;;

  let visible_sql ~prefix ~param =
    [%string "(NOT %{prefix}hidden OR %{prefix}author_id = $%{param#Int})"]
  ;;

  let list t ~viewer ?limit ?(offset = 0) () =
    match t with
    | Mock { posts; _ } ->
      !posts
      |> List.filter ~f:(fun post -> (not post.special_post) && visible_to ~viewer post)
      |> List.sort ~compare:(fun a b -> Time_ns.compare b.created_at a.created_at)
      |> (fun posts -> List.drop posts offset)
      |> (fun posts ->
      match limit with
      | None -> posts
      | Some limit -> List.take posts limit)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let limit_clause =
          match limit with
          | None -> ""
          | Some limit -> [%string "LIMIT %{limit#Int}"]
        in
        let columns = String.concat ~sep:", " Database_schema.Post.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.opt Value.of_int viewer ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Post.table} WHERE NOT \
               special_post AND %{visible_sql ~prefix:\"\" ~param:1} ORDER BY created_at \
               DESC %{limit_clause} OFFSET %{offset#Int}"]
        in
        List.map rows ~f:Database_schema.Post.of_row)
  ;;

  let list_by_tag t ~tag ~viewer ?limit ?(offset = 0) () =
    match t with
    | Mock { posts; tags; post_tags; _ } ->
      (match
         List.find !tags ~f:(fun candidate -> String.Caseless.equal candidate.name tag)
       with
       | None -> Deferred.Or_error.return []
       | Some { id = tag_id; _ } ->
         let post_ids =
           List.filter_map !post_tags ~f:(fun post_tag ->
             match tag_id = post_tag.tag_id with
             | true -> Some post_tag.post_id
             | false -> None)
           |> Int.Set.of_list
         in
         !posts
         |> List.filter ~f:(fun post ->
           Set.mem post_ids post.id && (not post.special_post) && visible_to ~viewer post)
         |> List.sort ~compare:(fun a b -> Time_ns.compare b.created_at a.created_at)
         |> (fun posts -> List.drop posts offset)
         |> (fun posts ->
         match limit with
         | None -> posts
         | Some limit -> List.take posts limit)
         |> Deferred.Or_error.return)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let where =
          [%string "AND NOT p.special_post AND %{visible_sql ~prefix:\"p.\" ~param:2}"]
        in
        let limit_clause =
          match limit with
          | None -> ""
          | Some limit -> [%string "LIMIT %{limit#Int}"]
        in
        let columns =
          Database_schema.Post.columns
          |> List.map ~f:(fun column -> "p." ^ column)
          |> String.concat ~sep:", "
        in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string tag; Value.opt Value.of_int viewer ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Post.table} p JOIN \
               %{Database_schema.Post_tag.table} pt ON p.id = pt.post_id JOIN \
               %{Database_schema.Tag.table} tag ON pt.tag_id = tag.id WHERE \
               lower(tag.name) = lower($1) %{where} ORDER BY p.created_at DESC \
               %{limit_clause} OFFSET %{offset#Int}"]
        in
        List.map rows ~f:Database_schema.Post.of_row)
  ;;

  let search t ~query ?(limit = 8) () =
    match t with
    | Mock { posts; _ } ->
      let matches text =
        Option.exists text ~f:(fun text ->
          String.Caseless.is_substring text ~substring:query)
      in
      List.filter !posts ~f:(fun post ->
        (not post.special_post)
        && (not post.hidden)
        && (String.Caseless.is_substring post.title ~substring:query
            || matches post.content_en
            || matches post.content_ko))
      |> List.sort ~compare:(fun a b -> Time_ns.compare b.created_at a.created_at)
      |> (fun posts -> List.take posts limit)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let pattern = [%string "%%%{query}%%"] in
        let columns = String.concat ~sep:", " Database_schema.Post.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string pattern; Value.of_int limit ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Post.table} WHERE NOT \
               special_post AND NOT hidden AND (title ILIKE $1 OR content_en ILIKE $1 OR \
               content_ko ILIKE $1) ORDER BY created_at DESC LIMIT $2"]
        in
        List.map rows ~f:Database_schema.Post.of_row)
  ;;

  let set_hidden t ~id ~hidden =
    match t with
    | Mock { posts; _ } ->
      posts
      := List.map !posts ~f:(fun post ->
           match post.id = id with
           | true -> { post with hidden }
           | false -> post);
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.execute_unit
          connection
          ~params:[ Value.of_bool hidden; Value.of_int id ]
          [%string "UPDATE %{Database_schema.Post.table} SET hidden = $1 WHERE id = $2"])
  ;;

  let create t ~title ~slug ~content_en ~content_ko ~author_id ~special_post ~now =
    match t with
    | Mock { posts; _ } ->
      (match is_unique !posts ~field:(fun post -> post.slug) ~value:slug with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message "duplicate key value violates unique constraint" (slug : string)])
       | true ->
         let post : Database_schema.Post.t =
           { id = next_id !posts ~id:(fun post -> post.id)
           ; title
           ; slug
           ; content_en
           ; content_ko
           ; author_id
           ; created_at = now
           ; special_post
           ; hidden = false
           }
         in
         posts := post :: !posts;
         Deferred.Or_error.return post)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let hidden = false in
        let params =
          [ Value.of_string title
          ; Value.of_string slug
          ; Value.opt Value.of_string content_en
          ; Value.opt Value.of_string content_ko
          ; Value.of_int author_id
          ; Database_schema.value_of_time_ns now
          ; Value.of_bool special_post
          ; Value.of_bool hidden
          ]
        in
        let%map result =
          Pgx_async.execute
            connection
            ~params
            [%string
              {sql|
          INSERT INTO %{Database_schema.Post.table}
            (title, slug, content_en, content_ko, author_id, created_at, special_post, hidden)
          VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
          RETURNING id
          |sql}]
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value
           ; title
           ; slug
           ; content_en
           ; content_ko
           ; author_id
           ; created_at = now
           ; special_post
           ; hidden
           }
           : Database_schema.Post.t)
        | ([] | [ _ :: _ ] | _ :: _) as rows ->
          raise_s
            [%message "Unexpected result from Post insert" (rows : Value.t list list)])
  ;;

  let update t ~id ~title ~slug ~content_en ~content_ko ~special_post =
    match t with
    | Mock { posts; _ } ->
      (* The post keeping its own slug is not a duplicate, so it is left out of the
         check. *)
      let others = List.filter !posts ~f:(fun post -> post.id <> id) in
      (match is_unique others ~field:(fun post -> post.slug) ~value:slug with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message "duplicate key value violates unique constraint" (slug : string)])
       | true ->
         posts
         := List.map !posts ~f:(fun post ->
              match post.id = id with
              | true -> { post with title; slug; content_en; content_ko; special_post }
              | false -> post);
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let params =
          [ Value.of_string title
          ; Value.of_string slug
          ; Value.opt Value.of_string content_en
          ; Value.opt Value.of_string content_ko
          ; Value.of_bool special_post
          ; Value.of_int id
          ]
        in
        Pgx_async.execute_unit
          connection
          ~params
          [%string
            {sql|
          UPDATE %{Database_schema.Post.table}
          SET title = $1, slug = $2, content_en = $3, content_ko = $4, special_post = $5
          WHERE id = $6
          |sql}])
  ;;
end

module User = struct
  let create t ~username ~email ~password_hash ~date_joined =
    match t with
    | Mock { users; _ } ->
      (match
         is_unique !users ~field:(fun user -> user.username) ~value:username
         && is_unique !users ~field:(fun user -> user.email) ~value:email
       with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message
                "duplicate key value violates unique constraint"
                  (username : string)
                  (email : string)])
       | true ->
         let user : Database_schema.User.t =
           { id = next_id !users ~id:(fun (user : Database_schema.User.t) -> user.id)
           ; username
           ; email
           ; password_hash
           ; date_joined
           ; last_login = None
           }
         in
         users := user :: !users;
         Deferred.Or_error.return user)
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
            [%string
              {sql|
            INSERT INTO %{Database_schema.User.table} (username, email, password_hash, date_joined)
            VALUES ($1, $2, $3, $4)
            RETURNING id
            |sql}]
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

  let find_by_id t ~id =
    match t with
    | Mock { users; _ } ->
      List.find !users ~f:(fun user -> user.id = id) |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.User.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int id ]
            [%string "SELECT %{columns} FROM %{Database_schema.User.table} WHERE id = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          [%message "Expected at most one user for id" (id : int)])
        |> Option.map ~f:Database_schema.User.of_row)
  ;;

  let list t =
    match t with
    | Mock { users; _ } ->
      List.sort !users ~compare:(fun a b -> Int.compare a.id b.id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let columns = String.concat ~sep:", " Database_schema.User.columns in
        let%map rows =
          Pgx_async.execute
            connection
            [%string "SELECT %{columns} FROM %{Database_schema.User.table} ORDER BY id"]
        in
        List.map rows ~f:Database_schema.User.of_row)
  ;;

  let set_password_hash t ~username ~password_hash =
    match t with
    | Mock { users; _ } ->
      users
      := List.map !users ~f:(fun user ->
           match String.equal user.username username with
           | false -> user
           | true -> { user with password_hash });
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.execute_unit
          connection
          ~params:[ Value.of_string password_hash; Value.of_string username ]
          [%string
            "UPDATE %{Database_schema.User.table} SET password_hash = $1 WHERE username \
             = $2"])
  ;;

  let set_last_login t ~username ~last_login =
    match t with
    | Mock { users; _ } ->
      users
      := List.map !users ~f:(fun user ->
           match String.equal user.username username with
           | false -> user
           | true -> { user with last_login = Some last_login });
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.execute_unit
          connection
          ~params:
            [ Database_schema.value_of_time_ns last_login; Value.of_string username ]
          [%string
            "UPDATE %{Database_schema.User.table} SET last_login = $1 WHERE username = $2"])
  ;;

  let delete t ~username =
    match t with
    | Mock { users; _ } ->
      users
      := List.filter !users ~f:(fun user -> not (String.equal user.username username));
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.execute_unit
          connection
          ~params:[ Value.of_string username ]
          [%string "DELETE FROM %{Database_schema.User.table} WHERE username = $1"])
  ;;

  let find_by_username t ~username =
    match t with
    | Mock { users; _ } ->
      List.find !users ~f:(fun user -> String.equal user.username username)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.User.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string username ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.User.table} WHERE username = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          [%message "Expected at most one user for username" (username : string)])
        |> Option.map ~f:Database_schema.User.of_row)
  ;;
end

module Image = struct
  let create t ~filename ~date =
    match t with
    | Mock { images; _ } ->
      let image : Database_schema.Image.t =
        { id = next_id !images ~id:(fun image -> image.id); filename; date }
      in
      images := image :: !images;
      Deferred.Or_error.return image
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map result =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string filename; Value.of_date date ]
            [%string
              {sql|
            INSERT INTO %{Database_schema.Image.table} (filename, date)
            VALUES ($1, $2)
            RETURNING id
            |sql}]
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value; filename; date } : Database_schema.Image.t)
        | rows ->
          raise_s
            [%message "Unexpected result from Image insert" (rows : Value.t list list)])
  ;;

  let find_by_id t ~id =
    match t with
    | Mock { images; _ } ->
      List.find !images ~f:(fun image -> image.id = id) |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.Image.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int id ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Image.table} WHERE id = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          [%message "Expected at most one image for id" (id : int)])
        |> Option.map ~f:Database_schema.Image.of_row)
  ;;
end

module Tag = struct
  let find_by_name t ~name =
    match t with
    | Mock { tags; _ } ->
      List.find !tags ~f:(fun tag -> String.Caseless.equal tag.name name)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.Tag.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string name ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Tag.table} WHERE lower(name) = \
               lower($1)"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          [%message "Expected at most one tag for name" (name : string)])
        |> Option.map ~f:Database_schema.Tag.of_row)
  ;;

  let find_or_create t ~name =
    match t with
    | Mock { tags; _ } ->
      (match List.find !tags ~f:(fun tag -> String.Caseless.equal tag.name name) with
       | Some tag -> Deferred.Or_error.return tag
       | None ->
         let tag : Database_schema.Tag.t =
           { id = next_id !tags ~id:(fun tag -> tag.id); name }
         in
         tags := tag :: !tags;
         Deferred.Or_error.return tag)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string name ]
            [%string
              {sql|
            INSERT INTO %{Database_schema.Tag.table} (name)
            VALUES ($1)
            ON CONFLICT ((lower(name))) DO UPDATE SET name = %{Database_schema.Tag.table}.name
            RETURNING id, name
            |sql}]
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
    | Mock { tags; post_tags; _ } ->
      !tags
      |> List.filter_map ~f:(fun tag ->
        let count = List.count !post_tags ~f:(fun post_tag -> tag.id = post_tag.tag_id) in
        match Int.sign count with
        | Pos -> Some (tag, count)
        | Zero | Neg -> None)
      |> List.sort ~compare:(fun (_, count1) (_, count2) -> Int.compare count2 count1)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            [%string
              {sql|
            SELECT tag.id, tag.name, COUNT(*)
            FROM %{Database_schema.Tag.table} tag
            JOIN %{Database_schema.Post_tag.table} ON tag.id = %{Database_schema.Post_tag.table}.tag_id
            GROUP BY tag.id
            ORDER BY COUNT(*) DESC
            |sql}]
        in
        List.map rows ~f:(fun row ->
          match row with
          | [ id; name; count ] ->
            Database_schema.Tag.of_row [ id; name ], Value.to_int_exn count
          | _ ->
            raise_s [%message "Unexpected row shape for Tag count" (row : Value.t list)]))
  ;;
end

module Post_tag = struct
  let tags_for_post t ~post_id =
    match t with
    | Mock { tags; post_tags; _ } ->
      let tag_ids =
        List.filter_map !post_tags ~f:(fun post_tag ->
          match post_tag.post_id = post_id with
          | true -> Some post_tag.tag_id
          | false -> None)
        |> Int.Set.of_list
      in
      List.filter !tags ~f:(fun tag -> Set.mem tag_ids tag.id)
      |> List.sort ~compare:(fun a b -> Int.compare a.id b.id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.Tag.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int post_id ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Tag.table} JOIN \
               %{Database_schema.Post_tag.table} ON %{Database_schema.Tag.table}.id = \
               %{Database_schema.Post_tag.table}.tag_id WHERE \
               %{Database_schema.Post_tag.table}.post_id = $1 ORDER BY \
               %{Database_schema.Tag.table}.id"]
        in
        List.map rows ~f:Database_schema.Tag.of_row)
  ;;

  let set_tags t ~post_id ~tag_ids =
    let tag_ids = List.dedup_and_sort tag_ids ~compare:Int.compare in
    match t with
    | Mock { post_tags; _ } ->
      let kept =
        List.filter !post_tags ~f:(fun post_tag -> post_tag.post_id <> post_id)
      in
      let added =
        List.map tag_ids ~f:(fun tag_id ->
          ({ post_id; tag_id } : Database_schema.Post_tag.t))
      in
      post_tags := added @ kept;
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.with_transaction connection (fun connection ->
          let%bind () =
            Pgx_async.execute_unit
              connection
              ~params:[ Value.of_int post_id ]
              [%string "DELETE FROM %{Database_schema.Post_tag.table} WHERE post_id = $1"]
          in
          Deferred.List.iter tag_ids ~how:`Sequential ~f:(fun tag_id ->
            Pgx_async.execute_unit
              connection
              ~params:[ Value.of_int post_id; Value.of_int tag_id ]
              [%string
                "INSERT INTO %{Database_schema.Post_tag.table} (post_id, tag_id) VALUES \
                 ($1, $2)"])))
  ;;
end

module Publication = struct
  let create t ~title ~image_id ~authors ~journal ~link =
    match t with
    | Mock { publication; _ } ->
      let publication_row : Database_schema.Publication.t =
        { id = next_id !publication ~id:(fun publication -> publication.id)
        ; title
        ; image_id
        ; authors
        ; journal
        ; link
        ; hidden = false
        }
      in
      publication := publication_row :: !publication;
      Deferred.Or_error.return publication_row
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
            [%string
              {sql|
            INSERT INTO %{Database_schema.Publication.table} (title, image_id, authors, journal, link, hidden)
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING id
            |sql}]
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
            [%message
              "Unexpected result from Publication insert" (rows : Value.t list list)])
  ;;

  let list t ?(include_hidden = false) () =
    match t with
    | Mock { publication; _ } ->
      List.filter !publication ~f:(fun publication ->
        (not publication.hidden) || include_hidden)
      |> List.sort ~compare:(fun a b -> Int.compare a.id b.id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let where =
          match include_hidden with
          | true -> ""
          | false -> "WHERE NOT hidden"
        in
        let columns = String.concat ~sep:", " Database_schema.Publication.columns in
        let%map rows =
          Pgx_async.execute
            connection
            [%string
              "SELECT %{columns} FROM %{Database_schema.Publication.table} %{where} \
               ORDER BY id"]
        in
        List.map rows ~f:Database_schema.Publication.of_row)
  ;;
end

module News = struct
  let create t ~content ~date =
    match t with
    | Mock { news; last_news_id; _ } ->
      incr last_news_id;
      let news_row : Database_schema.News.t = { id = !last_news_id; content; date } in
      news := news_row :: !news;
      Deferred.Or_error.return news_row
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map result =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string content; Value.of_date date ]
            [%string
              {sql|
            INSERT INTO %{Database_schema.News.table} (content, date)
            VALUES ($1, $2)
            RETURNING id
            |sql}]
        in
        match result with
        | [ [ id_value ] ] ->
          ({ id = Value.to_int_exn id_value; content; date } : Database_schema.News.t)
        | rows ->
          raise_s
            [%message "Unexpected result from News insert" (rows : Value.t list list)])
  ;;

  let list t =
    match t with
    | Mock { news; _ } ->
      List.sort !news ~compare:(fun a b -> Date.compare b.date a.date)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let columns = String.concat ~sep:", " Database_schema.News.columns in
        let%map rows =
          Pgx_async.execute
            connection
            [%string
              "SELECT %{columns} FROM %{Database_schema.News.table} ORDER BY date DESC "]
        in
        List.map rows ~f:Database_schema.News.of_row)
  ;;

  let delete t ~id =
    match t with
    | Mock { news; _ } ->
      (match List.exists !news ~f:(fun news -> news.id = id) with
       | false -> Deferred.Or_error.error_s [%message "No such news" (id : int)]
       | true ->
         news := List.filter !news ~f:(fun news -> news.id <> id);
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_int id ]
            [%string
              "DELETE FROM %{Database_schema.News.table} WHERE id = $1 RETURNING id"]
        in
        match rows with
        | [] -> raise_s [%message "No such news" (id : int)]
        | _ :: _ -> ())
  ;;
end

module Session = struct
  let create t ~token_hash ~user_id ~expires_at =
    let session : Database_schema.Session.t = { token_hash; user_id; expires_at } in
    match t with
    | Mock { sessions; _ } ->
      (match
         is_unique !sessions ~field:(fun session -> session.token_hash) ~value:token_hash
       with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message
                "duplicate key value violates unique constraint" (token_hash : string)])
       | true ->
         sessions := session :: !sessions;
         Deferred.Or_error.return session)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let%map () =
          Pgx_async.execute_unit
            connection
            ~params:
              [ Value.of_string token_hash
              ; Value.of_int user_id
              ; Database_schema.value_of_time_ns expires_at
              ]
            [%string
              {sql|
            INSERT INTO %{Database_schema.Session.table} (token_hash, user_id, expires_at)
            VALUES ($1, $2, $3)
            |sql}]
        in
        session)
  ;;

  let find_by_token_hash t ~token_hash =
    match t with
    | Mock { sessions; _ } ->
      List.find !sessions ~f:(fun session -> String.equal session.token_hash token_hash)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        let columns = String.concat ~sep:", " Database_schema.Session.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Value.of_string token_hash ]
            [%string
              "SELECT %{columns} FROM %{Database_schema.Session.table} WHERE token_hash \
               = $1"]
        in
        Utils.expect_at_most_one rows ~error_message:(fun _ ->
          [%message "Expected at most one session for token hash"])
        |> Option.map ~f:Database_schema.Session.of_row)
  ;;

  let delete t ~token_hash =
    match t with
    | Mock { sessions; _ } ->
      sessions
      := List.filter !sessions ~f:(fun session ->
           not (String.equal session.token_hash token_hash));
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let module Value = Pgx_async.Value in
        Pgx_async.execute_unit
          connection
          ~params:[ Value.of_string token_hash ]
          [%string "DELETE FROM %{Database_schema.Session.table} WHERE token_hash = $1"])
  ;;

  let delete_expired t ~now =
    match t with
    | Mock { sessions; _ } ->
      sessions
      := List.filter !sessions ~f:(fun session -> Time_ns.( > ) session.expires_at now);
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        Pgx_async.execute_unit
          connection
          ~params:[ Database_schema.value_of_time_ns now ]
          [%string "DELETE FROM %{Database_schema.Session.table} WHERE expires_at <= $1"])
  ;;
end

module For_testing = struct
  let create_in_memory
        ?(posts = [])
        ?(users = [])
        ?(images = [])
        ?(tags = [])
        ?(post_tags = [])
        ?(publication = [])
        ?(news = [])
        ?(sessions = [])
        ()
    =
    Mock
      { posts = ref posts
      ; users = ref users
      ; images = ref images
      ; tags = ref tags
      ; post_tags = ref post_tags
      ; publication = ref publication
      ; news = ref news
      ; last_news_id =
          ref (List.fold news ~init:0 ~f:(fun highest news -> Int.max highest news.id))
      ; sessions = ref sessions
      }
  ;;

  let with_test_connection ~f =
    Pgx_async.with_conn ~database:"blogdb_test" (fun connection ->
      (* Reset the database every time. *)
      let%bind () =
        Pgx_async.execute_unit connection "DROP SCHEMA IF EXISTS public CASCADE"
      in
      let%bind () = Pgx_async.execute_unit connection "CREATE SCHEMA public" in
      let t = Real { connection } in
      f t)
  ;;
end
