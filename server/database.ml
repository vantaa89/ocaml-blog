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
      ; last_id : int ref
      ; sessions : Database_schema.Session.t list ref
      ; api_tokens : Database_schema.Api_token.t list ref
      }

let is_unique rows ~field ~value =
  not (List.exists rows ~f:(fun row -> String.equal (field row) value))
;;

let fresh_id last_id =
  incr last_id;
  !last_id
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

let insert_exn
      (type id row)
      (module Table : Database_schema.Table with type t = row)
      (module Id : Id with type t = id)
      connection
      ~(row : id:id -> row)
  =
  Deferred.Or_error.try_with (fun () ->
    let columns, values =
      List.zip_exn Table.columns (Table.to_row (row ~id:(Id.of_int_exn 1)))
      |> List.filter ~f:(fun (column, _) -> not (String.equal column "id"))
      |> List.unzip
    in
    let num_params = List.length values in
    let placeholders =
      List.init ~f:Fn.id num_params
      |> List.map ~f:(fun i -> "$" ^ Int.to_string (i + 1))
      |> String.concat ~sep:", "
    in
    let columns = String.concat ~sep:", " columns in
    let%map rows =
      Pgx_async.execute
        connection
        ~params:values
        [%string
          "INSERT INTO %{Table.table} (%{columns}) VALUES (%{placeholders}) RETURNING id"]
    in
    match rows with
    | [ [ id ] ] -> row ~id:(Id.of_pgx_async_value_exn id)
    | rows ->
      raise_s
        [%message
          "Unexpected result from insert"
            ~table:(Table.table : string)
            (rows : Pgx_async.Value.t list list)])
;;

let find_unique
      (type row)
      (module Table : Database_schema.Table with type t = row)
      connection
      ~where
      ~param
  =
  Deferred.Or_error.try_with (fun () ->
    let columns = String.concat ~sep:", " Table.columns in
    let%map rows =
      Pgx_async.execute
        connection
        ~params:[ param ]
        [%string "SELECT %{columns} FROM %{Table.table} WHERE %{where}"]
    in
    Utils.expect_at_most_one rows ~error_message:(fun rows ->
      [%message
        "Expected at most one row"
          ~table:(Table.table : string)
          (where : string)
          ~count:(List.length rows : int)])
    |> Option.map ~f:Table.of_row)
;;

module Post = struct
  let find_by_slug t ~slug =
    match t with
    | Mock { posts; _ } ->
      List.find !posts ~f:(fun post -> String.equal post.slug slug)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.Post)
        connection
        ~where:"slug = $1"
        ~param:(Pgx_async.Value.of_string slug)
  ;;

  let find_by_id t ~id =
    match t with
    | Mock { posts; _ } ->
      List.find !posts ~f:(fun post -> Post_id.equal post.id id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.Post)
        connection
        ~where:"id = $1"
        ~param:(Post_id.to_pgx_async_value id)
  ;;

  let visible_to ~viewer (post : Database_schema.Post.t) =
    (not post.hidden) || [%equal: User_id.t option] viewer (Some post.author_id)
  ;;

  let list t ?tag ~viewer ?limit ?(offset = 0) () =
    match t with
    | Mock { posts; tags; post_tags; _ } ->
      let tagged_posts =
        match tag with
        | None -> !posts
        | Some tag ->
          List.filter !posts ~f:(fun post ->
            List.exists !post_tags ~f:(fun post_tag ->
              Post_id.equal post_tag.post_id post.id
              && List.exists !tags ~f:(fun candidate ->
                Tag_id.equal candidate.id post_tag.tag_id
                && String.Caseless.equal candidate.name tag)))
      in
      let posts =
        List.filter tagged_posts ~f:(fun post ->
          (not post.special_post) && visible_to ~viewer post)
        |> List.sort ~compare:(fun a b -> Time_ns.compare b.created_at a.created_at)
        |> Fn.flip List.drop offset
      in
      Deferred.Or_error.return
        (match limit with
         | None -> posts
         | Some limit -> List.take posts limit)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let tag_condition =
          match tag with
          | None -> ""
          | Some _ ->
            [%string
              "AND id IN (SELECT pt.post_id FROM %{Database_schema.Post_tag.table} pt \
               JOIN %{Database_schema.Tag.table} tag ON pt.tag_id = tag.id WHERE \
               lower(tag.name) = lower($2))"]
        in
        let limit_clause =
          match limit with
          | None -> ""
          | Some limit -> [%string "LIMIT %{limit#Int}"]
        in
        let columns = String.concat ~sep:", " Database_schema.Post.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:
              (Pgx_async.Value.opt User_id.to_pgx_async_value viewer
               :: Option.to_list (Option.map tag ~f:Pgx_async.Value.of_string))
            [%string
              "SELECT %{columns} FROM %{Database_schema.Post.table} WHERE NOT \
               special_post AND (NOT hidden OR author_id = $1) %{tag_condition} ORDER BY \
               created_at DESC %{limit_clause} OFFSET %{offset#Int}"]
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
      |> Fn.flip List.take limit
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let pattern = [%string "%%%{query}%%"] in
        let columns = String.concat ~sep:", " Database_schema.Post.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Pgx_async.Value.of_string pattern; Pgx_async.Value.of_int limit ]
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
           match Post_id.equal post.id id with
           | true -> { post with hidden }
           | false -> post);
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        Pgx_async.execute_unit
          connection
          ~params:[ Pgx_async.Value.of_bool hidden; Post_id.to_pgx_async_value id ]
          [%string "UPDATE %{Database_schema.Post.table} SET hidden = $1 WHERE id = $2"])
  ;;

  let create t ~title ~slug ~content_en ~content_ko ~author_id ~special_post ~now =
    let row ~id : Database_schema.Post.t =
      { id
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
    match t with
    | Mock { posts; last_id; _ } ->
      (match is_unique !posts ~field:(fun post -> post.slug) ~value:slug with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message "duplicate key value violates unique constraint" (slug : string)])
       | true ->
         let post = row ~id:(Post_id.of_int_exn (fresh_id last_id)) in
         posts := post :: !posts;
         Deferred.Or_error.return post)
    | Real { connection } ->
      insert_exn (module Database_schema.Post) (module Post_id) connection ~row
  ;;

  let update t ~id ~title ~slug ~content_en ~content_ko ~special_post =
    match t with
    | Mock { posts; _ } ->
      (* The post keeping its own slug is not a duplicate, so it is left out of the
         check. *)
      let others = List.filter !posts ~f:(fun post -> not (Post_id.equal post.id id)) in
      (match is_unique others ~field:(fun post -> post.slug) ~value:slug with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message "duplicate key value violates unique constraint" (slug : string)])
       | true ->
         posts
         := List.map !posts ~f:(fun post ->
              match Post_id.equal post.id id with
              | true -> { post with title; slug; content_en; content_ko; special_post }
              | false -> post);
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let params =
          [ Pgx_async.Value.of_string title
          ; Pgx_async.Value.of_string slug
          ; Pgx_async.Value.opt Pgx_async.Value.of_string content_en
          ; Pgx_async.Value.opt Pgx_async.Value.of_string content_ko
          ; Pgx_async.Value.of_bool special_post
          ; Post_id.to_pgx_async_value id
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
    let row ~id : Database_schema.User.t =
      { id; username; email; password_hash; date_joined; last_login = None }
    in
    match t with
    | Mock { users; last_id; _ } ->
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
         let user = row ~id:(User_id.of_int_exn (fresh_id last_id)) in
         users := user :: !users;
         Deferred.Or_error.return user)
    | Real { connection } ->
      insert_exn (module Database_schema.User) (module User_id) connection ~row
  ;;

  let find_by_id t ~id =
    match t with
    | Mock { users; _ } ->
      List.find !users ~f:(fun user -> User_id.equal user.id id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.User)
        connection
        ~where:"id = $1"
        ~param:(User_id.to_pgx_async_value id)
  ;;

  let list t =
    match t with
    | Mock { users; _ } ->
      List.sort !users ~compare:(fun a b -> User_id.compare a.id b.id)
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
        Pgx_async.execute_unit
          connection
          ~params:
            [ Pgx_async.Value.of_string password_hash
            ; Pgx_async.Value.of_string username
            ]
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
        Pgx_async.execute_unit
          connection
          ~params:
            [ Database_schema.value_of_time_ns last_login
            ; Pgx_async.Value.of_string username
            ]
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
        Pgx_async.execute_unit
          connection
          ~params:[ Pgx_async.Value.of_string username ]
          [%string "DELETE FROM %{Database_schema.User.table} WHERE username = $1"])
  ;;

  let find_by_username t ~username =
    match t with
    | Mock { users; _ } ->
      List.find !users ~f:(fun user -> String.equal user.username username)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.User)
        connection
        ~where:"username = $1"
        ~param:(Pgx_async.Value.of_string username)
  ;;
end

module Image = struct
  let create t ~filename ~date =
    let row ~id : Database_schema.Image.t = { id; filename; date } in
    match t with
    | Mock { images; last_id; _ } ->
      let image = row ~id:(Image_id.of_int_exn (fresh_id last_id)) in
      images := image :: !images;
      Deferred.Or_error.return image
    | Real { connection } ->
      insert_exn (module Database_schema.Image) (module Image_id) connection ~row
  ;;

  let find_by_id t ~id =
    match t with
    | Mock { images; _ } ->
      List.find !images ~f:(fun image -> Image_id.equal image.id id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.Image)
        connection
        ~where:"id = $1"
        ~param:(Image_id.to_pgx_async_value id)
  ;;

  let list_with_reference_counts t =
    match t with
    | Mock { images; posts; publication; _ } ->
      List.sort !images ~compare:(fun a b -> Image_id.compare a.id b.id)
      |> List.map ~f:(fun image ->
        let url = Database_schema.Image.url image in
        let posts =
          List.count !posts ~f:(fun post ->
            Database_schema.Post.content_by_language post
            |> Map.exists ~f:(String.is_substring ~substring:url))
        in
        let publications =
          List.count !publication ~f:(fun publication ->
            Image_id.equal publication.image_id image.id)
        in
        image, posts + publications)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let columns = String.concat ~sep:", " Database_schema.Image.columns in
        let%bind rows =
          Pgx_async.execute
            connection
            [%string "SELECT %{columns} FROM %{Database_schema.Image.table} ORDER BY id"]
        in
        let count ~param sql =
          let%map rows = Pgx_async.execute connection ~params:[ param ] sql in
          match rows with
          | [ [ count ] ] -> Pgx_async.Value.to_int_exn count
          | _ ->
            raise_s
              [%message
                "Unexpected result from count" (rows : Pgx_async.Value.t list list)]
        in
        Deferred.List.map
          (List.map rows ~f:Database_schema.Image.of_row)
          ~how:`Sequential
          ~f:(fun image ->
            let%bind posts =
              count
                ~param:(Pgx_async.Value.of_string (Database_schema.Image.url image))
                [%string
                  {sql|
                SELECT COUNT(*) FROM %{Database_schema.Post.table}
                WHERE strpos(content_en, $1) > 0 OR strpos(content_ko, $1) > 0
                |sql}]
            in
            let%map publications =
              count
                ~param:(Image_id.to_pgx_async_value image.id)
                [%string
                  {sql|
                SELECT COUNT(*) FROM %{Database_schema.Publication.table}
                WHERE image_id = $1
                |sql}]
            in
            image, posts + publications))
  ;;
end

module Tag = struct
  let find_by_name t ~name =
    match t with
    | Mock { tags; _ } ->
      List.find !tags ~f:(fun tag -> String.Caseless.equal tag.name name)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.Tag)
        connection
        ~where:"lower(name) = lower($1)"
        ~param:(Pgx_async.Value.of_string name)
  ;;

  let find_or_create t ~name =
    match t with
    | Mock { tags; last_id; _ } ->
      (match List.find !tags ~f:(fun tag -> String.Caseless.equal tag.name name) with
       | Some tag -> Deferred.Or_error.return tag
       | None ->
         let tag : Database_schema.Tag.t =
           { id = Tag_id.of_int_exn (fresh_id last_id); name }
         in
         tags := tag :: !tags;
         Deferred.Or_error.return tag)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Pgx_async.Value.of_string name ]
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
            [%message
              "Unexpected result from Tag upsert" (rows : Pgx_async.Value.t list list)])
  ;;

  (** Tags that are attached to at least one post, most-used first. *)
  let list_with_post_counts t =
    match t with
    | Mock { tags; post_tags; _ } ->
      !tags
      |> List.filter_map ~f:(fun tag ->
        let count =
          List.count !post_tags ~f:(fun post_tag -> Tag_id.equal tag.id post_tag.tag_id)
        in
        match Int.sign count with
        | Pos -> Some (tag, count)
        | Zero | Neg -> None)
      |> List.sort ~compare:(fun (_, count1) (_, count2) -> Int.compare count2 count1)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
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
            Database_schema.Tag.of_row [ id; name ], Pgx_async.Value.to_int_exn count
          | _ ->
            raise_s
              [%message
                "Unexpected row shape for Tag count" (row : Pgx_async.Value.t list)]))
  ;;
end

module Post_tag = struct
  let tags_for_post t ~post_id =
    match t with
    | Mock { tags; post_tags; _ } ->
      List.filter !tags ~f:(fun tag ->
        List.exists !post_tags ~f:(fun post_tag ->
          Post_id.equal post_tag.post_id post_id && Tag_id.equal post_tag.tag_id tag.id))
      |> List.sort ~compare:(fun a b -> Tag_id.compare a.id b.id)
      |> Deferred.Or_error.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let columns = String.concat ~sep:", " Database_schema.Tag.columns in
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Post_id.to_pgx_async_value post_id ]
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
    let tag_ids = List.dedup_and_sort tag_ids ~compare:Tag_id.compare in
    match t with
    | Mock { post_tags; _ } ->
      let kept =
        List.filter !post_tags ~f:(fun post_tag ->
          not (Post_id.equal post_tag.post_id post_id))
      in
      let added =
        List.map tag_ids ~f:(fun tag_id ->
          ({ post_id; tag_id } : Database_schema.Post_tag.t))
      in
      post_tags := added @ kept;
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        Pgx_async.with_transaction connection (fun connection ->
          let%bind () =
            Pgx_async.execute_unit
              connection
              ~params:[ Post_id.to_pgx_async_value post_id ]
              [%string "DELETE FROM %{Database_schema.Post_tag.table} WHERE post_id = $1"]
          in
          Deferred.List.iter tag_ids ~how:`Sequential ~f:(fun tag_id ->
            Pgx_async.execute_unit
              connection
              ~params:
                [ Post_id.to_pgx_async_value post_id; Tag_id.to_pgx_async_value tag_id ]
              [%string
                "INSERT INTO %{Database_schema.Post_tag.table} (post_id, tag_id) VALUES \
                 ($1, $2)"])))
  ;;
end

module Publication = struct
  let create t ~title ~image_id ~authors ~journal ~link =
    let row ~id : Database_schema.Publication.t =
      { id; title; image_id; authors; journal; link; hidden = false }
    in
    match t with
    | Mock { publication; last_id; _ } ->
      let publication_row = row ~id:(Publication_id.of_int_exn (fresh_id last_id)) in
      publication := publication_row :: !publication;
      Deferred.Or_error.return publication_row
    | Real { connection } ->
      insert_exn
        (module Database_schema.Publication)
        (module Publication_id)
        connection
        ~row
  ;;

  let list t ?(include_hidden = false) () =
    match t with
    | Mock { publication; _ } ->
      List.filter !publication ~f:(fun publication ->
        (not publication.hidden) || include_hidden)
      |> List.sort ~compare:(fun a b -> Publication_id.compare a.id b.id)
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

  let set_hidden t ~id ~hidden =
    match t with
    | Mock { publication; _ } ->
      (match
         List.exists !publication ~f:(fun publication ->
           Publication_id.equal publication.id id)
       with
       | false ->
         Deferred.Or_error.error_s
           [%message "No such publication" (id : Publication_id.t)]
       | true ->
         publication
         := List.map !publication ~f:(fun publication ->
              match Publication_id.equal publication.id id with
              | true -> { publication with hidden }
              | false -> publication);
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map rows =
          Pgx_async.execute
            connection
            ~params:
              [ Pgx_async.Value.of_bool hidden; Publication_id.to_pgx_async_value id ]
            [%string
              "UPDATE %{Database_schema.Publication.table} SET hidden = $1 WHERE id = $2 \
               RETURNING id"]
        in
        match rows with
        | [] -> raise_s [%message "No such publication" (id : Publication_id.t)]
        | _ :: _ -> ())
  ;;

  let delete t ~id =
    match t with
    | Mock { publication; _ } ->
      (match
         List.partition_tf !publication ~f:(fun publication ->
           Publication_id.equal publication.id id)
       with
       | [], _ ->
         Deferred.Or_error.error_s
           [%message "No such publication" (id : Publication_id.t)]
       | _ :: _, rest ->
         publication := rest;
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Publication_id.to_pgx_async_value id ]
            [%string
              "DELETE FROM %{Database_schema.Publication.table} WHERE id = $1 RETURNING \
               id"]
        in
        match rows with
        | [] -> raise_s [%message "No such publication" (id : Publication_id.t)]
        | _ :: _ -> ())
  ;;
end

module News = struct
  let create t ~content ~date =
    let row ~id : Database_schema.News.t = { id; content; date } in
    match t with
    | Mock { news; last_id; _ } ->
      let news_row = row ~id:(News_id.of_int_exn (fresh_id last_id)) in
      news := news_row :: !news;
      Deferred.Or_error.return news_row
    | Real { connection } ->
      insert_exn (module Database_schema.News) (module News_id) connection ~row
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
      (match List.partition_tf !news ~f:(fun news -> News_id.equal news.id id) with
       | [], _ -> Deferred.Or_error.error_s [%message "No such news" (id : News_id.t)]
       | _ :: _, rest ->
         news := rest;
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ News_id.to_pgx_async_value id ]
            [%string
              "DELETE FROM %{Database_schema.News.table} WHERE id = $1 RETURNING id"]
        in
        match rows with
        | [] -> raise_s [%message "No such news" (id : News_id.t)]
        | _ :: _ -> ())
  ;;
end

module Session = struct
  let create t ~token_hash ~user_id ~expires_at =
    let session : Database_schema.Session.t = { token_hash; user_id; expires_at } in
    match t with
    | Mock { sessions; _ } ->
      (match
         is_unique
           !sessions
           ~field:(fun session -> Token.Hash.to_string session.token_hash)
           ~value:(Token.Hash.to_string token_hash)
       with
       | false ->
         Deferred.return
           (Or_error.error_s
              [%message
                "duplicate key value violates unique constraint"
                  (token_hash : Token.Hash.t)])
       | true ->
         sessions := session :: !sessions;
         Deferred.Or_error.return session)
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map () =
          Pgx_async.execute_unit
            connection
            ~params:
              [ Pgx_async.Value.of_string (Token.Hash.to_string token_hash)
              ; User_id.to_pgx_async_value user_id
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
      List.find !sessions ~f:(fun session ->
        Token.Hash.equal session.token_hash token_hash)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.Session)
        connection
        ~where:"token_hash = $1"
        ~param:(Pgx_async.Value.of_string (Token.Hash.to_string token_hash))
  ;;

  let delete t ~token_hash =
    match t with
    | Mock { sessions; _ } ->
      sessions
      := List.filter !sessions ~f:(fun session ->
           not (Token.Hash.equal session.token_hash token_hash));
      Deferred.Or_error.return ()
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        Pgx_async.execute_unit
          connection
          ~params:[ Pgx_async.Value.of_string (Token.Hash.to_string token_hash) ]
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

module Api_token = struct
  let create t ~label ~token_hash ~user_id ~created_at =
    let api_token : Database_schema.Api_token.t =
      { label; token_hash; user_id; created_at }
    in
    match t with
    | Mock { api_tokens; _ } ->
      (match
         ( is_unique !api_tokens ~field:(fun api_token -> api_token.label) ~value:label
         , is_unique
             !api_tokens
             ~field:(fun api_token -> Token.Hash.to_string api_token.token_hash)
             ~value:(Token.Hash.to_string token_hash) )
       with
       | true, true ->
         api_tokens := api_token :: !api_tokens;
         Deferred.Or_error.return api_token
       | false, _ | _, false ->
         Deferred.Or_error.error_s
           [%message "duplicate key value violates unique constraint" (label : string)])
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map () =
          Pgx_async.execute_unit
            connection
            ~params:(Database_schema.Api_token.to_row api_token)
            [%string
              {sql|
            INSERT INTO %{Database_schema.Api_token.table} (label, token_hash, user_id, created_at)
            VALUES ($1, $2, $3, $4)
            |sql}]
        in
        api_token)
  ;;

  let find_by_token_hash t ~token_hash =
    match t with
    | Mock { api_tokens; _ } ->
      List.find !api_tokens ~f:(fun api_token ->
        Token.Hash.equal api_token.token_hash token_hash)
      |> Deferred.Or_error.return
    | Real { connection } ->
      find_unique
        (module Database_schema.Api_token)
        connection
        ~where:"token_hash = $1"
        ~param:(Pgx_async.Value.of_string (Token.Hash.to_string token_hash))
  ;;

  let list t =
    match t with
    | Mock { api_tokens; _ } ->
      String.Map.of_list_with_key_or_error !api_tokens ~get_key:(fun api_token ->
        api_token.label)
      |> Deferred.return
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let columns = String.concat ~sep:", " Database_schema.Api_token.columns in
        let%map rows =
          Pgx_async.execute
            connection
            [%string "SELECT %{columns} FROM %{Database_schema.Api_token.table}"]
        in
        List.map rows ~f:Database_schema.Api_token.of_row
        |> String.Map.of_list_with_key_exn ~get_key:(fun api_token -> api_token.label))
  ;;

  let delete t ~label =
    match t with
    | Mock { api_tokens; _ } ->
      (match
         List.partition_tf !api_tokens ~f:(fun api_token ->
           String.equal api_token.label label)
       with
       | [], _ ->
         Deferred.Or_error.error_s [%message "No such API token" (label : string)]
       | _ :: _, rest ->
         api_tokens := rest;
         Deferred.Or_error.return ())
    | Real { connection } ->
      Deferred.Or_error.try_with (fun () ->
        let%map rows =
          Pgx_async.execute
            connection
            ~params:[ Pgx_async.Value.of_string label ]
            [%string
              "DELETE FROM %{Database_schema.Api_token.table} WHERE label = $1 RETURNING \
               label"]
        in
        match rows with
        | [] -> raise_s [%message "No such API token" (label : string)]
        | _ :: _ -> ())
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
        ?(api_tokens = [])
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
      ; last_id =
          List.concat
            [ List.map posts ~f:(fun post -> Post_id.to_int post.id)
            ; List.map users ~f:(fun user -> User_id.to_int user.id)
            ; List.map images ~f:(fun image -> Image_id.to_int image.id)
            ; List.map tags ~f:(fun tag -> Tag_id.to_int tag.id)
            ; List.map publication ~f:(fun publication ->
                Publication_id.to_int publication.id)
            ; List.map news ~f:(fun news -> News_id.to_int news.id)
            ]
          |> List.fold ~init:0 ~f:Int.max
          |> ref
      ; sessions = ref sessions
      ; api_tokens = ref api_tokens
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
