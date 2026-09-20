open! Core
open! Async
open! Import

(** Quickcheck to ensure the equivalence between the postgres and the in-memory mock implementation *)

module Test_string = struct
  type t =
    (string
    [@quickcheck.generator
      Base_quickcheck.Generator.string_of Base_quickcheck.Generator.char_alphanum])
  (* use only alphanumerics *)
  [@@deriving equal, sexp_of, quickcheck]
end

module Test_slug = struct
  type t =
    | A
    | B
    | C
  [@@deriving equal, sexp_of, quickcheck]

  let to_string : t -> string = function
    | A -> "slug-a"
    | B -> "slug-b"
    | C -> "slug-c"
  ;;
end

module Test_token = struct
  type t =
    | A
    | B
    | C
  [@@deriving equal, sexp_of, quickcheck]

  let to_string : t -> string = function
    | A -> "token-a"
    | B -> "token-b"
    | C -> "token-c"
  ;;
end

(* Session expiry is expressed relative to a [now] captured once per program, so that both
   sides agree on the value even though each side runs the op at a slightly different
   time. A whole day of slack keeps [delete_expired]'s own [Time_ns.now ()] on the
   intended side of the boundary. *)
module Test_expiry = struct
  type t =
    | Expired
    | Valid
  [@@deriving equal, sexp_of, quickcheck]

  let to_time_ns t ~now : Time_ns.t =
    let one_day = Time_ns.Span.of_day 1. in
    match t with
    | Expired -> Time_ns.sub now one_day
    | Valid -> Time_ns.add now one_day
  ;;
end

(* A reference to an entity created earlier in the same program, as an index into the
   creation order. This is needed both to ensure that a generated index always lands on a
   row that is there, and to compare real/mock ids: real ids can drift apart (e.g. a
   failed insert still consumes a Postgres sequence value) even when both sides behave
   identically, so ids are normalized to creation-order indices before comparison. *)
module Ref = struct
  type t =
    (int[@quickcheck.generator Base_quickcheck.Generator.small_positive_or_zero_int])
  [@@deriving equal, sexp_of, quickcheck]
end

module Op = struct
  type t =
    | Create_post of
        { slug : Test_slug.t
        ; content_en : Test_string.t option
        ; content_ko : Test_string.t option
        ; special_post : bool
        }
    | Find_post_by_slug of Test_slug.t
    | Find_post_by_id of Ref.t
    | Set_hidden of
        { post : Ref.t
        ; hidden : bool
        }
    | List_posts of { as_author : bool }
    | Search_posts of Test_string.t
    | Find_or_create_tag of Test_slug.t
    | Find_tag_by_slug of Test_slug.t
    | Set_tags of
        { post : Ref.t
        ; tags : Ref.t list
        }
    | Tags_for_post of Ref.t
    | List_tags_with_post_counts
    | List_posts_by_tag_slug of
        { slug : Test_slug.t
        ; as_author : bool
        }
    | Create_news of Test_string.t
    | List_news
    | Create_publication of
        { authors : Test_string.t
        ; journal : Test_string.t
        ; link : Test_string.t option
        }
    | List_publications of { include_hidden : bool }
    | Create_session of
        { token : Test_token.t
        ; expiry : Test_expiry.t
        }
    | Find_session of Test_token.t
    | Delete_session of Test_token.t
    | Delete_expired_sessions
  [@@deriving sexp_of, quickcheck]
end

(* The normalized versions of each schema with symbolic ids and no [created_at]. *)
module Observed = struct
  module Post = struct
    type t =
      { id : Ref.t
      ; title : string
      ; slug : string
      ; content_en : string option
      ; content_ko : string option
      ; special_post : bool
      ; hidden : bool
      }
    [@@deriving equal, sexp_of]
  end

  module Tag = struct
    type t =
      { id : Ref.t
      ; name : string
      ; slug : string
      }
    [@@deriving equal, sexp_of]
  end

  module News = struct
    type t =
      { content : string
      ; date : Date.t
      }
    [@@deriving equal, sexp_of]
  end

  module Publication = struct
    type t =
      { title : string
      ; authors : string
      ; journal : string
      ; link : string option
      ; hidden : bool
      }
    [@@deriving equal, sexp_of]
  end

  module Session = struct
    type t =
      { token_hash : string
      ; user : Ref.t
      ; expires_at : Time_ns.Alternate_sexp.t
      }
    [@@deriving equal, sexp_of]
  end
end

module Response = struct
  type t =
    | Unit
    | Post of Observed.Post.t option
    | Posts of Observed.Post.t list
    | Tag of Observed.Tag.t option
    | Tags of Observed.Tag.t list
    | Tag_counts of (Observed.Tag.t * int) list
    | News of Observed.News.t list
    | Publications of Observed.Publication.t list
    | Session of Observed.Session.t option
    (* The op referred to an object that does not exist. *)
    | Skipped
    (* Error messages differ between a Postgres error and a [Mock] one, so we only compare
       that both sides failed. *)
    | Failed
  [@@deriving equal, sexp_of]
end

(* Each side (real db and in-memory mock) of the comparison. Keeps track of the
   [Database.t] object and the indices of the created objects to support [Ref.t]'s modulo
   behavior *)
module Side = struct
  type t =
    { db : Database.t
    ; posts : int Queue.t
    ; tags : int Queue.t
    ; users : int Queue.t
    ; author_id : int
    ; image_id : int
    ; now : Time_ns.t
    ; news_count : int ref
      (* Postgres does not guarantee the order between the news of the same date for
         [News.list] queries. Therefore, we make every news item get a distinct date,
         derived from how many exist so that both sides agree. *)
    }

  let ref_of_id queue id : Ref.t =
    match Queue.findi queue ~f:(fun _ id' -> id' = id) with
    | Some (index, _) -> index
    | None -> raise_s [%message "id not found in queue" (id : int) (queue : int Queue.t)]
  ;;

  let resolve queue index =
    match Queue.length queue with
    | 0 -> None
    | length -> Some (Queue.get queue (index % length))
  ;;

  (* Each side has one author, so a viewer is either them or nobody. *)
  let viewer t ~as_author =
    match as_author with
    | false -> None
    | true -> Some t.author_id
  ;;

  let post_ref t (post : Database_schema.Post.t) : Observed.Post.t =
    { id = ref_of_id t.posts post.id
    ; title = post.title
    ; slug = post.slug
    ; content_en = post.content_en
    ; content_ko = post.content_ko
    ; special_post = post.special_post
    ; hidden = post.hidden
    }
  ;;

  let tag_ref t (tag : Database_schema.Tag.t) : Observed.Tag.t =
    { id = ref_of_id t.tags tag.id; name = tag.name; slug = tag.slug }
  ;;

  let session_ref t (session : Database_schema.Session.t) : Observed.Session.t =
    { token_hash = session.token_hash
    ; user = ref_of_id t.users session.user_id
    ; expires_at = session.expires_at
    }
  ;;

  let create db ~now =
    let username = "author" in
    let%bind author =
      Database.User.create
        db
        ~username
        ~email:[%string "%{username}@example.com"]
        ~password_hash:"hash"
        ~date_joined:(Date.of_string "2024-01-01")
      |> Deferred.Or_error.ok_exn
    in
    let%map image =
      Database.Image.create db ~filename:"image.png" ~date:(Date.of_string "2024-01-01")
      |> Deferred.Or_error.ok_exn
    in
    { db
    ; posts = Queue.create ()
    ; tags = Queue.create ()
    ; users = Queue.of_list [ author.id ]
    ; author_id = author.id
    ; image_id = image.id
    ; now
    ; news_count = ref 0
    }
  ;;
end

let run_op (side : Side.t) (op : Op.t) =
  (* resolve [Ref.t] id into actual [post_id] *)
  let with_post_id ref_id ~f =
    match Side.resolve side.posts ref_id with
    | None -> return (Ok Response.Skipped)
    | Some post_id -> f post_id
  in
  let%map result =
    let open Deferred.Or_error.Let_syntax in
    match op with
    | Create_post { slug; content_en; content_ko; special_post } ->
      let slug = Test_slug.to_string slug in
      let%map post =
        Database.Post.create
          side.db
          ~title:[%string "post %{slug}"]
          ~slug
          ~content_en
          ~content_ko
          ~author_id:side.author_id
          ~special_post
      in
      Queue.enqueue side.posts post.id;
      Response.Post (Some (Side.post_ref side post))
    | Find_post_by_slug slug ->
      let%map post =
        Database.Post.find_by_slug side.db ~slug:(Test_slug.to_string slug)
      in
      Response.Post (Option.map post ~f:(Side.post_ref side))
    | Find_post_by_id index ->
      with_post_id index ~f:(fun id ->
        let%map post = Database.Post.find_by_id side.db ~id in
        Response.Post (Option.map post ~f:(Side.post_ref side)))
    | Set_hidden { post; hidden } ->
      with_post_id post ~f:(fun id ->
        let%map () = Database.Post.set_hidden side.db ~id ~hidden in
        Response.Unit)
    | List_posts { as_author } ->
      let%map posts =
        Database.Post.list side.db ~viewer:(Side.viewer side ~as_author) ()
      in
      Response.Posts (List.map posts ~f:(Side.post_ref side))
    | Search_posts query ->
      let%map posts = Database.Post.search side.db ~query () in
      Response.Posts (List.map posts ~f:(Side.post_ref side))
    | Find_or_create_tag slug ->
      let slug = Test_slug.to_string slug in
      let%map tag =
        Database.Tag.find_or_create side.db ~name:[%string "tag %{slug}"] ~slug
      in
      (* [find_or_create] may have found rather than created. *)
      (match Queue.mem side.tags tag.id ~equal:Int.equal with
       | true -> ()
       | false -> Queue.enqueue side.tags tag.id);
      Response.Tag (Some (Side.tag_ref side tag))
    | Find_tag_by_slug slug ->
      let%map tag = Database.Tag.find_by_slug side.db ~slug:(Test_slug.to_string slug) in
      Response.Tag (Option.map tag ~f:(Side.tag_ref side))
    | Set_tags { post; tags } ->
      with_post_id post ~f:(fun post_id ->
        let tag_ids = List.filter_map tags ~f:(Side.resolve side.tags) in
        let%map () = Database.Post_tag.set_tags side.db ~post_id ~tag_ids in
        Response.Unit)
    | Tags_for_post index ->
      with_post_id index ~f:(fun post_id ->
        let%map tags = Database.Post_tag.tags_for_post side.db ~post_id in
        Response.Tags (List.map tags ~f:(Side.tag_ref side)))
    | List_tags_with_post_counts ->
      let%map counts = Database.Tag.list_with_post_counts side.db in
      Response.Tag_counts
        (List.map counts ~f:(fun (tag, count) -> Side.tag_ref side tag, count))
    | List_posts_by_tag_slug { slug; as_author } ->
      let%map posts =
        Database.Post.list_by_tag_slug
          side.db
          ~slug:(Test_slug.to_string slug)
          ~viewer:(Side.viewer side ~as_author)
          ()
      in
      Response.Posts (List.map posts ~f:(Side.post_ref side))
    | Create_news content ->
      incr side.news_count;
      let date = Date.add_days (Date.of_string "2024-01-01") !(side.news_count) in
      let%map (_ : Database_schema.News.t) =
        Database.News.create side.db ~content ~date
      in
      Response.Unit
    | List_news ->
      let%map news = Database.News.list side.db in
      Response.News
        (List.map news ~f:(fun (news : Database_schema.News.t) : Observed.News.t ->
           { content = news.content; date = news.date }))
    | Create_publication { authors; journal; link } ->
      let%map (_ : Database_schema.Publication.t) =
        Database.Publication.create
          side.db
          ~title:[%string "publication %{authors}"]
          ~image_id:side.image_id
          ~authors
          ~journal
          ~link
      in
      Response.Unit
    | List_publications { include_hidden } ->
      let%map publications = Database.Publication.list side.db ~include_hidden () in
      Response.Publications
        (List.map
           publications
           ~f:
             (fun
               (publication : Database_schema.Publication.t) : Observed.Publication.t ->
             { title = publication.title
             ; authors = publication.authors
             ; journal = publication.journal
             ; link = publication.link
             ; hidden = publication.hidden
             }))
    | Create_session { token; expiry } ->
      let%map session =
        Database.Session.create
          side.db
          ~token_hash:(Test_token.to_string token)
          ~user_id:side.author_id
          ~expires_at:(Test_expiry.to_time_ns expiry ~now:side.now)
      in
      Response.Session (Some (Side.session_ref side session))
    | Find_session token ->
      let%map session =
        Database.Session.find_by_token_hash
          side.db
          ~token_hash:(Test_token.to_string token)
      in
      Response.Session (Option.map session ~f:(Side.session_ref side))
    | Delete_session token ->
      let%map () =
        Database.Session.delete side.db ~token_hash:(Test_token.to_string token)
      in
      Response.Unit
    | Delete_expired_sessions ->
      let%map () = Database.Session.delete_expired side.db in
      Response.Unit
  in
  match result with
  | Ok response -> response
  | Error _ -> Response.Failed
;;

let run_program ~real ~mock ops =
  Deferred.List.iteri ops ~how:`Sequential ~f:(fun step op ->
    let%bind real_response = run_op real op in
    let%map mock_response = run_op mock op in
    match Response.equal real_response mock_response with
    | true -> ()
    | false ->
      raise_s
        [%message
          "Real and Mock disagree"
            (step : int)
            (op : Op.t)
            ~real:(real_response : Response.t)
            ~mock:(mock_response : Response.t)
            ~program:(ops : Op.t list)])
;;

(* The derived [Op.t list] generator produces programs of about four ops, which is far too
   short to reach a state that needs several ops to set up (e.g. creating an expired
   session, sweeping it, then looking it up). Draw the length uniformly instead. *)
let program_generator =
  let open Base_quickcheck.Generator.Let_syntax in
  let%bind length = Base_quickcheck.Generator.int_uniform_inclusive 0 30 in
  Base_quickcheck.Generator.list_with_length ~length Op.quickcheck_generator
;;

let%test_unit "Real and Mock databases are observationally equivalent" =
  Thread_safe.block_on_async_exn (fun () ->
    Async_quickcheck.async_test
      ~trials:300
        (* This is an IO-heavy test, but each trial is a local-socket roundtrip *)
      ~sexp_of:[%sexp_of: Op.t list]
      program_generator
      ~f:(fun ops ->
        (* Truncated to a whole second: Postgres [TIMESTAMPTZ] only keeps microseconds, so
           a nanosecond-precision [now] would not round-trip. *)
        let now =
          Time_ns.now ()
          |> Time_ns.to_span_since_epoch
          |> Time_ns.Span.to_int_sec
          |> Time_ns.Span.of_int_sec
          |> Time_ns.of_span_since_epoch
        in
        Database.For_testing.with_test_connection ~f:(fun real_db ->
          let%bind () = Database.create_tables real_db >>| ok_exn in
          let%bind real = Side.create real_db ~now in
          let%bind mock = Side.create (Database.For_testing.create_in_memory ()) ~now in
          run_program ~real ~mock ops)))
;;
