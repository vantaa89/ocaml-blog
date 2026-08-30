open! Core
open! Async
open! Import

let tag_to_rpc (tag : Database_schema.Tag.t) : Rpcs.Tag.t =
  { name = tag.name; slug = tag.slug }
;;

let primary_content (post : Database_schema.Post.t) =
  (* English takes the precedence *)
  let content = Database_schema.Post.content_by_language post in
  match Map.find content English with
  | Some content -> content
  | None -> Option.value (Map.find content Korean) ~default:""
;;

let post_to_rpc db (post : Database_schema.Post.t) =
  let%map.Deferred.Or_error tags = Database.Post_tag.tags_for_post db ~post_id:post.id in
  ({ title = post.title
   ; slug = post.slug
   ; content = Database_schema.Post.content_by_language post
   ; created_at = post.created_at
   ; tags = List.map tags ~f:tag_to_rpc
   ; special_post = post.special_post
   ; hidden = post.hidden
   }
   : Rpcs.Post.t)
;;

(* Returns an excerpt with at most [max_length] characters of [content], centered on the
   first occurrence of [query] when there is one. The result can exceed [max_length] as it
   may contain elipses *)
let excerpt content ~query ~max_length =
  let content = String.tr content ~target:'\n' ~replacement:' ' in
  let length = String.length content in
  let start =
    Option.value_map query ~default:0 ~f:(fun query ->
      match String.length query >= 2 with
      | true ->
        (match
           String.substr_index
             (String.lowercase content)
             ~pattern:(String.lowercase query)
         with
         | None -> 0 (* No match with [query]; start from the start *)
         | Some match_position ->
           Int.max 0 (Int.min (match_position - (max_length / 2)) (length - max_length)))
      | false -> 0)
  in
  let stop = Int.min length (start + max_length) in
  let prefix =
    match start > 0 with
    | true -> "..."
    | false -> ""
  in
  let suffix =
    match stop < length with
    | true -> "..."
    | false -> ""
  in
  prefix ^ String.sub content ~pos:start ~len:(stop - start) ^ suffix
;;

let posts_to_summaries db ?query (posts : Database_schema.Post.t list) =
  let open Deferred.Or_error.Let_syntax in
  Deferred.Or_error.List.map ~how:`Sequential posts ~f:(fun post ->
    let%map tags = Database.Post_tag.tags_for_post db ~post_id:post.id in
    ({ title = post.title
     ; slug = post.slug
     ; excerpt =
         excerpt
           (primary_content post)
           ~query
           ~max_length:Rpcs.Post_summary.max_excerpt_length
     ; created_at = post.created_at
     ; tags = List.map tags ~f:tag_to_rpc
     ; languages = Map.keys (Database_schema.Post.content_by_language post)
     ; hidden = post.hidden
     }
     : Rpcs.Post_summary.t))
;;

let find_post db ~slug =
  let open Deferred.Or_error.Let_syntax in
  let%bind post = Database.Post.find_by_slug db ~slug in
  match post with
  | None -> return None
  | Some post ->
    let%map post = post_to_rpc db post in
    Some post
;;

let tags_with_counts db =
  let%map.Deferred.Or_error tags = Database.Tag.list_with_post_counts db in
  List.map tags ~f:(fun (tag, post_count) : Rpcs.Tag_with_count.t ->
    { tag = tag_to_rpc tag; post_count })
;;

let publication_to_rpc db ~media_url (publication : Database_schema.Publication.t) =
  let%map.Deferred.Or_error image =
    Database.Image.find_by_id db ~id:publication.image_id
  in
  let image_url =
    match image with
    | None -> ""
    | Some image -> Database_schema.Image.url image ~media_url
  in
  ({ title = publication.title
   ; image_url
   ; authors = publication.authors
   ; journal = publication.journal
   ; link = publication.link
   }
   : Rpcs.Publication.t)
;;

let main_page db ~media_url =
  let open Deferred.Or_error.Let_syntax in
  let%bind main_post = find_post db ~slug:"main" in
  let%bind recent_posts = Database.Post.list db ~limit:4 () in
  let%bind recent_posts = posts_to_summaries db recent_posts in
  let%bind publications = Database.Publication.list db () in
  let%bind publications =
    Deferred.Or_error.List.map
      publications
      ~how:`Sequential
      ~f:(publication_to_rpc db ~media_url)
  in
  let%map news = Database.News.list db in
  ({ main_post
   ; recent_posts
   ; publications
   ; news =
       List.map news ~f:(fun news : Rpcs.News.t ->
         { content = news.content; date = news.date })
   }
   : Rpcs.Get_main_page.Response.t)
;;

let post_list db ~query ~include_hidden =
  let open Deferred.Or_error.Let_syntax in
  let ({ tag_slug; limit; offset } : Rpcs.Get_post_list.Query.t) = query in
  let%bind posts =
    match tag_slug with
    | None -> Database.Post.list db ~include_hidden ?limit ?offset ()
    | Some slug ->
      Database.Post.list_by_tag_slug db ~slug ~include_hidden ?limit ?offset ()
  in
  posts_to_summaries db posts
;;

let search db ~query =
  let open Deferred.Or_error.Let_syntax in
  let query = String.strip query in
  match String.length query < 2 with
  | true -> return []
  | false ->
    let%bind posts = Database.Post.search db ~query () in
    posts_to_summaries db posts ~query
;;

(* TODO: Hidden posts are only visible to their authenticated author. Authentication is
   not wired up yet, so they stay hidden from every caller. *)
let include_hidden = false
let implement rpc f = Rpc.Rpc.implement rpc (fun db query -> f db query >>| ok_exn)

let implementations ~media_url =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ implement Rpcs.Get_main_page.rpc (fun db () -> main_page db ~media_url)
      ; implement Rpcs.Get_about_page.rpc (fun db () -> find_post db ~slug:"about")
      ; implement Rpcs.Get_post.rpc (fun db { slug } -> find_post db ~slug)
      ; implement Rpcs.Get_post_list.rpc (fun db query ->
          post_list db ~query ~include_hidden)
      ; implement Rpcs.Get_tags.rpc (fun db () -> tags_with_counts db)
      ; implement Rpcs.Search_posts.rpc (fun db { query } -> search db ~query)
      ; implement Rpcs.Render_markdown.rpc (fun _db { markdown } ->
          Deferred.Or_error.return (Markdown_renderer.render ~markdown))
      ]
;;
