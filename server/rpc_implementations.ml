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
  Deferred.Or_error.List.map ~how:`Sequential posts ~f:(fun post ->
    let%map.Deferred.Or_error tags =
      Database.Post_tag.tags_for_post db ~post_id:post.id
    in
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
     }
     : Rpcs.Post_summary.t))
;;

let find_post db ~slug ~viewer =
  let open Deferred.Or_error.Let_syntax in
  let%bind post = Database.Post.find_by_slug db ~slug in
  match post with
  | None -> return None
  | Some post ->
    (match (not post.hidden) || [%equal: int option] viewer (Some post.author_id) with
     | false -> return None
     | true ->
       let%map post = post_to_rpc db post in
       Some post)
;;

let tags_with_counts db =
  let%map.Deferred.Or_error tags = Database.Tag.list_with_post_counts db in
  List.map tags ~f:(fun (tag, post_count) : Rpcs.Tag_with_count.t ->
    { tag = tag_to_rpc tag; post_count })
;;

let publication_to_rpc db (publication : Database_schema.Publication.t) =
  let%map.Deferred.Or_error image =
    Database.Image.find_by_id db ~id:publication.image_id
  in
  let image_url =
    match image with
    | None -> ""
    | Some image -> Database_schema.Image.url image
  in
  ({ title = publication.title
   ; image_url
   ; authors = publication.authors
   ; journal = publication.journal
   ; link = publication.link
   }
   : Rpcs.Publication.t)
;;

let main_page db ~viewer =
  let open Deferred.Or_error.Let_syntax in
  let%bind main_post = find_post db ~slug:"main" ~viewer in
  let%bind recent_posts = Database.Post.list db ~viewer:None ~limit:4 () in
  let%bind recent_posts = posts_to_summaries db recent_posts in
  let%bind publications = Database.Publication.list db () in
  let%bind publications =
    Deferred.Or_error.List.map publications ~how:`Sequential ~f:(publication_to_rpc db)
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

let post_list db ~query ~viewer =
  let open Deferred.Or_error.Let_syntax in
  let ({ tag_slug; limit; offset } : Rpcs.Get_post_list.Query.t) = query in
  let%bind posts =
    match tag_slug with
    | None -> Database.Post.list db ~viewer ?limit ?offset ()
    | Some slug -> Database.Post.list_by_tag_slug db ~slug ~viewer ?limit ?offset ()
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

let set_post_hidden db ~query:({ slug; hidden } : Rpcs.Set_post_hidden.Query.t) ~user_id =
  match%bind.Deferred.Or_error Database.Post.find_by_slug db ~slug with
  | None -> Deferred.Or_error.error_s [%message "No such post" (slug : string)]
  | Some post ->
    (match [%equal: int option] user_id (Some post.author_id) with
     | false -> Deferred.Or_error.error_s [%message "Not yours to change" (slug : string)]
     | true -> Database.Post.set_hidden db ~id:post.id ~hidden)
;;

let duplicate_slug ?except_id slug ~db =
  match%map.Deferred.Or_error Database.Post.find_by_slug db ~slug with
  | None -> false
  | Some existing ->
    (match except_id with
     | None -> true
     | Some id -> existing.id <> id)
;;

let create_post db ~(form : Rpcs.Post_form.t) ~user_id ~now =
  let open Deferred.Or_error.Let_syntax in
  let open Rpcs.Create_post.Response in
  match user_id with
  | None -> return Not_logged_in
  | Some author_id ->
    (match%bind duplicate_slug ~db form.slug with
     | true -> return Duplicate_slug
     | false ->
       let%map (_ : Database_schema.Post.t) =
         Database.Post.create
           db
           ~title:form.title
           ~slug:form.slug
           ~content_en:(Map.find form.content English)
           ~content_ko:(Map.find form.content Korean)
           ~author_id
           ~special_post:form.special_post
           ~now
       in
       Saved)
;;

let update_post db ~query:({ slug; form } : Rpcs.Update_post.Query.t) ~user_id =
  let open Deferred.Or_error.Let_syntax in
  let open Rpcs.Update_post.Response in
  match%bind Database.Post.find_by_slug db ~slug with
  | None -> return Not_found
  | Some post ->
    (match [%equal: int option] user_id (Some post.author_id) with
     | false -> return Not_found
     | true ->
       (match%bind duplicate_slug form.slug ~db ~except_id:post.id with
        | true -> return Duplicate_slug
        | false ->
          let%bind () =
            Database.Post.update
              db
              ~id:post.id
              ~title:form.title
              ~slug:form.slug
              ~content_en:(Map.find form.content English)
              ~content_ko:(Map.find form.content Korean)
              ~special_post:form.special_post
          in
          return Saved))
;;

let current_user db ~now ~session_token =
  let open Deferred.Or_error.Let_syntax in
  let%bind user_id = Authenticator.current_user_id ~db ~now ~session_token in
  let%map user =
    match user_id with
    | None -> return None
    | Some id -> Database.User.find_by_id db ~id
  in
  ((match user with
    | None -> Not_logged_in
    | Some user -> Logged_in { username = user.username })
   : Rpcs.Get_current_user.Response.t)
;;

module Connection_state = struct
  type t = { session_token : string option }
end

let implement rpc f =
  Rpc.Rpc.implement rpc (fun (state : Connection_state.t) query ->
    f state query >>| ok_exn)
;;

let implementations ~db ~time_source =
  let open Deferred.Or_error.Let_syntax in
  let current_user_id ~session_token =
    Authenticator.current_user_id ~db ~now:(Time_source.now time_source) ~session_token
  in
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ implement Rpcs.Get_main_page.rpc (fun { session_token } () ->
          let%bind viewer = current_user_id ~session_token in
          main_page db ~viewer)
      ; implement Rpcs.Get_about_page.rpc (fun { session_token } () ->
          let%bind viewer = current_user_id ~session_token in
          find_post db ~slug:"about" ~viewer)
      ; implement Rpcs.Get_post.rpc (fun { session_token } { slug } ->
          let%bind viewer = current_user_id ~session_token in
          find_post db ~slug ~viewer)
      ; implement Rpcs.Get_post_list.rpc (fun { session_token } query ->
          let%bind viewer = current_user_id ~session_token in
          post_list db ~query ~viewer)
      ; implement Rpcs.Get_tags.rpc (fun _state () -> tags_with_counts db)
      ; implement Rpcs.Search_posts.rpc (fun _state { query } -> search db ~query)
      ; implement Rpcs.Set_post_hidden.rpc (fun { session_token } query ->
          let%bind user_id = current_user_id ~session_token in
          set_post_hidden db ~query ~user_id)
      ; implement Rpcs.Create_post.rpc (fun { session_token } form ->
          let now = Time_source.now time_source in
          let%bind user_id = Authenticator.current_user_id ~db ~now ~session_token in
          create_post db ~form ~user_id ~now)
      ; implement Rpcs.Update_post.rpc (fun { session_token } query ->
          let%bind user_id = current_user_id ~session_token in
          update_post db ~query ~user_id)
      ; implement Rpcs.Get_current_user.rpc (fun { session_token } () ->
          current_user db ~now:(Time_source.now time_source) ~session_token)
      ; implement Rpcs.Render_markdown.rpc (fun _state { markdown } ->
          Deferred.Or_error.return (Markdown_renderer.render ~markdown))
      ]
;;
