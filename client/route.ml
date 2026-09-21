open! Core
open! Import

type t =
  | Home
  | Posts of
      { tag_slug : string option
      ; page : int
      }
  | Post of { slug : string }
  | New_post
  | Edit_post of { slug : string }
  | About
  | Search of
      { query : string
      ; page : int
      }
  | Login
[@@deriving sexp, equal]

let page_of_query query =
  match Map.find query "page" with
  | None -> 1
  | Some values ->
    (match List.hd values with
     | None -> 1
     | Some value ->
       (match Int.of_string_opt value with
        | None -> 1
        | Some page -> Int.max 1 page))
;;

let parse_exn (components : Bonsai_web_ui_url_var.Components.t) : t =
  let segments =
    String.split components.path ~on:'/'
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Uri.pct_decode
  in
  let page = page_of_query components.query in
  match segments with
  | [] -> Home
  | [ "about" ] -> About
  | [ "posts" ] ->
    let tag_slug =
      Map.find components.query "tag"
      |> Option.bind ~f:List.hd
      |> Option.map ~f:String.strip
    in
    Posts { tag_slug; page }
  | [ "post"; slug ] -> Post { slug }
  | [ "post"; slug; "edit" ] -> Edit_post { slug }
  | [ "new-post" ] -> New_post
  | [ "login" ] -> Login
  | [ "search"; query ] -> Search { query; page }
  | _ -> Home
;;

let query_of_alist alist = String.Map.of_alist_exn (List.filter_opt alist)

let page_param page =
  match page with
  | 1 -> None
  | page -> Some ("page", [ Int.to_string page ])
;;

let unparse (t : t) : Bonsai_web_ui_url_var.Components.t =
  let create ?query path = Bonsai_web_ui_url_var.Components.create ~path ?query () in
  match t with
  | Home -> create ""
  | About -> create "about"
  | Login -> create "login"
  | Post { slug } -> create [%string "post/%{Uri.pct_encode slug}"]
  | New_post -> create "new-post"
  | Edit_post { slug } -> create [%string "post/%{Uri.pct_encode slug}/edit"]
  | Posts { tag_slug; page } ->
    let query =
      query_of_alist
        [ Option.map tag_slug ~f:(fun tag_slug -> "tag", [ tag_slug ]); page_param page ]
    in
    create ~query "posts"
  | Search { query; page } ->
    let query_params = query_of_alist [ page_param page ] in
    create ~query:query_params [%string "search/%{Uri.pct_encode query}"]
;;

let to_string t =
  let (components : Bonsai_web_ui_url_var.Components.t) = unparse t in
  let query =
    match Map.is_empty components.query with
    | true -> ""
    | false -> "?" ^ Uri.encoded_of_query (Map.to_alist components.query)
  in
  [%string "/%{components.path}%{query}"]
;;

let with_page t page =
  match t with
  | Posts { tag_slug; page = _ } -> Posts { tag_slug; page }
  | Search { query; page = _ } -> Search { query; page }
  | (Home | About | Post _ | New_post | Edit_post _ | Login) as t -> t
;;

let title = function
  | Home -> "Your Name"
  | Posts { tag_slug = None; _ } -> "Posts"
  | Posts { tag_slug = Some tag_slug; _ } -> [%string "tag: %{tag_slug}"]
  | Post { slug } -> slug
  | New_post -> "new post"
  | Edit_post { slug } -> [%string "edit: %{slug}"]
  | About -> "about"
  | Search { query; _ } -> [%string "search: %{query}"]
  | Login -> "login"
;;
