open! Core

type t =
  | Home
  | Posts of
      { tag : string option
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
  | Not_found of { path : string }
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

let of_url ~path ~query : t =
  let segments =
    String.split path ~on:'/'
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Uri.pct_decode
  in
  let page = page_of_query query in
  match segments with
  | [] -> Home
  | [ "about" ] -> About
  | [ "posts" ] ->
    let tag =
      Map.find query "tag" |> Option.bind ~f:List.hd |> Option.map ~f:String.strip
    in
    Posts { tag; page }
  | [ "post"; slug ] -> Post { slug }
  | [ "post"; slug; "edit" ] -> Edit_post { slug }
  | [ "new-post" ] -> New_post
  | [ "login" ] -> Login
  | [ "search"; query ] -> Search { query; page }
  | _ -> Not_found { path }
;;

let query_of_alist alist = String.Map.of_alist_exn (List.filter_opt alist)

let page_param page =
  match page with
  | 1 -> None
  | page -> Some ("page", [ Int.to_string page ])
;;

let to_url (t : t) =
  match t with
  | Home -> "", String.Map.empty
  | About -> "about", String.Map.empty
  | Login -> "login", String.Map.empty
  | Not_found { path } -> path, String.Map.empty
  | Post { slug } -> [%string "post/%{Uri.pct_encode slug}"], String.Map.empty
  | New_post -> "new-post", String.Map.empty
  | Edit_post { slug } -> [%string "post/%{Uri.pct_encode slug}/edit"], String.Map.empty
  | Posts { tag; page } ->
    ( "posts"
    , query_of_alist [ Option.map tag ~f:(fun tag -> "tag", [ tag ]); page_param page ] )
  | Search { query; page } ->
    [%string "search/%{Uri.pct_encode query}"], query_of_alist [ page_param page ]
;;

let to_string t =
  let path, query = to_url t in
  let query =
    match Map.is_empty query with
    | true -> ""
    | false -> "?" ^ Uri.encoded_of_query (Map.to_alist query)
  in
  [%string "/%{path}%{query}"]
;;

let with_page t page =
  match t with
  | Posts { tag; page = _ } -> Posts { tag; page }
  | Search { query; page = _ } -> Search { query; page }
  | (Home | About | Post _ | New_post | Edit_post _ | Login | Not_found _) as t -> t
;;

let title = function
  | Home -> Owner_profile.info.name
  | Posts { tag = None; _ } -> "Posts"
  | Posts { tag = Some tag; _ } -> [%string "tag: %{tag}"]
  | Post { slug } -> slug
  | New_post -> "new post"
  | Edit_post { slug } -> [%string "edit: %{slug}"]
  | About -> "about"
  | Search { query; _ } -> [%string "search: %{query}"]
  | Login -> "login"
  | Not_found _ -> "not found"
;;
