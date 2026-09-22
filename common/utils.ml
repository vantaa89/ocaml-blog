open! Core

let expect_at_most_one list ~error_message =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: _ :: _ -> raise_s (error_message list)
;;

let first_image ~markdown =
  let pattern = Re.Perl.compile_pat {|!\[\]\(([^\s)]*)\)|} in
  let%map.Option match_groups = Re.exec_opt pattern markdown in
  Re.Group.get match_groups 1
;;
