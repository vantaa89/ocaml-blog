open! Core

let expect_at_most_one list ~error_message =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: _ :: _ -> raise_s (error_message list)
;;
