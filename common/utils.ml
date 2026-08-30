open! Core

let expect_at_most_one list ~error_message =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: _ :: _ -> failwith (error_message list)
;;
