open! Core

module T = struct
  type t =
    | English
    | Korean
  [@@deriving hash, compare, sexp_of]
end

include Hashable.Make_plain (T)
include T

let to_code = function
  | English -> "en"
  | Korean -> "ko"
;;

let of_code_exn code =
  match code with
  | "en" -> English
  | "ko" -> Korean
  | _ -> raise_s [%message "Unknown language code " (code : string)]
;;
