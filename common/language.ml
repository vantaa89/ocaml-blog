open! Core

module T = struct
  type t =
    | English
    | Korean
  [@@deriving bin_io, hash, compare, equal, sexp]
end

include Comparable.Make_plain (T)
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
