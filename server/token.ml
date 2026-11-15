open! Core
open! Import

type t = string

let length = 32

let generate () =
  Mirage_crypto_rng_unix.getrandom length
  |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
;;

let of_string = Fn.id
let to_string = Fn.id

module Hash = struct
  type t = string [@@deriving equal, sexp_of]

  let of_string = Fn.id
  let to_string = Fn.id
end

let hash token = Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex
