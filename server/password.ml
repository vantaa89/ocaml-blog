open! Core
open! Import

let t_cost = 2
let m_cost = 19 * 1024 (* unit: KiB *)
let parallelism = 1
let salt_len = 16
let hash_len = 32

let hash_password_exn password =
  let salt = Mirage_crypto_rng_unix.getrandom salt_len in
  let encoded_len =
    Argon2.encoded_len ~t_cost ~m_cost ~parallelism ~salt_len ~hash_len ~kind:Argon2.ID
  in
  match
    Argon2.ID.hash_encoded
      ~t_cost
      ~m_cost
      ~parallelism
      ~pwd:password
      ~salt
      ~hash_len
      ~encoded_len
  with
  | Ok result -> Argon2.ID.encoded_to_string result
  | Error error ->
    raise_s
      [%message
        "Failed to hash the password" ~error:(Argon2.ErrorCodes.message error : string)]
;;
