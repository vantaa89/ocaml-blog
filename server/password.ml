open! Core
open! Import

let t_cost = 2
let m_cost = 19 * 1024 (* unit: KiB *)
let parallelism = 1
let salt_len = 16
let hash_len = 32

let hash_exn password =
  let salt = Mirage_crypto_rng_unix.getrandom salt_len in
  let encoded_len =
    Argon2.encoded_len ~t_cost ~m_cost ~parallelism ~salt_len ~hash_len ~kind:ID
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

let verify ~password_hash ~password =
  match Argon2.verify ~encoded:password_hash ~pwd:password ~kind:ID with
  | Ok true -> Ok true
  | Ok false ->
    (* A mismatch is reported as [Error VERIFY_MISMATCH], not [Ok false] *)
    Or_error.error_s [%message "Argon2.verify unexpectedly returned [Ok false]"]
  | Error VERIFY_MISMATCH -> Ok false
  | Error error ->
    Or_error.error_s
      [%message
        "Failed to verify password" ~error:(Argon2.ErrorCodes.message error : string)]
;;
