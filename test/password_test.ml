open! Core
open! Import

let%expect_test "hash_exn result includes the algorithm, parameters, and salt" =
  let fields = Password.hash_exn "my secret password" |> String.split ~on:'$' in
  (* We don't print salt and the hash computed from it as they are random *)
  print_s
    [%message
      ""
        ~fields:(List.take fields 4 : string list)
        ~lengths:(List.map fields ~f:String.length : int list)];
  [%expect {| ((fields ("" argon2id v=19 m=19456,t=2,p=1)) (lengths (0 8 4 15 22 43))) |}]
;;

let verify_and_print ~password_hash password =
  print_s [%sexp (Password.verify_exn ~password_hash ~password : bool)]
;;

let%expect_test "verify_exn accepts only the exact password" =
  let password_hash = Password.hash_exn "my secret password" in
  verify_and_print ~password_hash "my secret password";
  [%expect {| true |}];
  verify_and_print ~password_hash "my secret password ";
  [%expect {| false |}];
  verify_and_print ~password_hash "My secret password";
  [%expect {| false |}];
  verify_and_print ~password_hash "my secret passwor";
  [%expect {| false |}];
  verify_and_print ~password_hash "";
  [%expect {| false |}]
;;

let%expect_test "verify_exn accepts every hash of the same password despite the salts" =
  let password = "my secret password" in
  let first_hash = Password.hash_exn password in
  let second_hash = Password.hash_exn password in
  print_s [%sexp (String.equal first_hash second_hash : bool)];
  [%expect {| false |}];
  verify_and_print ~password_hash:first_hash password;
  [%expect {| true |}];
  verify_and_print ~password_hash:second_hash password;
  [%expect {| true |}]
;;

let%expect_test "verify_exn raises on a malformed hash" =
  let valid_hash = Password.hash_exn "my secret password" in
  let verify password_hash =
    Expect_test_helpers_core.require_does_raise [%here] (fun () ->
      Password.verify_exn ~password_hash ~password:"my secret password")
  in
  verify "";
  [%expect {| ("Failed to verify password" (error "Decoding failed")) |}];
  verify "not a hash";
  [%expect {| ("Failed to verify password" (error "Decoding failed")) |}];
  verify (String.drop_suffix valid_hash 10);
  [%expect {| ("Failed to verify password" (error "Decoding failed")) |}];
  (* a hash of another Argon2 variant *)
  verify (String.substr_replace_first valid_hash ~pattern:"argon2id" ~with_:"argon2i");
  [%expect {| ("Failed to verify password" (error "Decoding failed")) |}]
;;
