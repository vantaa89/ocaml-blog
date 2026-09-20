open! Core
open! Import

let verify_and_print ~password_hash password =
  print_s [%sexp (Password.verify ~password_hash ~password : bool Or_error.t)]
;;

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

let%expect_test "verify accepts only the exact password" =
  let password_hash = Password.hash_exn "my secret password" in
  verify_and_print ~password_hash "my secret password";
  [%expect {| (Ok true) |}];
  verify_and_print ~password_hash "my secret password ";
  [%expect {| (Ok false) |}];
  verify_and_print ~password_hash "My secret password";
  [%expect {| (Ok false) |}];
  verify_and_print ~password_hash "my secret passwor";
  [%expect {| (Ok false) |}];
  verify_and_print ~password_hash "";
  [%expect {| (Ok false) |}]
;;

let%expect_test "verify accepts every hash of the same password despite the salts" =
  let password = "my secret password" in
  let first_hash = Password.hash_exn password in
  let second_hash = Password.hash_exn password in
  print_s [%sexp (String.equal first_hash second_hash : bool)];
  [%expect {| false |}];
  verify_and_print ~password_hash:first_hash password;
  [%expect {| (Ok true) |}];
  verify_and_print ~password_hash:second_hash password;
  [%expect {| (Ok true) |}]
;;

let%expect_test "verify returns an error on a malformed hash" =
  let password = "my secret password" in
  let valid_hash = Password.hash_exn password in
  verify_and_print ~password_hash:"" password;
  [%expect {| (Error ("Failed to verify password" (error "Decoding failed"))) |}];
  verify_and_print ~password_hash:"not a hash" password;
  [%expect {| (Error ("Failed to verify password" (error "Decoding failed"))) |}];
  verify_and_print ~password_hash:(String.drop_suffix valid_hash 10) password;
  [%expect {| (Error ("Failed to verify password" (error "Decoding failed"))) |}];
  (* a hash of another Argon2 variant *)
  verify_and_print
    ~password_hash:
      (String.substr_replace_first valid_hash ~pattern:"argon2id" ~with_:"argon2i")
    password;
  [%expect {| (Error ("Failed to verify password" (error "Decoding failed"))) |}]
;;
