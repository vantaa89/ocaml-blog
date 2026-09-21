open! Core
open! Async
open! Import

let max_attempts = 3
let window = Time_ns.Span.of_min 10.

let with_limiter ~f =
  let time_source =
    Time_source.create ~now:(Time_ns.of_string_with_utc_offset "2026-08-01 00:00:00Z") ()
  in
  let t =
    Rate_limiter.create
      ~max_attempts
      ~window
      ~time_source:(Time_source.read_only time_source)
  in
  let advance span = Time_source.advance_by_alarms_by time_source span in
  f t ~advance
;;

let attempt t ~key =
  let result = Rate_limiter.record_attempt t ~key in
  print_s [%sexp (result : [ `Allowed | `Too_many ])]
;;

let%expect_test "the first [max_attempts] are allowed and the rest are not" =
  let key = "username" in
  with_limiter ~f:(fun t ~advance:_ ->
    attempt t ~key;
    [%expect {| Allowed |}];
    attempt t ~key;
    [%expect {| Allowed |}];
    attempt t ~key;
    [%expect {| Allowed |}];
    attempt t ~key;
    [%expect {| Too_many |}];
    attempt t ~key;
    [%expect {| Too_many |}];
    return ())
;;

let%expect_test "keys are counted separately" =
  with_limiter ~f:(fun t ~advance:_ ->
    List.init max_attempts ~f:Fn.id
    |> List.iter ~f:(fun (_ : int) -> attempt t ~key:"author");
    [%expect
      {|
      Allowed
      Allowed
      Allowed
    |}];
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    attempt t ~key:"someone-else";
    [%expect {| Allowed |}];
    return ())
;;

let%expect_test "attempts age out of the window one by one" =
  with_limiter ~f:(fun t ~advance ->
    (* Spread the attempts a minute apart, so they also expire a minute apart. *)
    let%bind () =
      List.init max_attempts ~f:Fn.id
      |> Deferred.List.iter ~how:`Sequential ~f:(fun _ ->
        attempt t ~key:"author";
        advance (Time_ns.Span.of_min 1.))
    in
    [%expect
      {|
      Allowed
      Allowed
      Allowed
      |}];
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    let%bind () = advance Time_ns.Span.(window - of_min 3. + of_sec 1.) in
    attempt t ~key:"author";
    [%expect {| Allowed |}];
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    return ())
;;

let%expect_test "a rejected attempt does not extend the lockout" =
  with_limiter ~f:(fun t ~advance ->
    List.init max_attempts ~f:Fn.id |> List.iter ~f:(fun _ -> attempt t ~key:"author");
    [%expect
      {|
    Allowed
    Allowed
    Allowed
    |}];
    let%bind () = advance (Time_ns.Span.of_min 5.) in
    (* Below attempts are rejected *)
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    (* Rejected attempts are not recorded, so a new attempt past the window is allowed *)
    let%bind () = advance Time_ns.Span.(of_min 5. + of_sec 1.) in
    attempt t ~key:"author";
    [%expect {| Allowed |}];
    return ())
;;

let%expect_test "[clear] forgets a key's attempts" =
  with_limiter ~f:(fun t ~advance:_ ->
    List.init max_attempts ~f:Fn.id |> List.iter ~f:(fun _ -> attempt t ~key:"author");
    [%expect
      {|
      Allowed
      Allowed
      Allowed |}];
    attempt t ~key:"author";
    [%expect {| Too_many |}];
    Rate_limiter.clear t ~key:"author";
    attempt t ~key:"author";
    [%expect {| Allowed |}];
    (* Clearing a key that was never seen is a no-op. *)
    Rate_limiter.clear t ~key:"nobody";
    return ())
;;
