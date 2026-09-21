open! Core
open! Async

type t =
  { attempts : Time_ns.t Deque.t String.Table.t
  ; max_attempts : int
  ; window : Time_ns.Span.t
  ; time_source : Time_source.t
  }

let create ~max_attempts ~window ~time_source =
  { attempts = String.Table.create (); max_attempts; window; time_source }
;;

let record_attempt t ~key =
  let now = Time_source.now t.time_source in
  match Hashtbl.find t.attempts key with
  | None ->
    let attempts = Deque.of_array [| now |] in
    (* This never raises as [t.attempts] does not have [key] *)
    Hashtbl.add_exn t.attempts ~key ~data:attempts;
    `Allowed
  | Some attempts ->
    let rec sweep () =
      match Deque.peek_front attempts with
      | None -> ()
      | Some past_attempt ->
        (match Time_ns.( > ) now (Time_ns.add past_attempt t.window) with
         | false -> ()
         | true ->
           Deque.drop_front attempts;
           sweep ())
    in
    sweep ();
    (match Deque.length attempts >= t.max_attempts with
     (* A rejected attempt is not recorded. The key is the username, so counting it would
        let anyone who knows the name hold the account shut for as long as they keep
        knocking. *)
     | true -> `Too_many
     | false ->
       Deque.enqueue_back attempts now;
       `Allowed)
;;

let clear t ~key = Hashtbl.remove t.attempts key
