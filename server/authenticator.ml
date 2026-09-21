open! Core
open! Async
open! Import

let cookie_name = "sessionid"

type t = { login_attempts : Rate_limiter.t }

let create
      ~config:({ max_login_attempts; login_attempt_window; _ } : Config.t)
      ~time_source
  =
  { login_attempts =
      Rate_limiter.create
        ~max_attempts:max_login_attempts
        ~window:login_attempt_window
        ~time_source
  }
;;

let session_span = Time_ns.Span.of_day 14.
let token_length = 32

(* The cookie carries the token itself and the database keeps only its digest, so a leaked
   [session] table does not hand out usable sessions. *)
let hash_token token = Digestif.SHA256.digest_string token |> Digestif.SHA256.to_hex

let session_cookie ~value ~max_age =
  let header =
    Cohttp.Cookie.Set_cookie_hdr.make
      ~expiration:(`Max_age (Time_ns.Span.to_int_sec max_age |> Int64.of_int))
      ~path:"/"
      ~secure:true
      ~http_only:true
      (cookie_name, value)
    |> Cohttp.Cookie.Set_cookie_hdr.serialize
  in
  Cohttp.Header.of_list [ header ]
;;

let respond_internal_error error =
  Log.Global.error_s
    [%message "Error while handling an authentication request" (error : Error.t)];
  Cohttp_async.Server.respond_string
    ~status:`Internal_server_error
    "Internal server error"
;;

let login t ~db ~body _request =
  let%bind body = Cohttp_async.Body.to_string body in
  let field name =
    let form = Uri.query_of_encoded body in
    (* [Uri.query_of_encoded] splits each value on [,], so we concatenate using comma *)
    List.Assoc.find form name ~equal:String.equal
    |> Option.map ~f:(String.concat ~sep:",")
  in
  match Option.both (field "username") (field "password") with
  | None ->
    Cohttp_async.Server.respond_string
      ~status:`Bad_request
      "Expected a username and a password"
  | Some (username, password) ->
    (match Rate_limiter.record_attempt t.login_attempts ~key:username with
     | `Too_many ->
       Cohttp_async.Server.respond_string
         ~status:`Too_many_requests
         "Too many login attempts; try again later"
     | `Allowed ->
       let logged_in =
         let open Deferred.Or_error.Let_syntax in
         match%bind
           Database.User.find_by_username db ~username
           |> Deferred.Or_error.tag ~tag:"looking up the user"
         with
         | None -> return `Denied
         | Some user ->
           (match%bind
              Password.verify ~password_hash:user.password_hash ~password
              |> Deferred.return
            with
            | false -> return `Denied
            | true ->
              (* Logins are the only location where the [session] table grows, so we sweep
                 the table here to remove stale entries. *)
              don't_wait_for
                (let%map.Deferred swept = Database.Session.delete_expired db in
                 Or_error.iter_error swept ~f:(fun error ->
                   Log.Global.error_s
                     [%message "Failed to delete expired sessions" (error : Error.t)]));
              let token =
                Mirage_crypto_rng_unix.getrandom token_length
                |> Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
              in
              let now = Time_ns.now () in
              let%bind () =
                Database.User.set_last_login db ~username ~last_login:now
                |> Deferred.Or_error.tag ~tag:"recording the login"
              in
              let%map (_ : Database_schema.Session.t) =
                Database.Session.create
                  db
                  ~token_hash:(hash_token token)
                  ~user_id:user.id
                  ~expires_at:(Time_ns.add now session_span)
                |> Deferred.Or_error.tag ~tag:"creating the session"
              in
              `Logged_in token)
       in
       (match%bind logged_in with
        | Error error -> respond_internal_error error
        | Ok `Denied ->
          Cohttp_async.Server.respond_string
            ~status:`Unauthorized
            "Invalid username or password"
        | Ok (`Logged_in token) ->
          Rate_limiter.clear t.login_attempts ~key:username;
          Cohttp_async.Server.respond
            ~headers:(session_cookie ~value:token ~max_age:session_span)
            `No_content))
;;

let session_token request =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Cohttp.Request.headers request) in
  List.Assoc.find cookies cookie_name ~equal:String.equal
;;

let current_user_id ~db ~session_token =
  let open Deferred.Or_error.Let_syntax in
  match session_token with
  | None -> return None
  | Some token ->
    let%map session =
      Database.Session.find_by_token_hash db ~token_hash:(hash_token token)
    in
    Option.bind session ~f:(fun (session : Database_schema.Session.t) ->
      match Time_ns.( > ) session.expires_at (Time_ns.now ()) with
      | false -> None
      | true -> Some session.user_id)
;;

let logout ~db request =
  let respond_logged_out () =
    let headers =
      (* Cookies do not provide deletion, so this is the standard way of deleting cookies.
         The value is set somewhat arbitrarily, but it must not be empty:
         [Set_cookie_hdr.serialize] then omits the [=] and browsers ignore the whole
         header. *)
      session_cookie ~value:"deleted" ~max_age:Time_ns.Span.zero
    in
    Cohttp_async.Server.respond ~headers `No_content
  in
  match session_token request with
  | None -> respond_logged_out ()
  | Some token ->
    (match%bind Database.Session.delete db ~token_hash:(hash_token token) with
     | Error error -> respond_internal_error error
     | Ok () -> respond_logged_out ())
;;
