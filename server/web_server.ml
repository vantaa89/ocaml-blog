open! Core
open! Async
open! Import
module Server = Cohttp_async.Server

module Http_route = struct
  let media_url = String.chop_prefix_exn Urls.media_path ~prefix:"/"
  let static_url = "static"

  type t =
    | Media of { path : string }
    | Static of { path : string }
    | Index
    | Login
    | Logout
    | Not_found

  let of_request ~(meth : Cohttp.Code.meth) ~path : t =
    let segments = String.split path ~on:'/' |> List.filter ~f:(Fn.non String.is_empty) in
    match meth with
    | `GET | `HEAD ->
      (match segments with
       | [] -> Index
       | prefix :: rest ->
         let path = String.concat rest ~sep:"/" in
         (match String.equal prefix media_url, String.equal prefix static_url with
          | true, false -> Media { path }
          | false, true -> Static { path }
          | false, false -> Index
          | true, true -> Not_found (* impossible *)))
    | `POST ->
      (match segments with
       | [ "login" ] -> Login
       | [ "logout" ] -> Logout
       | _ -> Not_found)
    | _ -> Not_found
  ;;
end

let with_same_origin_check request ~f =
  match
    Cohttp.Request.headers request |> Cohttp_async_websocket.Header.origin_and_host_match
  with
  | Ok () -> f ()
  | Error error ->
    Log.Global.info_s [%message "Rejected cross-origin request" (error : Error.t)];
    Server.respond_string ~status:`Forbidden "Forbidden"
;;

let serve_file ~docroot ~path =
  (* [resolve_local_file] strips [..] segments, so a request cannot escape [docroot]. *)
  let file = Cohttp.Path.resolve_local_file ~docroot ~uri:(Uri.make ~path ()) in
  match%bind Sys.is_file file with
  | `Yes -> Server.respond_with_file file
  | `No | `Unknown -> Server.respond_string ~status:`Not_found "File not found"
;;

let serve_index ~static_dir =
  let file = static_dir ^/ "index.html" in
  match%bind Sys.file_exists file with
  | `No | `Unknown -> Server.respond_string ~status:`Not_found "File not found"
  | `Yes ->
    let%bind template = Reader.file_contents file in
    let html =
      List.fold
        [ "{{name}}", Owner_profile.info.name
        ; "{{description}}", Owner_profile.info.description
        ]
        ~init:template
        ~f:(fun html (pattern, value) ->
          (* [value] needs to be escaped *)
          let escaped = Buffer.create (String.length value) in
          Cmarkit_html.buffer_add_html_escaped_string escaped value;
          String.substr_replace_all html ~pattern ~with_:(Buffer.contents escaped))
    in
    Server.respond_string
      ~headers:(Cohttp.Header.of_list [ "content-type", "text/html; charset=utf-8" ])
      html
;;

let serve ~time_source db (config : Config.t) =
  let authenticator = Authenticator.create ~config ~time_source in
  let http_handler (config : Config.t) () ~body _address request =
    let meth = Cohttp.Request.meth request in
    let dispatch () =
      let uri = Cohttp.Request.uri request in
      match Http_route.of_request ~meth ~path:(Uri.path uri) with
      | Media { path } -> serve_file ~docroot:config.media_dir ~path
      | Static { path } -> serve_file ~docroot:config.static_dir ~path
      | Index -> serve_index ~static_dir:config.static_dir
      | Login ->
        let now = Time_source.now time_source in
        Authenticator.login authenticator ~db ~now ~body request
      | Logout -> Authenticator.logout ~db request
      | Not_found -> Server.respond_string ~status:`Not_found "Not found"
    in
    match meth with
    | `GET -> dispatch ()
    | `HEAD ->
      let%bind response, body = dispatch () in
      let%map () = Cohttp_async.Body.drain body in
      (* [encoding] specifies how the recipient knows the end of the body *)
      { response with encoding = Fixed 0L }, Cohttp_async.Body.empty
    | `POST | `PUT | `PATCH | `DELETE | `CONNECT | `OPTIONS | `TRACE | `Other _ ->
      (* A status-changing method should be checked for same-origin. [`GET] and [`HEAD]
         are exempt because browsers omit [Origin] on ordinary navigations. *)
      with_same_origin_check request ~f:dispatch
  in
  Rpc_websocket.Rpc.serve
    ~where_to_listen:(Tcp.Where_to_listen.of_port config.port)
    ~implementations:(Rpc_implementations.implementations ~db ~time_source config)
    ~initial_connection_state:(fun () initiated_from _address _connection ->
      let session_token =
        match (initiated_from : Rpc_websocket.Rpc.Connection_initiated_from.t) with
        | Tcp -> None
        | Websocket_request request -> Authenticator.session_token request
      in
      { session_token })
    ~http_handler:(http_handler config)
    ~on_handler_error:
      (`Call
          (fun address exn ->
            Log.Global.error_s
              [%message
                "Error while handling request"
                  (address : Socket.Address.Inet.t)
                  (exn : Exn.t)]))
    ()
;;
