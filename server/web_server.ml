open! Core
open! Async
open! Import
module Server = Cohttp_async.Server

module Http_route = struct
  let media_url = "media"
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
    | `GET ->
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
       | [ "users"; "login" ] -> Login
       | [ "users"; "logout" ] -> Logout
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
  let file = Server.resolve_local_file ~docroot ~uri:(Uri.make ~path ()) in
  match%bind Sys.file_exists file with
  | `Yes -> Server.respond_with_file file
  | `No | `Unknown -> Server.respond_string ~status:`Not_found "File not found"
;;

let serve db (config : Config.t) =
  let http_handler (config : Config.t) () ~body _address request =
    let meth = Cohttp.Request.meth request in
    let dispatch () =
      let uri = Cohttp.Request.uri request in
      match Http_route.of_request ~meth ~path:(Uri.path uri) with
      | Media { path } -> serve_file ~docroot:config.media_dir ~path
      | Static { path } -> serve_file ~docroot:config.static_dir ~path
      | Index -> serve_file ~docroot:config.static_dir ~path:"index.html"
      | Login -> Authentication.handle_login db config ~body request
      | Logout -> Authentication.handle_logout db config request
      | Not_found -> Server.respond_string ~status:`Not_found "Not found"
    in
    match meth with
    | `GET | `HEAD -> dispatch ()
    | `POST | `PUT | `PATCH | `DELETE | `CONNECT | `OPTIONS | `TRACE | `Other _ ->
      (* A status-changing method should be checked for same-origin. [`GET] and [`HEAD]
         are exempt because browsers omit [Origin] on ordinary navigations. *)
      with_same_origin_check request ~f:dispatch
  in
  Rpc_websocket.Rpc.serve
    ~where_to_listen:(Tcp.Where_to_listen.of_port config.port)
    ~implementations:
      (Rpc_implementations.implementations ~media_url:("/" ^ Http_route.media_url))
    ~initial_connection_state:(fun () _initiated_from _address _connection -> db)
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
