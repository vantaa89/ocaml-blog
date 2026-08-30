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
    | _ -> Not_found
  ;;
end

let respond_not_found () = Server.respond_string ~status:`Not_found "Not found"

let serve_file ~docroot ~path =
  (* [resolve_local_file] strips [..] segments, so a request cannot escape [docroot]. *)
  let file = Server.resolve_local_file ~docroot ~uri:(Uri.make ~path ()) in
  match%bind Sys.file_exists file with
  | `Yes -> Server.respond_with_file file
  | `No | `Unknown -> respond_not_found ()
;;

let serve db (config : Config.t) =
  let http_handler (config : Config.t) () ~body:_ _address request =
    let uri = Cohttp.Request.uri request in
    match
      Http_route.of_request ~meth:(Cohttp.Request.meth request) ~path:(Uri.path uri)
    with
    | Media { path } -> serve_file ~docroot:config.media_dir ~path
    | Static { path } -> serve_file ~docroot:config.static_dir ~path
    | Index -> serve_file ~docroot:config.static_dir ~path:"index.html"
    | Not_found -> respond_not_found ()
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
