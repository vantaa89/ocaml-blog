open! Core
open! Async
open! Import

module Endpoint = struct
  type t =
    | About
    | Cv
  [@@deriving sexp_of]
end

let max_body_length = 10 * 1024 * 1024

let create_token ~db ~label ~user_id ~now =
  let token = Token.generate () in
  let%map.Deferred.Or_error (_ : Database_schema.Api_token.t) =
    Database.Api_token.create
      db
      ~label
      ~token_hash:(Token.hash token)
      ~user_id
      ~created_at:now
  in
  token
;;

let respond ~db ~media_dir ~body endpoint request =
  let%bind result =
    let open Deferred.Or_error.Let_syntax in
    let%bind api_token =
      match
        Cohttp.Header.get (Cohttp.Request.headers request) "authorization"
        |> Option.bind ~f:(String.chop_prefix ~prefix:"Bearer ")
      with
      | None -> return None
      | Some token ->
        Database.Api_token.find_by_token_hash
          db
          ~token_hash:(Token.hash (Token.of_string token))
    in
    match api_token with
    | None -> return (`Unauthorized, "Expected a valid API token")
    | Some { label; user_id; _ } ->
      (* The CV is part of the about page, so both endpoints belong to its author *)
      (match%bind Database.Post.find_by_slug db ~slug:"about" with
       | None -> return (`Not_found, "There is no about post")
       | Some { id; title; slug; author_id; content_ko; special_post; _ } ->
         (match author_id = user_id with
          | false -> return (`Forbidden, "The about post belongs to another user")
          | true ->
            (match Cohttp.Request.encoding request with
             | Chunked | Unknown ->
               (* A body with an unknown length is rejected to avoid OOM *)
               return (`Length_required, "Expected a body of known length")
             | Fixed length ->
               (match Int64.(length <= of_int max_body_length) with
                | false -> return (`Request_entity_too_large, "The body is too large")
                | true ->
                  let%bind content = Cohttp_async.Body.to_string body |> Deferred.ok in
                  Log.Global.info_s
                    [%message
                      "API request" ~token:(label : string) (endpoint : Endpoint.t)];
                  (match endpoint with
                   | About ->
                     (match Soup.parse content |> Soup.select_one "body" with
                      | None ->
                        return (`Bad_request, "Expected an HTML document with a <body>")
                      | Some body ->
                        let body =
                          Soup.children body
                          |> Soup.to_list
                          |> List.map ~f:Soup.to_string
                          |> String.concat
                        in
                        let%map () =
                          Database.Post.update
                            db
                            ~id
                            ~title
                            ~slug
                            ~content_en:(Some body)
                            ~content_ko
                            ~special_post
                        in
                        `No_content, "")
                   | Cv ->
                     (match String.is_prefix content ~prefix:"%PDF-" with
                      | false -> return (`Unsupported_media_type, "Expected a PDF")
                      | true ->
                        let%map () =
                          Deferred.Or_error.try_with (fun () ->
                            Writer.save (media_dir ^/ "cv.pdf") ~contents:content)
                        in
                        `No_content, ""))))))
  in
  match result with
  | Error error ->
    Log.Global.error_s [%message "Error while handling an API request" (error : Error.t)];
    Cohttp_async.Server.respond_string
      ~status:`Internal_server_error
      "Internal server error"
  | Ok (`Unauthorized, message) ->
    Cohttp_async.Server.respond_string
      ~headers:(Cohttp.Header.of_list [ "www-authenticate", "Bearer" ])
      ~status:`Unauthorized
      message
  | Ok (status, message) -> Cohttp_async.Server.respond_string ~status message
;;
