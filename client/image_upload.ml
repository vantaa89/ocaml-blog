open! Core
open! Import
open Js_of_ocaml

module Failure = struct
  type t =
    | Too_large
    | Unsupported_format
    | Unreadable of { name : string }
    | Not_logged_in
    | Unreachable
end

let accept = "image/png,image/jpeg,image/gif,image/webp"

let files_of_list (files : File.fileList Js.t) =
  List.init files##.length ~f:Fn.id
  |> List.filter_map ~f:(fun index -> Js.Opt.to_option (files##item index))
;;

let read_file =
  Effect.of_deferred_fun (fun (file : File.file Js.t) ->
    let result = Async_kernel.Ivar.create () in
    let reader = new%js File.fileReader in
    let fill contents =
      Async_kernel.Ivar.fill_if_empty result contents;
      Js._false
    in
    reader##.onload
    := Dom.handler (fun _ ->
         Js.Opt.to_option (File.CoerceTo.arrayBuffer reader##.result)
         |> Option.map ~f:Typed_array.String.of_arrayBuffer
         |> fill);
    reader##.onerror := Dom.handler (fun _ -> fill None);
    reader##readAsArrayBuffer file;
    Async_kernel.Ivar.read result)
;;

let result_of_response
  : Rpcs.Upload_image.Response.t Or_error.t -> (string, Failure.t) Result.t
  = function
  | Ok (Uploaded { url }) -> Ok url
  | Ok Not_logged_in -> Error Not_logged_in
  | Ok Too_large -> Error Too_large
  | Ok Unsupported_format -> Error Unsupported_format
  | Error (_ : Error.t) -> Error Unreachable
;;

let upload ~dispatch (file : File.file Js.t) =
  match file##.size > Rpcs.Upload_image.max_size with
  | true -> Effect.return (Error (Too_large : Failure.t))
  | false ->
    (match%bind.Effect read_file file with
     | None ->
       Effect.return (Error (Unreadable { name = Js.to_string file##.name } : Failure.t))
     | Some contents ->
       dispatch ({ contents } : Rpcs.Upload_image.Query.t)
       |> Effect.map ~f:result_of_response)
;;

let insert ~textarea_id ~urls =
  Effect.of_sync_fun
    (fun () ->
       Dom_html.getElementById_coerce textarea_id Dom_html.CoerceTo.textarea
       |> Option.map ~f:(fun textarea ->
         let markdown =
           List.map urls ~f:(fun url -> [%string "![](%{url})"])
           |> String.concat ~sep:"\n"
           |> Js.string
         in
         let value = textarea##.value in
         let start = textarea##.selectionStart in
         let value =
           (value##substring 0 start)##concat_2
             markdown
             (value##substring_toEnd textarea##.selectionEnd)
         in
         let caret = start + markdown##.length in
         textarea##.value := value;
         textarea##.selectionStart := caret;
         textarea##.selectionEnd := caret;
         textarea##focus;
         Js.to_string value))
    ()
;;

let has_type (data : Dom_html.dataTransfer Js.t) type_ =
  Js.to_array data##.types
  |> Array.exists ~f:(fun candidate -> String.equal (Js.to_string candidate) type_)
;;

let drop_target ~upload =
  Vdom.Attr.many
    [ Vdom.Attr.on_dragover (fun event ->
        match has_type event##.dataTransfer "Files" with
        | true -> Effect.Prevent_default
        | false -> Effect.Ignore)
    ; Vdom.Attr.on_drop (fun event ->
        match files_of_list event##.dataTransfer##.files with
        | [] -> Effect.Ignore
        | files -> Effect.Many [ Effect.Prevent_default; upload files ])
    ; Vdom.Attr.on_paste (fun event ->
        let data = event##.clipboardData in
        (* Office apps put a picture of copied text on the clipboard next to the text. *)
        match has_type data "text/plain", files_of_list data##.files with
        | true, _ | false, [] -> Effect.Ignore
        | false, files -> Effect.Many [ Effect.Prevent_default; upload files ])
    ]
;;

let button ~uploading ~upload =
  let label =
    match uploading with
    | true -> "Uploading…"
    | false -> "Insert image"
  in
  Vdom.Node.label
    ~attrs:
      [ Vdom.Attr.classes
          ("editor-image-button"
           ::
           (match uploading with
            | true -> [ "uploading" ]
            | false -> []))
      ; Vdom.Attr.title label
      ]
    [ Vdom.Node.input
        ~attrs:
          ([ Vdom.Attr.type_ "file"
           ; Vdom.Attr.create "accept" accept
           ; Vdom.Attr.create "multiple" ""
           ; Vdom.Attr.on_file_input (fun event files ->
               let files = files_of_list files in
               (* Picking the same file again would otherwise not count as an input. *)
               Js.Opt.iter event##.target (fun target ->
                 Js.Opt.iter (Dom_html.CoerceTo.input target) (fun input ->
                   input##.value := Js.string ""));
               upload files)
           ]
           @
           match uploading with
           | true -> [ Vdom.Attr.disabled ]
           | false -> [])
        ()
    ; Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "/static/icons/image.svg"; Vdom.Attr.alt label ]
        ()
    ]
;;
