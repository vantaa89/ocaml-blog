open! Core
open! Import

let mark =
  let size = 4 in
  let sign_changes row =
    List.zip_exn (List.drop_last_exn row) (List.tl_exn row)
    |> List.count ~f:(fun (a, b) -> not (Bool.equal a b))
  in
  (* Sylvester's construction, with rows sorted by the number of sign changes. *)
  let signs =
    List.init size ~f:(fun i ->
      List.init size ~f:(fun j -> Int.popcount (i land j) % 2 = 0))
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:sign_changes)
  in
  Vdom.Node.span
    ~attrs:[ Vdom.Attr.class_ "hadamard"; Vdom.Attr.create "aria-hidden" "true" ]
    (List.concat_mapi signs ~f:(fun row signs ->
       List.mapi signs ~f:(fun col positive ->
         Vdom.Node.span
           ~attrs:
             [ Vdom.Attr.classes
                 [ "hadamard-cell"
                 ; (match positive with
                    | true -> "positive"
                    | false -> "negative")
                 ]
               (* [Vdom.Attr.style] assigns properties one by one, which ignores custom
                  ones. *)
             ; Vdom.Attr.create "style" [%string "--row: %{row#Int}; --col: %{col#Int}"]
             ]
           [])))
;;

(* The cells' transition delays depend on the pointer, so the matrix is turned over only
   after the pointer has been recorded. Turning it with [:hover] would start the
   transitions with the previous position, since reading the layout here applies the
   pending hover style. *)
let turn_on_hover =
  let open Js_of_ocaml in
  let record_pointer ~(turned : bool option) (event : Dom_html.mouseEvent Js.t) =
    Js.Opt.iter event##.currentTarget (fun (host : Dom_html.element Js.t) ->
      Js.Opt.iter
        (host##querySelector (Js.string ".hadamard"))
        (fun grid ->
           let rect = grid##getBoundingClientRect in
           let set name ~position ~start ~end_ =
             (* Clamped, since a pointer that leaves quickly is reported far away *)
             let fraction =
               Float.clamp_exn ((position -. start) /. (end_ -. start)) ~min:0. ~max:1.
             in
             ignore
               (host##.style##setProperty
                  (Js.string name)
                  (Js.string (Float.to_string_hum ~decimals:3 fraction))
                  Js.undefined
                : Js.js_string Js.t)
           in
           set
             "--pointer-x"
             ~position:(Float.of_int event##.clientX)
             ~start:rect##.left
             ~end_:rect##.right;
           set
             "--pointer-y"
             ~position:(Float.of_int event##.clientY)
             ~start:rect##.top
             ~end_:rect##.bottom;
           match turned with
           | None -> ()
           | Some true -> host##setAttribute (Js.string "data-turned") (Js.string "")
           | Some false -> host##removeAttribute (Js.string "data-turned")))
  in
  Vdom.Attr.many
    [ Vdom.Attr.on_mouseenter (Effect.of_sync_fun (record_pointer ~turned:(Some true)))
    ; Vdom.Attr.on_mousemove (Effect.of_sync_fun (record_pointer ~turned:None))
    ; Vdom.Attr.on_mouseleave (Effect.of_sync_fun (record_pointer ~turned:(Some false)))
    ]
;;
