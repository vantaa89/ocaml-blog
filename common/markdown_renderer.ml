open! Core

module Heading = struct
  type t =
    { level : int
    ; id : string
    ; text : string
    }
end

let headings_of_doc doc =
  let heading_id h =
    match Cmarkit.Block.Heading.id h with
    | Some (`Auto id | `Id id) -> id
    | None -> Cmarkit.Inline.id (Cmarkit.Block.Heading.inline h)
  in
  let heading_text h =
    Cmarkit.Inline.to_plain_text ~break_on_soft:false (Cmarkit.Block.Heading.inline h)
    |> List.map ~f:String.concat
    |> String.concat ~sep:" "
  in
  let block _mapper acc = function
    | Cmarkit.Block.Heading (h, _) ->
      let level = Cmarkit.Block.Heading.level h in
      let id = heading_id h in
      let text = heading_text h in
      Cmarkit.Folder.ret (({ level; id; text } : Heading.t) :: acc)
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block () in
  List.rev (Cmarkit.Folder.fold_doc folder [] doc)
;;

(* Renders a flat heading list as a nested [<ul>] table of contents. *)
let toc_html_of_headings (headings : Heading.t list) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "<div class=\"toc\">\n";
  let base_level =
    match headings with
    | [] -> 1
    | { level; _ } :: _ -> level
  in
  let repeat n str =
    List.init n ~f:Fn.id |> List.iter ~f:(fun _i -> Buffer.add_string buf str)
  in
  Buffer.add_string buf "<ul>\n";
  let final_level =
    List.fold headings ~init:base_level ~f:(fun current { level; id; text } ->
      (match level > current with
       | true -> repeat (level - current) "<ul>\n"
       | false -> repeat (current - level) "</ul>\n");
      Buffer.add_string buf [%string "<li><a href=\"#%{id}\">%{text}</a></li>\n"];
      level)
  in
  repeat (final_level - base_level + 1) "</ul>\n";
  Buffer.add_string buf "</div>";
  Buffer.contents buf
;;

let render ~markdown =
  (* [~strict:false] turns on the extensions: footnotes and $...$ math. *)
  let doc = Cmarkit.Doc.of_string ~strict:false ~heading_auto_ids:true markdown in
  let headings = headings_of_doc doc in
  (* inline HTML is trusted *)
  let html = Cmarkit_html.of_doc ~safe:false doc in
  match headings with
  | [] -> html
  | _ :: _ ->
    String.substr_replace_first
      html
      ~pattern:"<p>[TOC]</p>"
      ~with_:(toc_html_of_headings headings)
;;
