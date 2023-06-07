open Omd

let from_string = of_string

let rec from_inline = function
  | Link (_, link) -> [ link.destination ]
  | Concat (_, x) -> List.concat_map from_inline x
  | Emph (_, x) | Strong (_, x) -> from_inline x
  | _ -> []

let extract_links md =
  let rec loop = function
    | [] -> []
    | Paragraph (_, x) :: tl -> from_inline x @ loop tl
    | List (_, _, _, bl) :: tl -> List.concat_map loop bl @ loop tl
    | Blockquote (_, bl) :: tl -> loop bl @ loop tl
    | Heading (_, _, x) :: tl -> from_inline x @ loop tl
    | Definition_list (_, dl) :: tl ->
        List.concat_map
          (fun { term; defs } ->
            from_inline term @ List.concat_map from_inline defs)
          dl
        @ loop tl
    | Table (_, headers, rows) :: tl ->
        let header_links = List.concat_map from_inline (List.map fst headers) in
        let body_links = List.concat_map (List.concat_map from_inline) rows in
        (header_links @ body_links) @ loop tl
    | _ :: tl -> loop tl
  in
  loop md
