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

let rec replace_in_inline v i u =
  match (u, v) with
  | Link (a, { label; destination; title }), (n, new_dest) ->
      let new_link =
        if i = n then Link (a, { label; destination = new_dest; title })
        else Link (a, { label; destination; title })
      in
      (i + 1, new_link)
  | Emph (a, x), v ->
      let i', x' = replace_in_inline v i x in
      (i', Emph (a, x'))
  | Strong (a, x), v ->
      let i', x' = replace_in_inline v i x in
      (i', Strong (a, x'))
  | Concat (a, xs), v ->
      let rec loop i xs =
        match (i, xs) with
        | i, [] -> (i, [])
        | i, x :: xs ->
            let i', x' = replace_in_inline v i x in
            (i', x' :: snd (loop i' xs))
      in
      let i', xs' = loop i xs in
      (i', Concat (a, xs'))
  | el, _ -> (i, el)
