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

let rec loop f v i xs =
  match (i, xs) with
  | i, [] -> (i, [])
  | i, x :: xs ->
      let i', x' = f v i x in
      (i', x' :: snd (loop f v i' xs))

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
      let i', xs' = loop replace_in_inline v i xs in
      (i', Concat (a, xs'))
  | el, _ -> (i, el)

let rec replace_in_block v i u =
  match (u, v) with
  | Paragraph (a, x), v ->
      let i', x' = replace_in_inline v i x in
      (i', Paragraph (a, x'))
  | List (a, b, c, xss), v ->
      let rec loop1 i xss =
        match (i, xss) with
        | i, [] -> (i, [])
        | i, xs :: xss ->
            let i', xs' = loop replace_in_block v i xs in
            (i', [ xs' ] @ snd (loop1 i' xss))
      in
      let i'', xss' = loop1 i xss in
      (i'', List (a, b, c, xss'))
  | Blockquote (a, xs), v ->
      let i', xs' = loop replace_in_block v i xs in
      (i', Blockquote (a, xs'))
  | Heading (a, b, x), v ->
      let i', x' = replace_in_inline v i x in
      (i', Heading (a, b, x'))
  | Definition_list (a, dl), v ->
      let rec loop1 i dl =
        match (i, dl) with
        | i, [] -> (i, [])
        | i, { term; defs } :: dl ->
            let i', term' = replace_in_inline v i term in
            let i'', ds' = loop replace_in_inline v i' defs in
            (i'', { term = term'; defs = ds' } :: snd (loop1 i dl))
      in
      let i', dl' = loop1 i dl in
      (i', Definition_list (a, dl'))
  | Table (a, headers, rows), v ->
      let rec header_loop i hs =
        match (i, hs) with
        | i, [] -> (i, [])
        | i, (hdr, al) :: hs ->
            let i', hdr' = replace_in_inline v i hdr in
            (i', (hdr', al) :: snd (header_loop i' hs))
      in
      let rec rows_loop i rows =
        match (i, rows) with
        | i, [] -> (i, [])
        | i, row :: rows ->
            let i', row' = loop replace_in_inline v i row in
            (i', [ row' ] @ snd (rows_loop i' rows))
      in
      let i', hs' = header_loop i headers in
      let i'', rows' = rows_loop i' rows in
      (i'', Table (a, hs', rows'))
  | el, _ -> (i, el)

let replace_in_doc = loop replace_in_block
