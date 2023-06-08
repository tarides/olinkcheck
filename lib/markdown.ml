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

let loop f i v xs =
  let rec aux f i v xs xs' =
    match xs with
    | [] -> (i, [], v, xs')
    | x :: xs ->
        let i', x', v' = f i v x in
        aux f i' v' xs (x' :: xs')
  in
  match aux f i v xs [] with i, _, v', xs' -> (i, List.rev xs', v')

let rec replace_in_inline i v u =
  match (u, v) with
  | Link (a, { label; destination; title }), (n, new_dest) :: vs ->
      let new_link, v =
        if i = n then (Link (a, { label; destination = new_dest; title }), vs)
        else (Link (a, { label; destination; title }), v)
      in
      (i + 1, new_link, v)
  | Emph (a, x), v ->
      let i', x', v' = replace_in_inline i v x in
      (i', Emph (a, x'), v')
  | Strong (a, x), v ->
      let i', x', v' = replace_in_inline i v x in
      (i', Strong (a, x'), v')
  | Concat (a, xs), v ->
      let i', xs', v' = loop replace_in_inline i v xs in
      (i', Concat (a, xs'), v')
  | el, _ -> (i, el, v)

let rec replace_in_block i v u =
  match (u, v) with
  | Paragraph (a, x), v ->
      let i', x', v' = replace_in_inline i v x in
      (i', Paragraph (a, x'), v')
  | List (a, b, c, xss), v ->
      let rec loop1 i xss v =
        match xss with
        | [] -> (i, [], v)
        | xs :: xss ->
            let i', xs', v' = loop replace_in_block i v xs in
            let i'', xss', v'' = loop1 i' xss v' in
            (i'', [ xs' ] @ xss', v'')
      in
      let i'', xss', v' = loop1 i xss v in
      (i'', List (a, b, c, xss'), v')
  | Blockquote (a, xs), v ->
      let i', xs', v' = loop replace_in_block i v xs in
      (i', Blockquote (a, xs'), v')
  | Heading (a, b, x), v ->
      let i', x', v' = replace_in_inline i v x in
      (i', Heading (a, b, x'), v')
  | Definition_list (a, dl), v ->
      let rec loop1 i dl v =
        match dl with
        | [] -> (i, [], v)
        | { term; defs } :: dl ->
            let i', term', v' = replace_in_inline i v term in
            let i'', ds', v'' = loop replace_in_inline i' v' defs in
            let i''', dl', v''' = loop1 i'' dl v'' in
            (i''', { term = term'; defs = ds' } :: dl', v''')
      in
      let i', dl', v' = loop1 i dl v in
      (i', Definition_list (a, dl'), v')
  | Table (a, headers, rows), v ->
      let rec header_loop i hs v =
        match hs with
        | [] -> (i, [], v)
        | (hdr, al) :: hs ->
            let i', hdr', v' = replace_in_inline i v hdr in
            let i'', hs', v'' = header_loop i' hs v' in
            (i'', (hdr', al) :: hs', v'')
      in
      let rec rows_loop i rows v =
        match rows with
        | [] -> (i, [], v)
        | row :: rows ->
            let i', row', v' = loop replace_in_inline i v row in
            let i'', rows', v'' = rows_loop i' rows v' in
            (i'', [ row' ] @ rows', v'')
      in
      let i', hs', v' = header_loop i headers v in
      let i'', rows', v'' = rows_loop i' rows v' in
      (i'', Table (a, hs', rows'), v'')
  | el, v -> (i, el, v)

let replace_in_doc v md = loop replace_in_block 0 v md
