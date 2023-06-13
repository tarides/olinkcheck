open Omd

let from_string = of_string

let rec from_inline = function
  | Link (_, link) -> Plaintext.extract_links link.destination
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

let loop f p xs =
  let p', xs' =
    List.fold_left
      (fun (p, xs) x ->
        let p', x' = f p x in
        (p', x' :: xs))
      (p, []) xs
  in
  (p', List.rev xs')

let rec replace_in_inline (i, v) u =
  match (u, v) with
  | Link (a, { label; destination; title }), v ->
      let (i', v'), new_dest = Plaintext.replace_links ~start:i v destination in
      let new_link = Link (a, { label; destination = new_dest; title }) in
      ((i', v'), new_link)
  | Emph (a, x), v ->
      let (i', v'), x' = replace_in_inline (i, v) x in
      ((i', v'), Emph (a, x'))
  | Strong (a, x), v ->
      let (i', v'), x' = replace_in_inline (i, v) x in
      ((i', v'), Strong (a, x'))
  | Concat (a, xs), v ->
      let (i', v'), xs' = loop replace_in_inline (i, v) xs in
      ((i', v'), Concat (a, xs'))
  | el, _ -> ((i, v), el)

let rec replace_in_block p u =
  match (u, p) with
  | Paragraph (a, x), p ->
      let p', x' = replace_in_inline p x in
      (p', Paragraph (a, x'))
  | List (a, b, c, xss), p ->
      let block_list_loop p xss =
        let p', xss' =
          List.fold_left
            (fun (p, xss) xs ->
              let p', xs' = loop replace_in_block p xs in
              (p', [ xs' ] @ xss))
            (p, []) xss
        in
        (p', List.rev xss')
      in
      let p', xss' = block_list_loop p xss in
      (p', List (a, b, c, xss'))
  | Blockquote (a, xs), p ->
      let p', xs' = loop replace_in_block p xs in
      (p', Blockquote (a, xs'))
  | Heading (a, b, x), p ->
      let p', x' = replace_in_inline p x in
      (p', Heading (a, b, x'))
  | Definition_list (a, dl), p ->
      let def_list_loop p dl =
        let p', dl' =
          List.fold_left
            (fun (p, ds) { term; defs } ->
              let p', term' = replace_in_inline p term in
              let p'', ds' = loop replace_in_inline p' defs in
              (p'', { term = term'; defs = ds' } :: ds))
            (p, []) dl
        in
        (p', List.rev dl')
      in
      let p', dl' = def_list_loop p dl in
      (p', Definition_list (a, dl'))
  | Table (a, headers, rows), p ->
      let header_loop p hs =
        let p', hs' =
          List.fold_left
            (fun (p, hs) (hdr, al) ->
              let p', hdr' = replace_in_inline p hdr in
              (p', (hdr', al) :: hs))
            (p, []) hs
        in
        (p', List.rev hs')
      in
      let rows_loop p rows =
        let p', rows' =
          List.fold_left
            (fun (p, rows) row ->
              let p', row' = loop replace_in_inline p row in
              (p', [ row' ] @ rows))
            (p, []) rows
        in
        (p', List.rev rows')
      in
      let p', hs' = header_loop p headers in
      let p'', rows' = rows_loop p' rows in
      (p'', Table (a, hs', rows'))
  | el, p -> (p, el)

let replace_links v md = loop replace_in_block (0, v) md
