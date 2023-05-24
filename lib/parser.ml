open Omd

let extract_links text =
  let md = of_string text in
  let rec loop = function
    | [] -> []
    | Url (href, s, _) :: tl ->
        let lst = href :: loop s in
        lst @ loop tl
    | X _ :: tl
    | Ref _ :: tl
    | Img_ref _ :: tl
    | Img _ :: tl
    | Text _ :: tl
    | Raw _ :: tl
    | Raw_block _ :: tl
    | Code_block _ :: tl
    | Code _ :: tl
    | Html_comment _ :: tl
    | Br :: tl
    | Hr :: tl
    | NL :: tl ->
        loop tl
    | Blockquote hd :: tl
    | Paragraph hd :: tl
    | Emph hd :: tl
    | Bold hd :: tl
    | H1 hd :: tl
    | H2 hd :: tl
    | H3 hd :: tl
    | H4 hd :: tl
    | H5 hd :: tl
    | H6 hd :: tl ->
        let lst = loop hd in
        lst @ loop tl
    | (Ul l | Ol l) :: tl | (Ulp l | Olp l) :: tl ->
        let lst = List.map loop l in
        List.flatten lst @ loop tl
    | Html (_, _, body) :: tl ->
        let lst = loop body in
        lst @ loop tl
    | Html_block (_, _, body) :: tl ->
        let lst = loop body in
        lst @ loop tl
  in
  loop md
