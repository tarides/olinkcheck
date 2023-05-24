open Omd

let extract_links text =
  let md = of_string text in
  let rec loop = function
    | [] -> []
    | Url (href, s, _) :: tl ->
        let lst = href :: loop s in
        lst @ loop tl
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
        let lst = List.concat_map loop l in
        lst @ loop tl
    | Html (_, _, body) :: tl ->
        let lst = loop body in
        lst @ loop tl
    | Html_block (_, _, body) :: tl ->
        let lst = loop body in
        lst @ loop tl
    | _ :: tl -> loop tl
  in
  loop md
