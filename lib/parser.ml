open Omd

let extract_links text =
  let md = of_string text in
  let rec loop = function
    | [] -> []
    | Url (href, s, _) :: tl ->
        let lst = href :: loop s in
        lst @ loop tl
    | ( Blockquote hd
      | Paragraph hd
      | Emph hd
      | Bold hd
      | H1 hd
      | H2 hd
      | H3 hd
      | H4 hd
      | H5 hd
      | H6 hd )
      :: tl ->
        let lst = loop hd in
        lst @ loop tl
    | (Ul l | Ol l | Ulp l | Olp l) :: tl ->
        let lst = List.concat_map loop l in
        lst @ loop tl
    | (Html (_, _, body) | Html_block (_, _, body)) :: tl ->
        let lst = loop body in
        lst @ loop tl
    | _ :: tl -> loop tl
  in
  loop md
