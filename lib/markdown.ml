open Omd

let from_string = of_string

let extract_links md =
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
      | H6 hd
      | Html (_, _, hd)
      | Html_block (_, _, hd) )
      :: tl ->
        let lst = loop hd in
        lst @ loop tl
    | (Ul l | Ol l | Ulp l | Olp l) :: tl ->
        let lst = List.concat_map loop l in
        lst @ loop tl
    | _ :: tl -> loop tl
  in
  loop md

let fix_links md =
  let links_status =
    md |> extract_links |> List.map (fun link -> (link, Link.status link))
  in
  let fix id new_url =
    let track_count =
      let cnt = ref (-1) in
      fun () ->
        incr cnt;
        !cnt
    in
    Omd_representation.visit (function
      | Url (a, b, c) ->
          let count = track_count () in
          if id = count then Some [ Url (new_url, b, c) ]
          else Some [ Url (a, b, c) ]
      | _ -> None)
  in
  (links_status, fix)
