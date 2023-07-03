open Soup

type t = soup node

let from_string = parse
let link_delimiter = ""

let extract_links html =
  html |> select "a[href]" |> to_list
  |> List.rev_map (fun a -> R.attribute "href" a)
  |> List.rev_map Plaintext.extract_links
  |> List.concat

let replace_links ?(start = 0) vs html =
  html |> select "a[href]"
  |> fold
       (fun ((i, v), html) a ->
         let (i', v'), new_href =
           Plaintext.replace_links ~start:i v (R.attribute "href" a)
         in
         let () = set_attribute "href" new_href a in
         ((i', v'), html))
       ((start, vs), html)
