type t = string

let from_string = Fun.id
let link_delimiter = ""

let url_regexp =
  Re.compile
    (Re.Perl.re
       {|https?://(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b((\.[-a-zA-Z0-9()@:%_\+~#?&//=]+)|([-a-zA-Z0-9()@:%_\+~#?&//=]))*|})

let extract_links = Re.matches url_regexp

let replace_in_split (i, v) text =
  match (text, v) with
  | `Text x, _ -> ((i, v), x)
  | `Delim x, (n, new_link) :: vs ->
      let new_x, v = if i = n then (new_link, vs) else (Re.Group.get x 0, v) in
      ((i + 1, v), new_x)
  | `Delim x, [] -> ((i, v), Re.Group.get x 0)

let replace_links ?(start = 0) v text =
  let groups = Re.split_full url_regexp text in
  let loop p xs =
    let p', xs' =
      List.fold_left
        (fun (p, xs) x ->
          let p', x' = replace_in_split p x in
          (p', x' :: xs))
        (p, []) xs
    in
    (p', List.rev xs')
  in
  let p, replaced_list = loop (start, v) groups in
  (p, String.concat "" replaced_list)
