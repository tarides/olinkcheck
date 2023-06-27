open Sexplib

type t = Type.t list

let from_string text =
  match Sexp.of_string_many text with sexp -> sexp | exception e -> raise e

let link_delimiter = ""

let extract_links =
  let rec loop = function
    | [] -> []
    | Sexp.Atom x :: tl -> Plaintext.extract_links x @ loop tl
    | Sexp.List x :: tl -> loop x @ loop tl
  in
  loop

let rec replace_in_sexp ?(start = 0) v sexp =
  match sexp with
  | Sexp.Atom x ->
      let p', x' = Plaintext.replace_links ~start v x in
      (p', Sexp.Atom x')
  | Sexp.List xs ->
      let p', xs' = loop (start, v) xs in
      (p', Sexp.List (List.rev xs'))

and loop p xs =
  List.fold_left
    (fun ((i, v), xs) x ->
      let p', x' = replace_in_sexp ~start:i v x in
      (p', x' :: xs))
    (p, []) xs

let replace_links ?(start = 0) v sexp_list =
  let p', xs' = loop (start, v) sexp_list in
  (p', List.rev xs')
