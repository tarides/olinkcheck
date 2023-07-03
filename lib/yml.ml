open Yaml

type t = value

let from_string = of_string_exn
let link_delimiter = ""

let extract_links yaml =
  let rec loop = function
    | `String x -> Plaintext.extract_links x
    | `A x -> List.concat @@ List.map loop x
    | `O svl ->
        List.concat
        @@ List.map (fun (k, v) -> Plaintext.extract_links k @ loop v) svl
    | _ -> []
  in
  loop yaml

let rec replace_links ?(start = 0) v yaml =
  match yaml with
  | `String x ->
      let p', x' = Plaintext.replace_links ~start v x in
      (p', `String x')
  | `A x ->
      let p', x' = loop (start, v) x in
      (p', `A (List.rev x'))
  | `O svl ->
      let p', svl' =
        List.fold_left
          (fun ((i, v), svl) (k, u) ->
            let (i', v'), k' = Plaintext.replace_links ~start:i v k in
            let p', u' = replace_links ~start:i' v' u in
            (p', (k', u') :: svl))
          ((start, v), [])
          svl
      in
      (p', `O (List.rev svl'))
  | el -> ((start, v), el)

and loop p xs =
  List.fold_left
    (fun ((i, v), xs) x ->
      let p', x' = replace_links ~start:i v x in
      (p', x' :: xs))
    (p, []) xs
