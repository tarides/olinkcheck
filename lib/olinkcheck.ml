module type BasicParser = sig
  type t

  val from_string : string -> t
  val link_delimiter : string
  val extract_links : t -> string list

  val replace_links :
    ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t
end

include Utils

module type Parser = sig
  type t

  val from_string : string -> t
  val extract_links : t -> string list

  val replace_links :
    ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t

  val annotate : bool -> string list -> string -> string * int list
end

module MakeParser (P : BasicParser) : Parser = struct
  type t = P.t

  let from_string = P.from_string
  let link_delimiter = P.link_delimiter
  let extract_links = P.extract_links
  let replace_links = P.replace_links

  let match_positions regexp str =
    let rec aux split_list pos_list cur_pos =
      match split_list with
      | [] -> List.rev pos_list
      | `Text x :: tl -> aux tl pos_list (String.length x + cur_pos)
      | `Delim x :: tl ->
          aux tl (cur_pos :: pos_list)
            (String.length (Re.Group.get x 0) + cur_pos)
    in
    aux (Re.split_full regexp str) [] 0

  let annotate verbose exclude_list str =
    (*find the links and exclude based on the exclude list*)
    let links =
      str |> from_string |> extract_links |> exclude_patterns exclude_list
    in
    (*get their status*)
    let status = Link.status_many links in
    List.combine status links
    (*remove 200 OK links if needed*)
    |> (if not verbose then List.filter (fun ((code, _), _) -> code <> 200)
        else Fun.id)
    (*sort by decreasing order of length of strings*)
    |> List.sort (fun ((_, _), linka) ((_, _), linkb) ->
           String.length linkb - String.length linka)
    |> List.fold_left
         (fun (str, replaced) ((code, reason), link) ->
           let find_regexp =
             link ^ link_delimiter |> Re.Pcre.quote |> Re.Pcre.re |> Re.compile
           in
           (*get the positions where this url has matched*)
           let match_positions = match_positions find_regexp str in
           (*remove the positions which have already been matched by a longer url*)
           let to_replace =
             match_positions
             |> List.filter (fun x -> not (List.mem x replaced))
             |> List.sort compare
           in
           (*replace in the remaining positions and get the modified positions*)
           let annotated_str, matched_now, matched_already, _ =
             List.fold_left
               (fun (str, now, already, acc) rpos ->
                 let suffix =
                   " - [" ^ string_of_int code ^ " " ^ reason ^ "]"
                 in
                 let replace_by = link ^ suffix ^ link_delimiter in
                 let start_pos = acc + rpos in
                 let end_pos =
                   start_pos + String.length (link ^ link_delimiter)
                 in
                 let before = String.sub str 0 start_pos in
                 let after =
                   String.sub str end_pos (String.length str - end_pos)
                 in
                 ( before ^ replace_by ^ after,
                   start_pos :: now,
                   List.map
                     (fun x -> if x > rpos then x + String.length suffix else x)
                     already,
                   acc + String.length suffix ))
               (str, [], replaced, 0) to_replace
           in
           (annotated_str, List.sort compare (matched_now @ matched_already)))
         (str, [])
end

module type ParserPair = sig
  module P1 : Parser
  module P2 : Parser

  type either_parser = P1_parsed of string | P2_parsed of string

  val separate : string -> either_parser list
  val join : either_parser list -> string
end

module MakePairParser (P : ParserPair) : Parser = struct
  type ast = T1 of P.P1.t | T2 of P.P2.t
  type t = ast list

  let from_string str =
    let parts = P.separate str in
    let rec loop parts =
      match parts with
      | [] -> []
      | P.(P1_parsed x) :: tl -> T1 (P.P1.from_string x) :: loop tl
      | P.(P2_parsed x) :: tl -> T2 (P.P2.from_string x) :: loop tl
    in
    loop parts

  let extract_links ts =
    let rec loop ts =
      match ts with
      | [] -> []
      | T1 t :: tl -> P.P1.extract_links t @ loop tl
      | T2 t :: tl -> P.P2.extract_links t @ loop tl
    in
    loop ts

  let replace_links ?(start = 0) v ts =
    let p', ts' =
      List.fold_left
        (fun ((start, v), ts) t ->
          let (start', v'), t' =
            match t with
            | T1 x ->
                let c, d = P.P1.replace_links ~start v x in
                (c, T1 d)
            | T2 x ->
                let c, d = P.P2.replace_links ~start v x in
                (c, T2 d)
          in
          ((start', v'), t' :: ts))
        ((start, v), [])
        ts
    in
    (p', List.rev ts')

  let annotate verbose exclude_list str =
    let parts = P.separate str in
    let annotated_parts, positions =
      List.fold_left
        (fun (asps, pos) sp ->
          let asp', pos' =
            match sp with
            | P.(P1_parsed x) ->
                let c, d = P.P1.annotate verbose exclude_list x in
                (P.(P1_parsed c), d)
            | P.(P2_parsed x) ->
                let c, d = P.P2.annotate verbose exclude_list x in
                (P.(P2_parsed c), d)
          in
          (asp' :: asps, pos' @ pos))
        ([], []) parts
    in
    (P.join (List.rev annotated_parts), positions)
end

module Plaintext = MakeParser (Plaintext)
module Markdown = MakeParser (Markdown)
module Sexp = MakeParser (Sexp)
module Yaml = MakeParser (Yml)
module Html = MakeParser (Html)
module Link = Link
