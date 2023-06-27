module type ParserType = sig
  type t

  val from_string : string -> t
  val link_delimiter : string
  val extract_links : t -> string list

  val replace_links :
    ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t
end

include Utils

module Parser (P : ParserType) = struct
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

  let annotate_in_str verbose exclude_list str =
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

module Plaintext = Parser (Plaintext)
module Markdown = Parser (Markdown)
module Sexp = Parser (Sexp)
module Yaml = Parser (Yml)
module Link = Link
