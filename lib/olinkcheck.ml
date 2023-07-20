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

  val annotate :
    ?verbose:bool ->
    ?exclude_prefixes:string list ->
    string ->
    string * int list
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

  let annotate ?(verbose = false) ?(exclude_prefixes = []) str =
    (*find the links and exclude based on the exclude list*)
    let links =
      str |> from_string |> extract_links
      |> exclude_patterns ~prefixes:exclude_prefixes
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

  let annotate ?(verbose = false) ?(exclude_prefixes = []) str =
    let parts = P.separate str in
    let annotated_parts, positions =
      List.fold_left
        (fun (asps, pos) sp ->
          let asp', pos' =
            match sp with
            | P.(P1_parsed x) ->
                let c, d = P.P1.annotate ~verbose ~exclude_prefixes x in
                (P.(P1_parsed c), d)
            | P.(P2_parsed x) ->
                let c, d = P.P2.annotate ~verbose ~exclude_prefixes x in
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

module YamlMdPair : ParserPair = struct
  (* Markdown with a YAML Header. *)
  module P1 = Yaml
  module P2 = Markdown

  type either_parser = P1_parsed of string | P2_parsed of string

  let separate str =
    (* The first occurence of \n---\n marks the end of YAML, and the beginning of Markdown.
     * Subsequent occurences are a part of the Markdown. *)
    let delim_regex = Re.Pcre.re "\n---\n" |> Re.compile in
    let splits = Re.split_full delim_regex str in
    let yaml, md =
      let rec loop (y, m) c = function
        | [] -> (y, m)
        | `Text x :: tl ->
            if c then loop (y, m ^ x) c tl else loop (y ^ x, m) c tl
        | `Delim x :: tl ->
            let a = Re.Group.get x 0 in
            loop (y, m ^ a) true tl
      in
      loop ("", "") false splits
    in
    (* The YAML header may have markdown in the followings fields:
       changelog: |
       intro: >
       intro: |
       highlights: >
       highlights: | *)
    let md_field_names_regex =
      Re.Pcre.re
        "(\n\
         changelog: \\|\n\
         )|(\n\
         intro: >\n\
         )|(\n\
         intro : \\|\n\
         )|(\n\
         highlights: >\n\
         )|(\n\
         highlights: \\|\n\
         )"
      |> Re.compile
    in
    let yaml_splits = Re.split_full md_field_names_regex yaml in
    let header, _ =
      List.fold_left
        (fun (hs, md_text) y ->
          let h, nxt =
            match (y, md_text) with
            | `Text x, true -> (P2_parsed x, false)
            | `Text x, false -> (P1_parsed x, false)
            | `Delim x, _ -> (P1_parsed (Re.Group.get x 0), true)
          in
          (h :: hs, nxt))
        ([], false) yaml_splits
    in
    List.rev header @ [ P2_parsed md ]

  let join strs =
    List.fold_left
      (fun acc str ->
        let x = match str with P1_parsed x -> x | P2_parsed x -> x in
        acc ^ x)
      "" strs
end

module YamlMd = MakePairParser (YamlMdPair)

module YamlHtmlPair : ParserPair = struct
  module P1 = YamlMd
  module P2 = Html

  type either_parser = P1_parsed of string | P2_parsed of string

  let separate str =
    (* The first occurence of \n---\n marks the end of YAML, and the beginning of HTML. *)
    let delim_regex = "\n---\n" |> Re.Pcre.re |> Re.compile in
    let splits = Re.split_full delim_regex str in
    let yaml, html =
      let rec loop (y, m) c = function
        | [] -> (y, m)
        | `Text x :: tl ->
            if c then loop (y, m ^ x) c tl else loop (y ^ x, m) c tl
        | `Delim x :: tl ->
            let a = Re.Group.get x 0 in
            loop (y, m ^ a) true tl
      in
      loop ("", "") false splits
    in
    [ P1_parsed yaml; P2_parsed html ]

  let join strs =
    List.fold_left
      (fun acc str ->
        let x = match str with P1_parsed x -> x | P2_parsed x -> x in
        acc ^ x)
      "" strs
end

module YamlHtml = MakePairParser (YamlHtmlPair)
