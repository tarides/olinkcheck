module type ParserType = sig
  type t

  val from_string : string -> t
  val link_delimiter : string
  val extract_links : t -> string list

  val replace_links :
    ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t
end

module Parser (P : ParserType) = struct
  type t = P.t

  let from_string = P.from_string
  let link_delimiter = P.link_delimiter
  let extract_links = P.extract_links
  let replace_links = P.replace_links

  let annotate_in_str str =
    let links = str |> from_string |> extract_links in
    let status = Link.status_many links in
    List.combine status links
    |> List.filter (fun ((code, _), _) -> code <> 200)
    |> List.fold_left
         (fun str ((code, reason), link) ->
           let replace_by =
             link ^ " - " ^ string_of_int code ^ " " ^ reason
             |> Fun.flip ( ^ ) link_delimiter
           in
           let to_replace =
             link
             |> Fun.flip ( ^ ) link_delimiter
             |> Re.Pcre.quote |> Re.Pcre.re |> Re.compile
           in
           Re.replace_string ~all:true to_replace ~by:replace_by str)
         str
end

module Plaintext = Parser (Plaintext)
module Markdown = Parser (Markdown)
module Sexp = Parser (Sexp)
module Yaml = Parser (Yml)
module Link = Link
