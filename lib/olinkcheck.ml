module type ParserType = sig
  type t

  val from_string : string -> t
  val extract_links : t -> string list

  val replace_links :
    ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t
end

module Parser (P : ParserType) = struct
  type t = P.t

  let from_string = P.from_string
  let extract_links = P.extract_links
  let replace_links = P.replace_links
end

module Plaintext = Parser (Plaintext)
module Markdown = Parser (Markdown)
module Sexp = Parser (Sexp)
module Link = Link
module Yaml = Yml
