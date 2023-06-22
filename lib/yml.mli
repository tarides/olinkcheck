type t = Yaml.value

val from_string : string -> t
val link_delimiter : string
val extract_links : t -> string list

val replace_links :
  ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t
