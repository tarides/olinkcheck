val from_string : string -> string
val extract_links : string -> string list

val replace_links :
  ?start:int ->
  (int * string) list ->
  string ->
  (int * (int * string) list) * string
