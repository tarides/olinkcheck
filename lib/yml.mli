val from_string : string -> Yaml.value
val extract_links : Yaml.value -> string list

val replace_links :
  ?start:int ->
  (int * string) list ->
  Yaml.value ->
  (int * (int * string) list) * Yaml.value
