val from_string : string -> Sexplib0.Sexp.t list
val extract_links : Sexplib0.Sexp.t list -> string list

val replace_links :
  ?start:int ->
  (int * string) list ->
  Sexplib0.Sexp.t list ->
  (int * (int * string) list) * Sexplib0.Sexp.t list
