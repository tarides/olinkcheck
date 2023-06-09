val from_string : string -> Omd.doc
val extract_links : 'a Omd.block list -> string list

val replace_in_doc :
  (int * string) list ->
  'a Omd.block list ->
  (int * (int * string) list) * 'a Omd.block list
