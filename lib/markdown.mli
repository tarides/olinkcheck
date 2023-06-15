type t = Omd.attributes Omd.block list

val from_string : string -> t
val extract_links : t -> string list

val replace_links :
  ?start:int -> (int * string) list -> t -> (int * (int * string) list) * t
