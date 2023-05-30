val extract_links : string -> string list

module Link : sig
  val status : string -> int * string
  val request : string -> Hyper.status Lwt.t
  val client : ('a -> [< Hyper.status ]) -> 'a -> int * string
end
