val extract_links : string -> string list
val link_status : string -> int * string
val link_status_request : string -> Hyper.status Lwt.t
val link_status_client : ('a -> [< Hyper.status ]) -> 'a -> int * string
