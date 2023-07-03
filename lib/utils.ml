(** Adapted from {{: https://github.com/ocaml/ocaml/blob/5.1/stdlib/string.ml#L201 } OCaml 5.1 String}*)
let starts_with ~prefix s =
  let len_s = String.length s and len_pre = String.length prefix in
  let rec aux i =
    if i = len_pre then true
    else if String.get s i <> String.get prefix i then false
    else aux (i + 1)
  in
  len_s >= len_pre && aux 0

let exclude_patterns exclude_list links =
  links
  |> List.filter (fun link ->
         List.fold_left
           (fun ok prefix -> ok && not (starts_with ~prefix link))
           true exclude_list)

(** Adapted from {{: https://github.com/ocaml/ocaml/blob/5.1/stdlib/in_channel.ml#L83 } Ocaml 5.1 In_channel}*)
let read_upto ic buf ofs len =
  let rec loop ofs len =
    if len = 0 then ofs
    else
      let r = Stdlib.input ic buf ofs len in
      if r = 0 then ofs else loop (ofs + r) (len - r)
  in
  loop ofs len - ofs

let ensure buf ofs n =
  let len = Bytes.length buf in
  if len >= ofs + n then buf
  else
    let new_len = ref len in
    while !new_len < ofs + n do
      new_len := (2 * !new_len) + 1
    done;
    let new_len = !new_len in
    let new_len =
      if new_len <= Sys.max_string_length then new_len
      else if ofs < Sys.max_string_length then Sys.max_string_length
      else
        failwith "read_bin: file content is larger than maximum string length"
    in
    let new_buf = Bytes.create new_len in
    Bytes.blit buf 0 new_buf 0 ofs;
    new_buf

let read_bin f =
  let ic = Stdlib.open_in_bin f in
  let chunk_size = 65536 in
  (* IO_BUFFER_SIZE *)
  let initial_size =
    try Stdlib.in_channel_length ic - Stdlib.pos_in ic with Sys_error _ -> -1
  in
  let initial_size = if initial_size < 0 then chunk_size else initial_size in
  let initial_size =
    if initial_size <= Sys.max_string_length then initial_size
    else Sys.max_string_length
  in
  let buf = Bytes.create initial_size in
  let nread = read_upto ic buf 0 initial_size in
  let contents =
    if nread < initial_size then
      (* EOF reached, buffer partially filled *)
      Bytes.sub_string buf 0 nread
    else
      (* nread = initial_size, maybe EOF reached *)
      match Stdlib.input_char ic with
      | exception End_of_file ->
          (* EOF reached, buffer is completely filled *)
          Bytes.unsafe_to_string buf
      | c ->
          (* EOF not reached *)
          let rec loop buf ofs =
            let buf = ensure buf ofs chunk_size in
            let rem = Bytes.length buf - ofs in
            (* [rem] can be < [chunk_size] if buffer size close to
               [Sys.max_string_length] *)
            let r = read_upto ic buf ofs rem in
            if r < rem then (* EOF reached *)
              Bytes.sub_string buf 0 (ofs + r)
            else (* r = rem *)
              loop buf (ofs + rem)
          in
          let buf = ensure buf nread (chunk_size + 1) in
          Bytes.set buf nread c;
          loop buf (nread + 1)
  in
  Stdlib.close_in_noerr ic;
  contents
