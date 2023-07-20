open Olinkcheck

let pp_link_status ?(verbose = false) (link, (status_code, status_string)) =
  if (not (verbose || status_code = 200)) || verbose then
    Printf.printf "%s - %i %s\n" link status_code status_string
  else ()

let with_ext ext = List.filter (fun file -> Filename.extension file = ext)

let rec files_with_ext ~ext file =
  if Sys.is_directory file then
    try
      file |> Sys.readdir |> Array.to_list
      |> List.map (Filename.concat file)
      |> List.partition Sys.is_directory
      |> fun (dirs, non_dirs) ->
      with_ext ext non_dirs @ List.concat @@ List.map (files_with_ext ~ext) dirs
    with Sys_error _ -> []
  else if Filename.extension file = ext then [ file ]
  else []

let read_exclude_prefixes file =
  match file with
  | Some file ->
      read_bin file |> String.split_on_char '\n'
      |> List.filter (fun pat -> pat <> "")
  | None -> []

let pp_link_status_file (type p) (module M : Parser with type t = p)
    ?(verbose = false) ~exclude_file ~ext file =
  let exclude_prefixes = read_exclude_prefixes exclude_file in
  file |> files_with_ext ~ext
  |> List.iter (fun file ->
         try
           file |> read_bin |> M.from_string |> M.extract_links
           |> exclude_patterns ~prefixes:exclude_prefixes
           |> (fun links ->
                print_endline file;
                let statuses = Link.status_many links in
                List.combine links statuses)
           |> List.iter (pp_link_status ~verbose)
         with Sys_error _ -> ())

let annotate_in_file (type p) (module M : Parser with type t = p)
    ?(verbose = false) ~exclude_file ~ext file =
  let exclude_prefixes = read_exclude_prefixes exclude_file in
  file |> files_with_ext ~ext
  |> List.iter (fun file ->
         try
           file |> read_bin |> M.(annotate ~verbose ~exclude_prefixes) |> fst
           |> fun new_str ->
           let out_chan = open_out file in
           Printf.fprintf out_chan "%s" new_str;
           close_out out_chan
         with Sys_error _ -> ())
