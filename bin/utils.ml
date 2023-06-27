let file_contents file = In_channel.(with_open_bin file input_all)

let pretty_print_link_status verbose (link, (status_code, status_string)) =
  if (not (verbose || status_code = 200)) || verbose then
    Printf.printf "%s - %i %s\n" link status_code status_string
  else ()

let with_ext ext = List.filter (fun file -> Filename.extension file = ext)

let rec files_with_ext ext file =
  if Sys.is_directory file then
    try
      file |> Sys.readdir |> Array.to_list
      |> List.map (Filename.concat file)
      |> List.partition Sys.is_directory
      |> fun (dirs, non_dirs) ->
      with_ext ext non_dirs @ List.concat_map (files_with_ext ext) dirs
    with Sys_error _ -> []
  else if Filename.extension file = ext then [ file ]
  else []

let read_exclude_list file =
  match file with
  | Some file -> file_contents file |> String.split_on_char '\n'
  | None -> []

let pretty_print_link_status_from_file verbose exclude_list ext from_string
    extract_links file =
  let _exclude_list = read_exclude_list exclude_list in
  file |> files_with_ext ext
  |> List.iter (fun file ->
         try
           file |> file_contents |> from_string |> extract_links
           |> (fun links ->
                print_endline file;
                let statuses = Olinkcheck.Link.status_many links in
                List.combine links statuses)
           |> List.iter (pretty_print_link_status verbose)
         with Sys_error _ -> ())

let annotate_in_file verbose exclude_list ext annotate_in_str file =
  let exclude_list = read_exclude_list exclude_list in
  file |> files_with_ext ext
  |> List.iter (fun file ->
         try
           file |> file_contents |> annotate_in_str verbose exclude_list |> fst
           |> fun new_str ->
           let out_chan = open_out file in
           Printf.fprintf out_chan "%s" new_str;
           close_out out_chan
         with Sys_error _ -> ())
