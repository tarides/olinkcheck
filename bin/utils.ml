let file_contents file = In_channel.(with_open_bin file In_channel.input_all)

let pretty_print_link_status (link, (status_code, status_string)) =
  Printf.printf "%s - %i %s\n" link status_code status_string

let rec files_with_ext ext file =
  if Sys.file_exists file then (
    try
      if Sys.is_directory file then
        let files =
          file |> Sys.readdir |> Array.to_list
          |> List.map (Filename.concat file)
        in
        let files_in_dir =
          List.filter
            (fun file ->
              (not (Sys.is_directory file)) && Filename.extension file = ext)
            files
        in
        let files_in_child_dirs =
          List.concat_map (files_with_ext ext)
            (List.filter Sys.is_directory files)
        in
        files_in_dir @ files_in_child_dirs
      else if Filename.extension file = ext then [ file ]
      else []
    with Sys_error x ->
      print_endline (x ^ ", skipping.");
      [])
  else []

let pretty_print_link_status_from_file ext from_string extract_links file =
  file |> files_with_ext ext
  |> List.iter (fun file ->
         print_endline file;
         try
           file |> file_contents |> from_string |> extract_links
           |> List.iter (fun link ->
                  (link, Olinkcheck.Link.status link)
                  |> pretty_print_link_status)
         with Sys_error x -> print_endline (x ^ ", skipping."))
