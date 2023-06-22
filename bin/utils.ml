let file_contents file = In_channel.(with_open_bin file In_channel.input_all)

let pretty_print_link_status (link, (status_code, status_string)) =
  Printf.printf "%s - %i %s\n" link status_code status_string

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

let pretty_print_link_status_from_file ext from_string extract_links file =
  let start_time = ref (Unix.time ()) in
  file |> files_with_ext ext
  |> List.iter (fun file ->
         try
           file |> file_contents |> from_string |> extract_links
           |> (fun links ->
                print_endline file;
                let statuses = Olinkcheck.Link.status_many links in
                List.combine links statuses)
           |> List.iter pretty_print_link_status;
           let end_time = Unix.time () in
           if end_time -. !start_time >= 600. then Unix.sleep 60;
           start_time := Unix.time ()
         with Sys_error _ -> ())

let annotate_in_file ext annotate_in_str file =
  let start_time = ref (Unix.time ()) in
  file |> files_with_ext ext
  |> List.iter (fun file ->
         try
           file |> file_contents |> annotate_in_str |> fun new_str ->
           let out_chan = open_out file in
           Printf.fprintf out_chan "%s" new_str;
           close_out out_chan;
           let end_time = Unix.time () in
           if end_time -. !start_time >= 600. then Unix.sleep 60;
           start_time := Unix.time ()
         with Sys_error _ -> ())
