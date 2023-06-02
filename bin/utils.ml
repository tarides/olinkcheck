let file_contents file = In_channel.(with_open_bin file In_channel.input_all)

let pretty_print_link_status (link, (status_code, status_string)) =
  Printf.printf "%s - %i %s\n" link status_code status_string

let pretty_print_link_status_from_file extract_links file =
  file |> file_contents |> extract_links
  |> List.iter (fun link ->
         (link, Olinkcheck.Link.status link) |> pretty_print_link_status)
