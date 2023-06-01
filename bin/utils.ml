let file_contents file = In_channel.(with_open_bin file In_channel.input_all)

let pretty_print_link_status (link, (status_code, status_string)) =
  Printf.printf "%s - %i %s\n" link status_code status_string
