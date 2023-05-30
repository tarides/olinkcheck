open Olinkcheck

let file_contents file = In_channel.(with_open_bin file In_channel.input_all)

let pretty_print_link_status (link, (status_code, status_string)) =
  Printf.printf "%s - %i %s\n" link status_code status_string

let with_arg f x = (x, f x)

let () =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: olinkcheck filename.md"
  else
    let links = extract_links (file_contents Sys.argv.(1)) in
    List.iter
      (fun link -> link |> with_arg Link.status |> pretty_print_link_status)
      links
