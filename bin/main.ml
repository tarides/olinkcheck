open Olinkcheck

let () =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: olinkcheck filename.md"
  else
    let links = extract_links (Utils.file_contents Sys.argv.(1)) in
    List.iter
      (fun link ->
        link |> Utils.with_arg Link.status |> Utils.pretty_print_link_status)
      links
