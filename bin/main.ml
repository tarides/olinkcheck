open Olinkcheck

let file_contents file = In_channel.with_open_bin file In_channel.input_all

let () =
  if Array.length Sys.argv < 2 then
    print_endline "Usage: dune exec _build/default/bin/main.exe filename.md"
  else List.iter print_endline (Parser.extract_links (file_contents Sys.argv.(1)))
