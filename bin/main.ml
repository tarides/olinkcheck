open Olinkcheck
open Cmdliner

type format = Markdown | Plaintext

let format =
  let doc = "Format of the input file." in
  let format = Arg.enum [ ("md", Markdown); ("txt", Plaintext) ] in
  Arg.(required & pos 0 (some format) None & info [] ~docv:"FORMAT" ~doc)

let file =
  let doc = "File to check links." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let olinkcheck format file =
  match format with
  | Markdown ->
      `Ok
        (Utils.pretty_print_link_status_from_file Markdown.from_string
           Markdown.extract_links file)
  | Plaintext ->
      `Ok
        (Utils.pretty_print_link_status_from_file Plaintext.from_string
           Plaintext.extract_links file)

let cmd =
  let doc = "Check the status of links in a file." in
  let info = Cmd.info "olinkcheck" ~doc in
  Cmd.v info Term.(ret (const olinkcheck $ format $ file))

let main = exit (Cmd.eval cmd)
let () = main
