open Olinkcheck
open Cmdliner

type format = Markdown | Plaintext

let format =
  let doc = "Format of the input file." in
  let format = Arg.enum [ ("md", Markdown); ("txt", Plaintext) ] in
  Arg.(required & pos 0 (some format) None & info [] ~docv:"FORMAT" ~doc)

let file =
  let doc = "File to check links." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE" ~doc)

let annotate_in_file =
  let doc = "Annotate broken links in the file" in
  Arg.(value & flag & info [ "annotate-in-file" ] ~doc)

let olinkcheck annotate_in_file format file =
  match format with
  | Markdown ->
      `Ok
        (if annotate_in_file then
           Markdown.(
             Utils.annotate_in_file ".md" from_string extract_links file)
         else
           Markdown.(
             Utils.pretty_print_link_status_from_file ".md" from_string
               extract_links file))
  | Plaintext ->
      `Ok
        (if annotate_in_file then
           Plaintext.(
             Utils.annotate_in_file ".txt" from_string extract_links file)
         else
           Plaintext.(
             Utils.pretty_print_link_status_from_file ".txt" from_string
               extract_links file))

let cmd =
  let doc = "Check the status of links in a file." in
  let info = Cmd.info "olinkcheck" ~doc in
  Cmd.v info Term.(ret (const olinkcheck $ annotate_in_file $ format $ file))

let main = exit (Cmd.eval cmd)
let () = main
