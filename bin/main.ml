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

let skip_ok =
  let doc = "Flag to skip reporting links with status 200 OK" in
  Arg.(value & flag & info [ "skip-ok" ] ~doc)

let olinkcheck skip_ok format file =
  match format with
  | Markdown ->
      `Ok
        Markdown.(
          Utils.pretty_print_link_status_from_file skip_ok ".md" from_string
            extract_links file)
  | Plaintext ->
      `Ok
        Plaintext.(
          Utils.pretty_print_link_status_from_file skip_ok ".txt" from_string
            extract_links file)

let cmd =
  let doc = "Check the status of links in a file." in
  let info = Cmd.info "olinkcheck" ~doc in
  Cmd.v info Term.(ret (const olinkcheck $ skip_ok $ format $ file))

let main = exit (Cmd.eval cmd)
let () = main
