open Olinkcheck
open Cmdliner

type format = Markdown | Plaintext | YamlMd | YamlHtml

let format =
  let doc = "Format of the input file." in
  let format =
    Arg.enum
      [
        ("md", Markdown);
        ("txt", Plaintext);
        ("md_with_yaml", YamlMd);
        ("html_with_yaml", YamlHtml);
      ]
  in
  Arg.(required & pos 0 (some format) None & info [] ~docv:"FORMAT" ~doc)

let file =
  let doc = "File to check links." in
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE" ~doc)

let verbose =
  let doc = "Flag to report links with status 200 OK" in
  Arg.(value & flag & info [ "verbose" ] ~doc)

let annotate_in_file =
  let doc = "Annotate broken links in the file" in
  Arg.(value & flag & info [ "annotate" ] ~doc)

let exclude_list =
  let doc = "File containing URL patterns to avoid querying" in
  Arg.(
    value
    & opt (some file) None
    & info [ "exclude-list" ] ~docv:"EXCLUDE-LIST" ~doc)

let olinkcheck annotate_in_file verbose exclude_list format file =
  let f =
    if annotate_in_file then Utils.annotate_in_file
    else Utils.pretty_print_link_status_from_file
  in
  match format with
  | Markdown -> `Ok (f (module Markdown) verbose exclude_list ".md" file)
  | Plaintext -> `Ok (f (module Plaintext) verbose exclude_list ".md" file)
  | YamlMd -> `Ok (f (module YamlMd) verbose exclude_list ".md" file)
  | YamlHtml -> `Ok (f (module YamlHtml) verbose exclude_list ".md" file)

let cmd =
  let doc = "Check the status of links in a file." in
  let info = Cmd.info "olinkcheck" ~doc in
  Cmd.v info
    Term.(
      ret
        (const olinkcheck $ annotate_in_file $ verbose $ exclude_list $ format
       $ file))

let _ = exit (Cmd.eval cmd)
