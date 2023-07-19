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
  match format with
  | Markdown ->
      `Ok
        (if annotate_in_file then
           Markdown.(
             Utils.annotate_in_file verbose exclude_list ".md" annotate file)
         else
           Markdown.(
             Utils.pretty_print_link_status_from_file verbose exclude_list ".md"
               from_string extract_links file))
  | Plaintext ->
      `Ok
        (if annotate_in_file then
           Plaintext.(
             Utils.annotate_in_file verbose exclude_list ".txt" annotate file)
         else
           Plaintext.(
             Utils.pretty_print_link_status_from_file verbose exclude_list
               ".txt" from_string extract_links file))
  | YamlMd ->
      `Ok
        (if annotate_in_file then
           YamlMd.(
             Utils.annotate_in_file verbose exclude_list ".md" annotate file)
         else
           YamlMd.(
             Utils.pretty_print_link_status_from_file verbose exclude_list ".md"
               from_string extract_links file))
  | YamlHtml ->
      `Ok
        (if annotate_in_file then
           YamlHtml.(
             Utils.annotate_in_file verbose exclude_list ".md" annotate file)
         else
           YamlHtml.(
             Utils.pretty_print_link_status_from_file verbose exclude_list ".md"
               from_string extract_links file))

let cmd =
  let doc = "Check the status of links in a file." in
  let info = Cmd.info "olinkcheck" ~doc in
  Cmd.v info
    Term.(
      ret
        (const olinkcheck $ annotate_in_file $ verbose $ exclude_list $ format
       $ file))

let _ = exit (Cmd.eval cmd)
