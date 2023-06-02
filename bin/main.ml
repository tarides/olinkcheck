open Olinkcheck
open Cmdliner

type format = Markdown

let format =
  let doc = "Format of the input file." in
  let format = Arg.enum [ ("md", Markdown) ] in
  Arg.(required & pos 0 (some format) None & info [] ~docv:"FORMAT" ~doc)

let file =
  let doc = "File to check links." in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let olinkcheck format file =
  match format with
  | Markdown ->
      `Ok
        (let links = Markdown.extract_links (Utils.file_contents file) in
         List.iter
           (fun link ->
             (link, Link.status link) |> Utils.pretty_print_link_status)
           links)

let cmd =
  let doc = "Check the status of links in a file." in
  let info = Cmd.info "olinkcheck" ~doc in
  Cmd.v info Term.(ret (const olinkcheck $ format $ file))

let main = exit (Cmd.eval cmd)
let () = main
