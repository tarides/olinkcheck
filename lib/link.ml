let link_status_request link =
  let open Hyper in
  let link_request = request ~method_:`GET link in
  let run_request request =
    let open Lwt.Syntax in
    let* response = run request in
    Lwt.return (status response)
  in
  run_request link_request

let run_link_status_request link = Lwt_main.run (link_status_request link)

let link_status_client server link =
  let open Hyper in
  let status =
    match server link with
    | exception Invalid_argument _ -> (0, "Invalid Link")
    | exception exn -> (0, Printexc.to_string exn)
    | status -> (status_to_int status, status_to_string status)
  in
  status

let link_status link = link_status_client run_link_status_request link
