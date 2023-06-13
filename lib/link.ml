let request link =
  let open Lwt.Syntax in
  let request = Hyper.request ~method_:`GET link in
  let* response = Hyper.run request in
  Lwt.return (Hyper.status response)

let client server link =
  let status =
    match server link with
    | exception Invalid_argument _ -> (0, "Invalid Link")
    | exception exn -> (1, "Client error: " ^ Printexc.to_string exn)
    | status -> (Hyper.status_to_int status, Hyper.status_to_string status)
  in
  status

let status = client (fun link -> link |> request |> Lwt_main.run)
