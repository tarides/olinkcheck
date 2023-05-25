let get_link_status link =
  let open Hyper in
  let request = request ~method_:`GET link in
  let status =
    match
      Lwt_main.run
        (let open Lwt.Syntax in
         let* response = run request in
         Lwt.return (status response))
    with
    | exception Invalid_argument _ -> (0, "Invalid Link")
    | exception exn -> (0, Printexc.to_string exn)
    | status -> (status_to_int status, status_to_string status)
  in
  status
