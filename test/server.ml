let webserver port signal status =
  let open Lwt.Syntax in
  Lwt_io.establish_server_with_client_address
    Unix.(ADDR_INET (inet_addr_loopback, port))
    (fun _handle (_in, out) ->
      let* () = Lwt_io.write out ("HTTP/1.1 " ^ status ^ "\r\n\r\n") in
      let* () = Lwt_io.close out in
      Lwt.wakeup_later signal ();
      Lwt.return_unit)
