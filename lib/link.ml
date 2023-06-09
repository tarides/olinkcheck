let request url =
  Ezcurl_lwt.get
    ~headers:
      [
        ( "User-Agent",
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 \
           (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36" );
      ]
    ~url ()

let reason = function
  | 100 -> "Continue"
  | 101 -> "Switching Protocols"
  | 102 -> "Processing"
  | 103 -> "Early Hints"
  | 200 -> "OK"
  | 201 -> "Created"
  | 202 -> "Accepted"
  | 203 -> "Non-Authoritative Information"
  | 204 -> "No Content"
  | 205 -> "Reset Content"
  | 206 -> "Partial Content"
  | 207 -> "Multi-Status"
  | 208 -> "Already Reported"
  | 226 -> "226"
  | 300 -> "Multiple Choices"
  | 301 -> "Moved Permanently"
  | 302 -> "Found"
  | 303 -> "See Other"
  | 304 -> "Not Modified"
  | 305 -> "Use Proxy"
  | 307 -> "Temporary Redirect"
  | 308 -> "Permanent Redirect"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 402 -> "Payment Required"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 406 -> "Not Acceptable"
  | 407 -> "Proxy Authentication Required"
  | 408 -> "Request Timeout"
  | 409 -> "Conflict"
  | 410 -> "Gone"
  | 411 -> "Length Required"
  | 412 -> "Precondition Failed"
  | 413 -> "Payload Too Large"
  | 414 -> "URI Too Long"
  | 415 -> "Unsupported Media Type"
  | 416 -> "Range Not Satisfiable"
  | 417 -> "Expectation Failed"
  | 421 -> "Misdirected Request"
  | 422 -> "Unprocessable Entity"
  | 423 -> "Locked"
  | 424 -> "Failed Dependency"
  | 425 -> "Too Early"
  | 426 -> "Upgrade Required"
  | 428 -> "Precondition Required"
  | 429 -> "Too Many Requests"
  | 431 -> "Request Header Fields Too Large"
  | 451 -> "Unavailable For Legal Reasons"
  | 500 -> "Internal Server Error"
  | 501 -> "Not Implemented"
  | 502 -> "Bad Gateway"
  | 503 -> "Service Unavailable"
  | 504 -> "Gateway Timeout"
  | 505 -> "HTTP Version Not Supported"
  | 506 -> "Variant Also Negotiates"
  | 507 -> "Insufficient Storage"
  | 508 -> "Loop Detected"
  | 510 -> "Not Extended"
  | 511 -> "Network Authentication Required"
  | _ -> "Invalid"

let fail_with_timeout () =
  let open Lwt.Syntax in
  let* () = Lwt_unix.sleep 300. in
  Lwt.return
    (Error (Curl.curlCode_of_int 28 |> Option.get, "Request timed out."))

let client server link =
  let open Lwt.Syntax in
  let* status = Lwt.pick [ server link; fail_with_timeout () ] in
  match status with
  | Error (_, e) -> Lwt.return (1, "Client error: " ^ e)
  | Ok r -> Lwt.return (r.Ezcurl_lwt.code, reason r.Ezcurl_lwt.code)

let status link = Lwt_main.run (client request link)
let status_many links = Lwt_main.run (Lwt_list.map_p (client request) links)
