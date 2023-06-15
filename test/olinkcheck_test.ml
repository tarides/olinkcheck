open Olinkcheck

let http_statuses =
  [
    (101, "Switching Protocols");
    (200, "OK");
    (201, "Created");
    (202, "Accepted");
    (203, "Non-Authoritative Information");
    (204, "No Content");
    (205, "Reset Content");
    (206, "Partial Content");
    (207, "Multi-Status");
    (208, "Already Reported");
    (226, "226");
    (300, "Multiple Choices");
    (301, "Moved Permanently");
    (302, "Found");
    (303, "See Other");
    (304, "Not Modified");
    (305, "Use Proxy");
    (307, "Temporary Redirect");
    (308, "Permanent Redirect");
    (400, "Bad Request");
    (401, "Unauthorized");
    (402, "Payment Required");
    (403, "Forbidden");
    (404, "Not Found");
    (405, "Method Not Allowed");
    (406, "Not Acceptable");
    (407, "Proxy Authentication Required");
    (408, "Request Timeout");
    (409, "Conflict");
    (410, "Gone");
    (411, "Length Required");
    (412, "Precondition Failed");
    (413, "Payload Too Large");
    (414, "URI Too Long");
    (415, "Unsupported Media Type");
    (416, "Range Not Satisfiable");
    (417, "Expectation Failed");
    (421, "Misdirected Request");
    (422, "Unprocessable Entity");
    (423, "Locked");
    (424, "Failed Dependency");
    (425, "Too Early");
    (426, "Upgrade Required");
    (428, "Precondition Required");
    (429, "Too Many Requests");
    (431, "Request Header Fields Too Large");
    (451, "Unavailable For Legal Reasons");
    (500, "Internal Server Error");
    (501, "Not Implemented");
    (502, "Bad Gateway");
    (503, "Service Unavailable");
    (504, "Gateway Timeout");
    (505, "HTTP Version Not Supported");
    (506, "Variant Also Negotiates");
    (507, "Insufficient Storage");
    (508, "Loop Detected");
    (510, "Not Extended");
    (511, "Network Authentication Required");
  ]

let test_request_with_status (code, reason) =
  let open Lwt.Syntax in
  let server url =
    let completed, signal = Lwt.wait () in
    let* webserver =
      Server.webserver 8080 signal (string_of_int code ^ " " ^ reason)
    in
    let status = Link.request url in
    let* () = completed in
    let* () = Lwt_io.shutdown_server webserver in
    status
  in
  Lwt_main.run (Link.client server "http://127.0.0.1:8080/")

let test_all_status_codes () =
  Alcotest.(check (list (pair int string)))
    "same lists" http_statuses
    (List.map test_request_with_status http_statuses)

let test_valid_link () =
  Alcotest.(check (pair int string))
    "same pair" (200, "OK")
    (Link.status "http://www.google.com")

let test_nonexistent_link () =
  Alcotest.(check (pair int string))
    "same pair" (404, "Not Found")
    (Link.status "http://google.com/does-not-exist")

let test_invalid_link () =
  Alcotest.(check int) "same code" 1 (fst (Link.status ""))

let () =
  Alcotest.run "Olinkcheck"
    [
      ( "markdown",
        [
          Alcotest.test_case "Empty text" `Quick Markdown_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Markdown_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Markdown_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick
            Markdown_test.test_fix_links;
        ] );
      ( "plaintext",
        [
          Alcotest.test_case "Empty text" `Quick Plaintext_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Plaintext_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Plaintext_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick
            Plaintext_test.test_fix_links;
        ] );
      ( "sexp",
        [
          Alcotest.test_case "Empty text" `Quick Sexp_test.test_empty_text;
          Alcotest.test_case "Text without links" `Quick
            Sexp_test.test_text_without_links;
          Alcotest.test_case "Text with links" `Slow
            Sexp_test.test_text_with_links;
          Alcotest.test_case "Test fix links" `Quick Sexp_test.test_fix_links;
        ] );
      ( "link-status",
        [
          Alcotest.test_case "Valid link" `Quick test_valid_link;
          Alcotest.test_case "Invalid link" `Quick test_invalid_link;
          Alcotest.test_case "Non-existent link" `Quick test_nonexistent_link;
          Alcotest.test_case "All status codes" `Quick test_all_status_codes;
        ] );
    ]
