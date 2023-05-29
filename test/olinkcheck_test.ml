open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string)) "same lists" [] (extract_links "")

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (extract_links
       "#Heading This text does not contain links. ![alt text](image.png) is \
        an image.")

let test_text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://link1.com";
      "https://link2.com";
      "http://link3.com";
      "http://link4.com";
    ]
    (extract_links
       "#Heading [link1](http://link1.com) ##Multiple links - \
        [link2](https://link2.com) - [link3](http://link3.com)  \
        ###[link4](http://link4.com)")

let test_valid_link () =
  Alcotest.(check (pair int string))
    "same pair" (200, "OK")
    (link_status "http://www.google.com")

let test_nonexistent_link () =
  Alcotest.(check (pair int string))
    "same pair" (404, "Not Found")
    (link_status "http://google.com/does-not-exist")

let test_invalid_link () =
  Alcotest.(check (pair int string))
    "same pair" (0, "Invalid Link") (link_status "")

let () =
  Alcotest.run "Olinkcheck"
    [
      ( "extract-links",
        [
          Alcotest.test_case "Empty text" `Quick test_empty_text;
          Alcotest.test_case "Text without links" `Quick test_text_without_links;
          Alcotest.test_case "Text with links" `Slow test_text_with_links;
        ] );
      ( "link-status",
        [
          Alcotest.test_case "Valid link" `Quick test_valid_link;
          Alcotest.test_case "Invalid link" `Quick test_invalid_link;
          Alcotest.test_case "Non-existent link" `Quick test_nonexistent_link;
        ] );
    ]
