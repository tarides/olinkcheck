open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string)) "same lists" [] (Parser.extract_links "")

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Parser.extract_links
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
    (Parser.extract_links
       "#Heading [link1](http://link1.com) ##Multiple links - \
        [link2](https://link2.com) - [link3](http://link3.com)  \
        ###[link4](http://link4.com)")

let () =
  Alcotest.run "Extract Links"
    [
      ( "extract-links",
        [
          Alcotest.test_case "Empty text" `Quick test_empty_text;
          Alcotest.test_case "Text without links" `Quick test_text_without_links;
          Alcotest.test_case "Text with links" `Slow test_text_with_links;
        ] );
    ]
