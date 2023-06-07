open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Markdown.extract_links (Markdown.from_string ""))

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Markdown.extract_links
       (Markdown.from_string
          "#Heading This text does not contain links. ![alt text](image.png) \
           is an image."))

let test_text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://link1.com";
      "http://link2.com";
      "http://link3.com";
      "http://link4.com";
      "http://link5.com";
      "http://link6.com";
      "http://link7.com";
      "http://link8.com";
      "http://link9.com";
      "http://link10.com";
      "http://link11.com";
      "http://link12.com";
      "http://link13.com";
    ]
    (Markdown.extract_links
       (Markdown.from_string
          In_channel.(with_open_bin "test.md" In_channel.input_all)))
