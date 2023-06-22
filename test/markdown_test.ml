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

let test_fix_links () =
  let new_links =
    [
      "http://newlink1.com";
      "http://newlink2.com";
      "http://newlink3.com";
      "http://newlink4.com";
      "http://newlink5.com";
      "http://newlink6.com";
      "http://newlink7.com";
      "http://newlink8.com";
      "http://newlink9.com";
      "http://newlink10.com";
      "http://newlink11.com";
      "http://newlink12.com";
      "http://newlink13.com";
    ]
  in
  Alcotest.(check (list string))
    "same lists" new_links
    (let ids = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ] in
     let md =
       Markdown.from_string
         In_channel.(with_open_bin "test.md" In_channel.input_all)
     in
     let vs = List.combine ids new_links in
     let _, transformed_md = Markdown.replace_links vs md in
     Markdown.extract_links transformed_md)

let test_annotate_in_str () =
  let md =
    "[link1](http://www.google.com) and \
     [link2](http://www.google.com/does-not-exist), but not \
     http://www.google.com/does-not-exist-too"
  in
  let annotated_md =
    "[link1](http://www.google.com) and \
     [link2](http://www.google.com/does-not-exist - [404 Not Found]) but not \
     http://www.google.com/does-not-exist-too"
  in
  Alcotest.(check string)
    "same string" annotated_md
    (Markdown.annotate_in_str md)
