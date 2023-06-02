open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string)) "same lists" [] (Plaintext.extract_links "")

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Plaintext.extract_links
       "This text does not contain any links. a://b.c is not a web link.")

let test_text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://link1.com";
      "https://link2.com/)abc-def-123";
      "https://www.link3.com/a?b=c)";
      "http://www.link4.com/a?b=c&";
    ]
    (Plaintext.extract_links
       "This text contains multiple links, such as http://link1.com. \
        https://link2.com/)abc-def-123, https://www.link3.com/a?b=c) and so \
        on. However, only a part of http://www.link4.com/a?b=c&]-not-matched \
        is matched.")
