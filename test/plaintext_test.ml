open Olinkcheck

let empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Plaintext.extract_links (Plaintext.from_string ""))

let text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Plaintext.extract_links
       (Plaintext.from_string
          "This text does not contain any links. a://b.c is not a web link."))

let text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://link1.com";
      "https://link2.com/)abc-def-123";
      "https://www.link3.com/a?b=c)";
      "http://www.link4.com/a?b=c&";
    ]
    (Plaintext.extract_links
       (Plaintext.from_string
          "This text contains multiple links, such as http://link1.com. \
           https://link2.com/)abc-def-123, https://www.link3.com/a?b=c) and so \
           on. However, only a part of \
           http://www.link4.com/a?b=c&]-not-matched is matched."))

let fix_links () =
  let new_links =
    [
      "http://newlink1.com";
      "https://newlink2.com";
      "http://www.newlink3.com/abc";
    ]
  in
  Alcotest.(check (list string))
    "same lists" new_links
    (let text =
       Plaintext.from_string
         "This http://www.link1.com text contains   multiple https://link2.com \
          links. like. http://link3.com."
     in
     let ids = [ 0; 1; 2 ] in
     let vs = List.combine ids new_links in
     let _, replaced_text = Plaintext.replace_links vs text in
     Plaintext.extract_links replaced_text)

let annotate () =
  let str =
    "http://www.google.com, http://www.google.com/does and \
     http://www.google.com/does-not-exist."
  in
  let annotated_str =
    "http://www.google.com, http://www.google.com/does - [404 Not Found] and \
     http://www.google.com/does-not-exist - [404 Not Found]."
  in
  Alcotest.(check string)
    "same string" annotated_str
    (fst (Plaintext.annotate str))

let verbose_annotate () =
  let str =
    "http://www.google.com, http://www.google.com/does and \
     http://www.google.com/does-not-exist."
  in
  let annotated_str =
    "http://www.google.com - [200 OK], http://www.google.com/does - [404 Not \
     Found] and http://www.google.com/does-not-exist - [404 Not Found]."
  in
  Alcotest.(check string)
    "same string" annotated_str
    (fst (Plaintext.annotate ~verbose:true str))
