open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Sexp.extract_links (Sexp.from_string ""))

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Sexp.extract_links
       (Sexp.from_string
          "(this (sexp (does not contain links) a://b.c) is not a web link.)"))

let test_text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://link1.com";
      "http://link2.com";
      "http://link3.com";
      "http://link4.com";
      "http://link5.com";
    ]
    (Sexp.extract_links
       (Sexp.from_string
          "(http://link1.com)(url (http://link2.com))(url2 (nested \
           (http://link3.com)) adjacent http://link4.com (http://link5.com))"))

let test_fix_links () =
  let new_links =
    [
      "http://newlink1.com";
      "http://newlink2.com";
      "http://newlink3.com";
      "http://newlink4.com";
      "http://newlink5.com";
    ]
  in
  Alcotest.(check (list string))
    "same lists" new_links
    (let ids = [ 0; 1; 2; 3; 4 ] in
     let sexp =
       Sexp.from_string
         "(http://link1.com)(url (http://link2.com))(url2 (nested \
          (http://link3.com)) adjacent http://link4.com (http://link5.com))"
     in
     let vs = List.combine ids new_links in
     let _, transformed_sexp = Sexp.replace_links vs sexp in
     Sexp.extract_links transformed_sexp)
