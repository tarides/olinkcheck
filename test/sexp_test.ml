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
