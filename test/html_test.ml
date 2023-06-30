open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Html.extract_links (Html.from_string ""))

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Html.extract_links
       (Html.from_string
          "<html><p>this html</p><ul><li>does \
           not</li><li>have</li><li>links</li></ul></html>"))

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
    (Html.extract_links
       (Html.from_string In_channel.(with_open_bin "test.html" input_all)))

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
     let html =
       Html.from_string In_channel.(with_open_bin "test.html" input_all)
     in
     let vs = List.combine ids new_links in
     let _, transformed_html = Html.replace_links vs html in
     Html.extract_links transformed_html)
