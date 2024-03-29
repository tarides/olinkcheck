open Olinkcheck

let empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Html.extract_links (Html.from_string ""))

let text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Html.extract_links
       (Html.from_string
          "<html><p>this html</p><ul><li>does \
           not</li><li>have</li><li>links</li></ul></html>"))

let text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://link1.com";
      "http://link2.com";
      "http://link3.com";
      "http://link4.com";
      "http://link5.com";
    ]
    ("data/test.html" |> Olinkcheck.read_bin |> Html.from_string
   |> Html.extract_links)

let fix_links () =
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
     let html = Html.from_string @@ Olinkcheck.read_bin "data/test.html" in
     let vs = List.combine ids new_links in
     let _, transformed_html = Html.replace_links vs html in
     Html.extract_links transformed_html)
