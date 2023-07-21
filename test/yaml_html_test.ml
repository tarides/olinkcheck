open Olinkcheck

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (YamlHtml.extract_links
       (YamlHtml.from_string
          "---\n\
          \ field: a\n\
          \ b: c\n\
           ---\n\
           <html>This HTML body does not have links</html>."))

let test_text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://linka.com";
      "http://linkb.com";
      "http://linkc.com";
      "http://link1.com";
      "http://link2.com";
      "http://link3.com";
      "http://link4.com";
      "http://link5.com";
    ]
    ("data/yaml_html_1.md" |> Olinkcheck.read_bin |> YamlHtml.from_string
   |> YamlHtml.extract_links)

let test_fix_links () =
  let new_links =
    [
      "http://newlinka.com";
      "http://newlinkb.com";
      "http://newlinkc.com";
      "http://newlink1.com";
      "http://newlink2.com";
      "http://newlink3.com";
      "http://newlink4.com";
      "http://newlink5.com";
    ]
  in
  Alcotest.(check (list string))
    "same lists" new_links
    (let ids = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
     let md =
       YamlHtml.from_string @@ Olinkcheck.read_bin "data/yaml_html_1.md"
     in
     let vs = List.combine ids new_links in
     let _, transformed_md = YamlHtml.replace_links vs md in
     YamlHtml.extract_links transformed_md)

let test_annotate () =
  let md = read_bin "data/yaml_html_2.md" in
  let annotated_md =
    "---\n\
     url: http://www.google.com\n\
     broken: http://www.google.com/does-not-exist - [404 Not Found]\n\
     ---\n\
     <html>\n\
     <a href=\"http://www.google.com\"> Link1 </a>\n\
     <a href=\"http://www.google.com/does-not-exist - [404 Not Found]\"> Link \
     2 </a>\n\
     <a href=\"http://www.google.com/does-not-exist-too - [404 Not Found]\"> \
     Link 3 </a>\n\
     </html>\n"
  in
  Alcotest.(check string)
    "same string" annotated_md
    (fst (YamlHtml.annotate md))

let test_verbose_annotate () =
  let md = read_bin "data/yaml_html_2.md" in
  let annotated_md =
    "---\n\
     url: http://www.google.com - [200 OK]\n\
     broken: http://www.google.com/does-not-exist - [404 Not Found]\n\
     ---\n\
     <html>\n\
     <a href=\"http://www.google.com - [200 OK]\"> Link1 </a>\n\
     <a href=\"http://www.google.com/does-not-exist - [404 Not Found]\"> Link \
     2 </a>\n\
     <a href=\"http://www.google.com/does-not-exist-too - [404 Not Found]\"> \
     Link 3 </a>\n\
     </html>\n"
  in
  Alcotest.(check string)
    "same string" annotated_md
    (fst (YamlHtml.annotate ~verbose:true md))
