open Olinkcheck

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (YamlMd.extract_links
       (YamlMd.from_string
          "---\n\
          \ field: a\n\
          \ b: c\n\
           ---\n\
           #Heading This text does not contain links. ![alt text](image.png) \
           is an image."))

let test_text_with_links () =
  Alcotest.(check (list string))
    "same lists"
    [
      "http://linka.com";
      "http://linkb.com";
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
    ("test_yaml_md.md" |> Olinkcheck.read_bin |> YamlMd.from_string
   |> YamlMd.extract_links)

let test_fix_links () =
  let new_links =
    [
      "http://newlinka.com";
      "http://newlinkb.com";
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
    (let ids = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14 ] in
     let md = YamlMd.from_string @@ Olinkcheck.read_bin "test_yaml_md.md" in
     let vs = List.combine ids new_links in
     let _, transformed_md = YamlMd.replace_links vs md in
     YamlMd.extract_links transformed_md)

let test_annotate () =
  let md = read_bin "test_yaml_md1.md" in
  let annotated_md =
    "---\n\
     url: http://www.google.com\n\
     broken: http://www.google.com/does-not-exist - [404 Not Found]\n\
     ---\n\
     [link1](http://www.google.com) and \
     [link2](http://www.google.com/does-not-exist - [404 Not Found]), but not \
     http://www.google.com/does-not-exist-too\n"
  in
  Alcotest.(check string)
    "same string" annotated_md
    (fst (YamlMd.annotate false [] md))

let test_verbose_annotate () =
  let md = read_bin "test_yaml_md1.md" in
  let annotated_md =
    "---\n\
     url: http://www.google.com - [200 OK]\n\
     broken: http://www.google.com/does-not-exist - [404 Not Found]\n\
     ---\n\
     [link1](http://www.google.com - [200 OK]) and \
     [link2](http://www.google.com/does-not-exist - [404 Not Found]), but not \
     http://www.google.com/does-not-exist-too\n"
  in
  Alcotest.(check string)
    "same string" annotated_md
    (fst (YamlMd.annotate true [] md))
