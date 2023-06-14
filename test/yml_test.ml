open Olinkcheck

let test_empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Yml.extract_links (Yml.from_string ""))

let test_text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Yml.extract_links
       (Yml.from_string "this: yaml\ndoc: \n- does: not\n- contain: links"))

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
    ]
    (Yml.extract_links
       (Yml.from_string In_channel.(with_open_bin "test.yaml" input_all)))

let test_fix_links () =
  let new_links =
    [
      "http://newlink1.com";
      "http://newlink2.com";
      "http://newlink3.com";
      "http://newlink4.com";
      "http://newlink5.com";
      "http://newlink6.com";
    ]
  in
  Alcotest.(check (list string))
    "same lists" new_links
    (let ids = [ 0; 1; 2; 3; 4; 5 ] in
     let md =
       Yml.from_string In_channel.(with_open_bin "test.yaml" input_all)
     in
     let vs = List.combine ids new_links in
     let _, transformed_yaml = Yml.replace_links vs md in
     Yml.extract_links transformed_yaml)
