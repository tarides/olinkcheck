open Olinkcheck

let empty_text () =
  Alcotest.(check (list string))
    "same lists" []
    (Yaml.extract_links (Yaml.from_string ""))

let text_without_links () =
  Alcotest.(check (list string))
    "same lists" []
    (Yaml.extract_links
       (Yaml.from_string "this: yaml\ndoc: \n- does: not\n- contain: links"))

let text_with_links () =
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
    ("data/test.yaml" |> Olinkcheck.read_bin |> Yaml.from_string
   |> Yaml.extract_links)

let fix_links () =
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
     let md = Yaml.from_string @@ Olinkcheck.read_bin "data/test.yaml" in
     let vs = List.combine ids new_links in
     let _, transformed_yaml = Yaml.replace_links vs md in
     Yaml.extract_links transformed_yaml)
