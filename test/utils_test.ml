let exclude_patterns () =
  Alcotest.(check (list string))
    "same lists"
    [ "http://link2.com"; "http://link3.com" ]
    (Olinkcheck.exclude_patterns
       ~prefixes:[ "http://link1.com"; "http://link2.com/a" ]
       [
         "http://link1.com/a";
         "http://link2.com";
         "http://link2.com/ab";
         "http://link3.com";
       ])
