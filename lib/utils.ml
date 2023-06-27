let exclude_patterns exclude_list links =
  let exclude_regexps =
    List.map
      (fun pattern -> pattern |> Re.Pcre.quote |> Re.Pcre.re |> Re.compile)
      exclude_list
  in
  links
  |> List.filter (fun link ->
         List.fold_left
           (fun ok regexp -> ok && not (Re.execp regexp link))
           true exclude_regexps)
