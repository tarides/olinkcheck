let exclude_patterns exclude_list links =
  links
  |> List.filter (fun link ->
         List.fold_left
           (fun ok prefix -> ok && not (String.starts_with ~prefix link))
           true exclude_list)
