let extract_links text =
  let regexp =
    Re.compile
      (Re.Perl.re
         {|https?://(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b((\.[-a-zA-Z0-9()@:%_\+~#?&//=]+)|([-a-zA-Z0-9()@:%_\+~#?&//=]))*|})
  in
  Re.matches regexp text
