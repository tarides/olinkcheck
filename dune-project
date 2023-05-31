(lang dune 3.7)

(name olinkcheck)

(generate_opam_files true)

(source
 (github tarides/olinkcheck))

(authors "Shreyas Krishnakumar")

(maintainers "Shreyas Krishnakumar")

(package
 (name olinkcheck)
 (synopsis "A tool to detect broken URLs in markdown")
 (description "This tool finds all URLs in markdown files and checks if they are valid by pinging them.")
 (depends ocaml dune omd (alcotest :with-test) hyper lwt cmdliner))

