(lang dune 3.7)

(name olinkcheck)

(generate_opam_files true)

(source
 (github tarides/olinkcheck))

(authors "Shreyas Krishnakumar")

(maintainers "Shreyas Krishnakumar")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name olinkcheck)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune omd alcotest :with-test)
 (tags
  (topics "to describe" your project)))

