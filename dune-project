(lang dune 3.7)

(name tanenbaum)
(version 0.1.0)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/AoC))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(license BSD-3-Clause)

(package
 (name tanenbaum)
 (synopsis "OCaml Advent of Code starter project")
 (depends
  ocaml
  reason
  dune
  (cmdliner(>= 1.2.0))
  (piaf (>= 0.1.0))
  (fmt (>= 0.9.0)))
 (tags ("advent-of-code")))
