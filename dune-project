(lang dune 3.12)

(name advent_2023)

(source
 (github lePerdu/advent_2023))

(authors "Zach Peltzer")
(maintainers "zdpeltzer@gmail.com")

(package
 (name advent_2023)
 (synopsis "Advent 2023 problems in OCaml")
 (depends ocaml dune bheap)
 (allow_empty))

(generate_opam_files)
