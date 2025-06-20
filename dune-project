(lang dune 3.14)

(name giflib)

(version 1.0.0)

(generate_opam_files true)

(source
 (github claudiusFX/ocaml-gif))

(authors "Jan Dudek <jd@jandudek.com>" "Michael Dales <michael@digitalflapjack.com" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>")

(license MIT)

(documentation https://github.com/claudiusFX/ocaml-gif)

(package
 (name giflib)
 (synopsis "Implementation of GIF image format in OCaml")
 (description "Implementation of GIF image format in OCaml.")
 (depends ocaml dune ounit2 camlp-streams zarith odoc (ocamlformat (>= 0.27.0)))
 (tags
  (image gif bitmap)))
