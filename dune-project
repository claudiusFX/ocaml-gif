(lang dune 3.18)

(name giflib)

(generate_opam_files true)

(source
 (github claudiusFX/ocaml-gif))

(authors "Jan Dudek <jd@jandudek.com>" "Michael Dales <michael@digitalflapjack.com>" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>" "Shreya Pawaskar <shreya.pawaskar.main@gmail.com>")

(license MIT)

(maintenance_intent "(latest)")

(documentation https://github.com/claudiusFX/ocaml-gif)

(package
 (name giflib)
 (synopsis "Implementation of GIF image format in OCaml")
 (description "Implementation of GIF image format in OCaml.")
 (depends (ocaml (>= 5.1)) dune (ounit2 :with-test) zarith (odoc :with-doc) (ocamlformat (and (>= 0.27.0) :with-dev-setup)))
 (tags
  (image gif bitmap)))
