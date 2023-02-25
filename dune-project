(lang dune 3.4)

(name dist-sys-challenge)

(generate_opam_files true)

(source
 (github TheLortex/ocaml-maelstrom))

(authors "Lucas Pluvinage")

(maintainers "Lucas Pluvinage")

(license MIT)

(package
 (name dist-sys-challenge)
 (synopsis "Solutions and a helper library for the Fly.io challenges")
 (depends ocaml dune eio yojson ppx_deriving_yojson fmt eio_main))
