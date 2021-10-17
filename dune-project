(lang dune 2.9)

(generate_opam_files true)

(name ppx_deriving_brr)
(source (github patricoferris/ppx_deriving_brr))
(license MIT)
(authors "Patrick Ferris")
(maintainers "patrick@sirref.org")

(package
 (name ppx_deriving_brr)
 (synopsis "PPX deriver for brr")
 (description "Generate common brr bindings using OCaml types as a basis")
 (documentation "https://patricoferris.github.io/ppx-deriving-brr")
 (depends
  brr 
  (ppxlib (and (>= 0.12.0)))))