(lang dune 3.16)

(name hazel)

(generate_opam_files true)

(source
 (github hazelgrove/hazel))

; (authors "Author")
; (maintainers "Maintainer")

(license MIT)

(package
 (name hazel)
 (allow_empty)
;  (synopsis "A short synopsis")
;  (description "A longer description")
 ; (tags
 ; (topics "to describe" your project))
 (depends
  (ocaml
   (>= 5.2.0))
  (menhir
   (>= 2.0))
  yojson
  (reason (= 3.11.0))
  (ppx_yojson_conv_lib (= v0.16.0))
  (ppx_yojson_conv (= v0.16.0))
  (incr_dom (= v0.16.0))
  (omd (= 1.3.2))
  (ezjs_idb (= 0.1.1))
  virtual_dom
  ppx_deriving
  ptmap
  uuidm
  unionFind
  ocamlformat ; Maybe make a dev tag?
  (junit_alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
