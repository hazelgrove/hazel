(lang dune 3.16)

(using menhir 2.0)

(name hazel)

(generate_opam_files true)

(source
 (github hazelgrove/hazel))

(authors "Hazel Development Team")

(maintainers "Hazel Development Team")

(license MIT)

(package
 (name hazel)
 (allow_empty)
 (synopsis
  "Hazel, a live functional programming environment with typed holes")
 ;  (description "A longer description")
 ; (tags
 ; (topics "to describe" your project))
 (depends
  (ocaml
   (>= 5.2.0))
  (menhir
   (>= 2.0))
  yojson
  (reason
   (>= 3.12.0))
  ppx_yojson_conv_lib
  ppx_yojson_conv
  incr_dom
  bisect_ppx
  (omd
   (>= 2.0.0~alpha4))
  ezjs_idb
  bonsai
  ppx_deriving
  ptmap
  (uuidm
   (= 0.9.8)) ; 0.9.9 has breaking deprecated changes
  unionFind
  ocamlformat
  (junit_alcotest
   (and
    (>= 2.1.0)
    :with-test))
  ocaml-lsp-server
  qcheck
  qcheck-alcotest
  ppx_deriving_qcheck))

; After upgrading to opam 2.2 use with-dev https://opam.ocaml.org/blog/opam-2-2-0/

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
