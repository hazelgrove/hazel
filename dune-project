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
  ppx_blob
  bisect_ppx
  (omd
   (>= 2.0.0~alpha4))
  ezjs_idb
  ; Jane Street v0.18 preview packages (janestreet/opam-repository), the first
  ; Bonsai/virtual_dom to allow js_of_ocaml 6 (#1131). See opam-jsoo6/README.md.
  (bonsai
   (>= v0.18~preview))
  (bonsai_web
   (>= v0.18~preview))
  (virtual_dom
   (>= v0.18~preview))
  ; Js_of_ocaml.Promise, which the preview's `browser` library needs
  (js_of_ocaml
   (>= 6.4.0))
  ppx_deriving
  ptmap
  ; 0.9.8 caps cmdliner < 2, which js_of_ocaml >= 6.3 needs
  (uuidm
   (>= 0.9.10))
  unionFind
  ocamlformat
  (junit_alcotest
   (and
    (>= 2.2.0)
    :with-test))
  ; 1.10 swaps its log link with Unix.rename, which js_of_ocaml >= 6.0.1 has
  (alcotest
   (and
    (>= 1.10.0)
    :with-test))
  ocaml-lsp-server
  ocaml-index
  qcheck
  qcheck-alcotest
  ppx_deriving_qcheck
  bignum
  csv))

; After upgrading to opam 2.2 use with-dev https://opam.ocaml.org/blog/opam-2-2-0/

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
