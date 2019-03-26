all:
	-dune build src @src/fmt --auto-promote

debug:
	dune build src --profile debug

deps:
	opam install dune reason js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent camomile js_of_ocaml-tyxml menhir oUnit sexplib ppx_sexp_conv

clean:
	dune clean
