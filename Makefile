all:
	-dune build src @fmt --auto-promote
	dune build src

debug:
	-dune build src @fmt --auto-promote
	dune build src --profile debug

deps:
	opam install dune reason js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent camomile js_of_ocaml-tyxml menhir oUnit sexplib ppx_sexp_conv

clean:
	dune clean
