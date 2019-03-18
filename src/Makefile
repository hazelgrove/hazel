all:
	-dune build @fmt --auto-promote
	dune build

debug:
	-dune build @fmt --auto-promote
	dune build --profile debug

deps:
	opam install dune reason js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent camomile js_of_ocaml-tyxml menhir oUnit sexplib ppx_sexp_conv

clean:
	dune clean
