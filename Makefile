debug:
	-dune build src @src/fmt --profile dev --auto-promote

release:
	dune build src --profile release

repl:
	dune utop src/hazelcore

deps:
	opam install dune reason js_of_ocaml tyxml deriving \
		ppx_deriving reactiveData ocp-indent js_of_ocaml-tyxml \
		menhir oUnit sexplib ppx_sexp_conv incr_dom utop

clean:
	dune clean
