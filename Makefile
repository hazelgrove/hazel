debug:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

release:
	dune build src --profile release

test:
	dune runtest

repl:
	dune utop src/hazelcore

deps:
	opam install dune reason js_of_ocaml tyxml deriving \
		ppx_deriving reactiveData ocp-indent js_of_ocaml-tyxml \
		menhir oUnit sexplib ppx_sexp_conv incr_dom utop

clean:
	dune clean
