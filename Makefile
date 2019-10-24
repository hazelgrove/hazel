debug:
	-dune build src @src/fmt --profile dev --auto-promote

release:
	dune build src --profile release

deps:
	opam install dune reason js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent js_of_ocaml-tyxml menhir oUnit sexplib ppx_sexp_conv

clean:
	dune clean

BUILD_WWW = "_build/default/src/www"
build_www:
	@echo $(BUILD_WWW)