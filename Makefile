dev debug:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

release:
	dune build src --profile release

test:
	dune runtest

repl:
	dune utop src/hazelcore

deps:
	opam install \
		core dune incr_dom oUnit ppx_let ppx_sexp_conv reason re \
		rtop sexplib utop

clean:
	dune clean

.PHONY: dev debug release test repl deps clean
