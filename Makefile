all: dev

deps:
	opam install \
		core dune incr_dom oUnit ppx_let ppx_sexp_conv reason re \
		rtop sexplib utop

dev debug:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

release:
	dune build src --profile release

test:
	dune runtest

chrome:
	chrome-browser _build/default/src/hazelweb/www/hazel.html

chromium:
	chromium-browser _build/default/src/hazelweb/www/hazel.html

firefox:
	firefox _build/default/src/hazelweb/www/hazel.html

repl:
	dune utop src/hazelcore

clean:
	dune clean

.PHONY: all deps dev debug release test chrome chromium firefox repl clean
