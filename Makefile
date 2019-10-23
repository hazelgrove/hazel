BUILD_WWW = "_build/default/src/hazelweb/www"

all: dev

deps:
	opam install \
		core dune incr_dom oUnit ppx_let ppx_sexp_conv reason re \
		rtop sexplib utop

dev debug:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

build_www:
	@echo $(BUILD_WWW)

release:
	dune build src --profile release

chrome:
	chrome-browser $(BUILD_WWW)/hazel.html

chromium:
	chromium-browser $(BUILD_WWW)/hazel.html

firefox:
	firefox $(BUILD_WWW)/hazel.html

repl:
	dune utop src/hazelcore

clean:
	dune clean

.PHONY: all deps dev debug release test chrome chromium firefox repl clean
