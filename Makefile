HTML_DIR=_build/default/src/hazelweb/www

all: dev

deps:
	opam install \
		core dune incr_dom oUnit ppx_let ppx_sexp_conv reason re \
		rtop sexplib utop

dev:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

release:
	dune build src --profile release

echo-html-dir:
	@echo "$(HTML_DIR)"

echo-html:
	@echo "$(HTML_DIR)/index.html"

win-chrome:
	"/mnt/c/Program Files (x86)/Google/Chrome/Application/chrome.exe" $(make echo-html)

#TODO: win-chromium:

win-firefox:
	"/mnt/c/Program Files (x86)/Mozilla Firefox/firefox.exe" $(make echo-html)


repl:
	dune utop src/hazelcore

clean:
	dune clean

.PHONY: all deps dev debug release test chrome chromium chromium-browser firefox repl clean
