HTML_DIR=$(shell pwd)/_build/default/src/hazelweb/www
HTML_FILE=$(HTML_DIR)/index.html

all: dev

deps:
	opam switch import opam.export

change-deps:
	opam switch export opam.export

dev:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

watch:
	dune build @src/fmt --auto-promote src --profile dev --watch

release:
	dune build src --profile release

echo-html-dir:
	@echo "$(HTML_DIR)"

echo-html:
	@echo "$(HTML_FILE)"

win-chrome:
	wslpath -w $(HTML_FILE) | xargs -0 "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"

win-firefox:
	wslpath -w $(HTML_FILE) | xargs -0 "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"

firefox:
	firefox "$(HTML_FILE)" &

chrome:
	chrome "$(HTML_FILE)" &

chrome-browser:
	chrome-browser "$(HTML_FILE)" &

chromium:
	chromium "$(HTML_FILE)" &

chromium-browser:
	chromium-browser "$(HTML_FILE)" &

xdg-open:
	xdg-open "$(HTML_FILE)"

open: dev
	open "$(HTML_FILE)"

repl:
	dune utop src/hazelcore

test:
	dune build @src/fmt --auto-promote || true
	dune runtest || true

fix-test-answers:
	dune promote || true

clean:
	dune clean

.PHONY: all deps dev release echo-html-dir echo-html win-chrome win-firefox repl clean
