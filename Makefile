HTML_DIR=_esy/default/build/default/src/hazelweb/www
HTML_FILE=$(HTML_DIR)/index.html
HTML_REAL_PATH=$(realpath $(HTML_FILE))

all: dev

deps:
	opam switch import opam.export

change-deps:
	opam switch export opam.export

dev:
	dune build @src/fmt --auto-promote || true
	dune build src --profile dev

release:
	dune build src --profile release

echo-html-dir:
	@echo "$(HTML_DIR)"

echo-html:
	@echo "$(HTML_FILE)"

win-chrome:
	"/mnt/c/Program Files (x86)/Google/Chrome/Application/chrome.exe" "$(HTML_REAL_PATH)"

win-firefox:
	"/mnt/c/Program Files/Mozilla Firefox/firefox.exe" "$(HTML_REAL_PATH)"

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

open:
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
