HTML_DIR=$(shell pwd)/_build/default/src/haz3lweb/www
SERVER="http://0.0.0.0:8000/"

all: dev

deps:
	opam switch import opam.export

change-deps:
	opam switch export opam.export

update-ocaml:
	opam update
	opam switch create 4.14 ocaml-base-compiler.4.14.0
	opam switch import opam.export --update-invariant

setup-instructor:
	cp src/haz3lweb/SchoolSettings_instructor.re src/haz3lweb/SchoolSettings.re

setup-student: 
	cp src/haz3lweb/SchoolSettings_student.re src/haz3lweb/SchoolSettings.re

dev-helper: 
	dune build @src/fmt --auto-promote src --profile dev

dev: setup-instructor dev-helper

dev-student: setup-student dev

fmt:
	dune fmt --auto-promote

watch: setup-instructor
	dune build @src/fmt --auto-promote src --profile dev --watch

watch-release: setup-instructor
	dune build @src/fmt --auto-promote src --profile release --watch

release: setup-instructor
	dune build @src/fmt --auto-promote src --profile release

release-student: setup-student
	dune build @src/fmt --auto-promote src --profile release

echo-html-dir:
	@echo "$(HTML_DIR)"

serve:
	cd $(HTML_DIR); python3 -m http.server 8000

serve2:
	cd $(HTML_DIR); python3 -m http.server 8001

repl:
	dune utop src/hazelcore

test:
	dune build @src/fmt --auto-promote || true
	dune exec src/hazeltest/hazeltest.exe -- --regression-dir src/hazeltest/regressions

reset-regression-tests:
	dune exec src/hazeltest/hazeltest.exe -- regression --regression-dir src/hazeltest/regressions --reset-regressions

fix-test-answers:
	dune promote || true

clean:
	dune clean
