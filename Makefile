TEST_DIR="$(shell pwd)/_build/default/test"
HTML_DIR="$(shell pwd)/_build/default/src/haz3lweb/www"
SERVER="http://0.0.0.0:8000/"

.PHONY: all deps change-deps setup-instructor setup-student dev dev-helper dev-student fmt watch watch-release release release-student echo-html-dir serve serve2 repl test clean

all: dev

deps:
	opam update
	opam install ./hazel.opam.locked --deps-only --with-test --with-doc

change-deps:
	opam update
	dune build hazel.opam
	opam install ./hazel.opam --deps-only --with-test --with-doc
	opam lock .
	sed -i'.old' '/host-/d' hazel.opam.locked  # remove host- lines which are arch-specific. Not using -i '' because of portability issues https://stackoverflow.com/questions/4247068/sed-command-with-i-option-failing-on-mac-but-works-on-linux

setup-instructor:
	cp src/haz3lweb/ExerciseSettings_instructor.re src/haz3lweb/ExerciseSettings.re

setup-student: 
	cp src/haz3lweb/ExerciseSettings_student.re src/haz3lweb/ExerciseSettings.re

dev-helper:
	dune fmt --auto-promote || true
	dune build @src/fmt --auto-promote src --profile dev

dev: setup-instructor dev-helper

dev-student: setup-student dev-helper

fmt:
	dune fmt --auto-promote

watch: setup-instructor
	dune build @src/fmt --auto-promote src --profile dev --watch

watch-release: setup-instructor
	dune build @src/fmt --auto-promote src --profile release --watch

release: setup-instructor
	dune build @src/fmt --auto-promote src --profile release

release-student: setup-student
	dune build @src/fmt --auto-promote src --profile dev # Uses dev profile for performance reasons. It may be worth it to retest since the ocaml upgrade

echo-html-dir:
	@echo $(HTML_DIR)

serve:
	cd $(HTML_DIR); python3 -m http.server 8000

serve2:
	cd $(HTML_DIR); python3 -m http.server 8001

repl:
	dune utop src/haz3lcore

test:
	dune fmt --auto-promote || true
	dune build @src/fmt @test/fmt --auto-promote src test --profile dev
	node $(TEST_DIR)/haz3ltest.bc.js

clean:
	dune clean
