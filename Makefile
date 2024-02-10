TEST_DIR="$(shell pwd)/_build/default/src/test"
HTML_DIR="$(shell pwd)/_build/default/src/haz3lweb/www"
SERVER="http://0.0.0.0:8000/"

all: dev

deps:
	opam update
	opam switch import opam.export

change-deps:
	opam switch export opam.export

setup-instructor:
	cp src/haz3lweb/ExerciseSettings_instructor.re src/haz3lweb/ExerciseSettings.re

setup-student: 
	cp src/haz3lweb/ExerciseSettings_student.re src/haz3lweb/ExerciseSettings.re

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
	dune build @src/fmt --auto-promote src --profile dev

echo-html-dir:
	@echo $(HTML_DIR)

serve:
	cd $(HTML_DIR); python3 -m http.server 8000

serve2:
	cd $(HTML_DIR); python3 -m http.server 8001

repl:
	dune utop src/haz3lcore

test:
	dune build @src/fmt --auto-promote src --profile dev
	node $(TEST_DIR)/haz3ltest.bc.js test

clean:
	dune clean
