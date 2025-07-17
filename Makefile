TEST_DIR="$(shell pwd)/_build/default/test"
HTML_DIR="$(shell pwd)/_build/default/src/web/www"
SERVER="http://0.0.0.0:8000/"

.PHONY: all deps change-deps setup-instructor setup-student dev dev-helper dev-student fmt watch watch-release release release-student echo-html-dir serve serve2 repl test clean

all: dev

deps:
	opam update
	opam install ./hazel.opam.locked --deps-only --with-test --with-doc
	npm install

change-deps:
	opam update
	dune build hazel.opam
	opam install ./hazel.opam --deps-only --with-test --with-doc
	opam lock .
	sed -i'.old' '/host-/d' hazel.opam.locked  # remove host- lines which are arch-specific. Not using -i '' because of portability issues https://stackoverflow.com/questions/4247068/sed-command-with-i-option-failing-on-mac-but-works-on-linux

setup-instructor:
	cp src/web/exercises/settings/ExerciseSettings_instructor.re src/web/exercises/settings/ExerciseSettings.re

setup-student: 
	cp src/web/exercises/settings/ExerciseSettings_student.re src/web/exercises/settings/ExerciseSettings.re

dev-helper:
	dune fmt --auto-promote || true
	dune build @ocaml-index @src/fmt --auto-promote src --profile dev

dev: setup-instructor dev-helper

dev-student: setup-student dev-helper

fmt:
	dune fmt --auto-promote

watch: setup-instructor
	dune build @ocaml-index @src/fmt --auto-promote src --profile dev --watch

watch-release: setup-instructor
	dune build @src/fmt --auto-promote src --profile release --watch

release: setup-instructor
	dune build @src/fmt --auto-promote src --profile release

release-student: setup-student
	dune build @src/fmt --auto-promote src --profile dev # Uses dev profile for performance reasons. It may be worth it to retest since the ocaml upgrade

echo-html-dir:
	@echo $(HTML_DIR)

unserve:
	@pids=$$(lsof -t -i:8000 -sTCP:LISTEN); \
	if [ $$(echo $$pids | wc -w) -eq 1 ]; then \
		echo "Killing process on port 8000 with PID $$pids"; \
		kill -9 $$pids; \
	else \
		echo "Not killing: found $$(echo $$pids | wc -w) processes on port 8000"; \
	fi

serve:  unserve
	cd $(HTML_DIR); python3 -m http.server 8000 --bind 0.0.0.0 &

hot:
	npx vite

serve2:
	cd $(HTML_DIR); python3 -m http.server 8001 --bind 0.0.0.0

repl:
	dune utop src/haz3lcore

test:
	dune fmt --auto-promote || true
	dune build @ocaml-index @src/fmt @test/fmt --auto-promote src test --profile dev
	node $(TEST_DIR)/haz3ltest.bc.js

test-quick:
	dune build @ocaml-index @src/fmt @test/fmt --auto-promote src test --profile dev
	node $(TEST_DIR)/haz3ltest.bc.js -q

watch-test:
	dune build @ocaml-index @fmt @runtest @default --profile dev --auto-promote --watch

coverage:
	dune build @src/fmt @test/fmt --auto-promote src test --profile dev
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report summary

ci:
	dune build --profile dev
	dune runtest --instrument-with bisect_ppx --force
	
generate-coverage-html:
	bisect-ppx-report html

clean:
	dune clean
