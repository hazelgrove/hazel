HTML_DIR="$(shell pwd)/_build/default/src/web/www"
SERVER="http://0.0.0.0:8000/"

.PHONY: all deps change-deps setup-instructor setup-student dev dev-helper dev-student fmt watch watch-release release release-student echo-html-dir serve serve2 hot repl test test-quick watch-test coverage generate-coverage-html ci ci-quick dead-code dead-code-json dead-code-summary clean setup-zarith

all: dev

# Install native BigInt runtime for zarith_stubs_js to fix WebWorker postMessage serialization.
# The vendored runtime.js uses native JS BigInt (from zarith_stubs_js v0.17.0) which survives
# structured clone, unlike the BigInteger.js library used in older versions.
setup-zarith:
	@echo "Installing native BigInt zarith runtime..."
	@cp vendor/zarith_native_bigint_runtime.js "$$(opam var lib)/zarith_stubs_js/runtime.js"

deps:
	opam repo add archive git+https://github.com/ocaml/opam-repository-archive
	opam update
	opam install ./hazel.opam.locked --deps-only --with-test --with-doc
	npm install
	$(MAKE) setup-zarith

change-deps:
	opam update
	dune build hazel.opam
	opam install ./hazel.opam --deps-only --with-test --with-doc
	opam lock .
	sed -i'.old' '/host-/d' hazel.opam.locked  # remove host- lines which are arch-specific. Not using -i '' because of portability issues https://stackoverflow.com/questions/4247068/sed-command-with-i-option-failing-on-mac-but-works-on-linux

setup-instructor:
	cp src/web/exercises/settings/ExerciseSettings_instructor.re src/web/exercises/settings/ExerciseSettings.re
	cp src/web/exercises/settings/TutorialSettings_instructor.re src/web/exercises/settings/TutorialSettings.re

setup-student:
	cp src/web/exercises/settings/ExerciseSettings_student.re src/web/exercises/settings/ExerciseSettings.re
	cp src/web/exercises/settings/TutorialSettings_student.re src/web/exercises/settings/TutorialSettings.re

dev-helper: setup-zarith
	dune fmt --auto-promote || true
	dune build @ocaml-index @src/fmt --auto-promote src --profile dev

dev: setup-instructor dev-helper

dev-student: setup-student dev-helper

fmt:
	dune fmt --auto-promote

watch: setup-instructor setup-zarith
	dune build @ocaml-index @src/fmt --auto-promote src --profile dev --watch

watch-release: setup-instructor setup-zarith
	dune build @src/fmt --auto-promote src --profile release --watch

release: setup-instructor setup-zarith
	dune build @src/fmt --auto-promote src --profile release

release-student: setup-student setup-zarith
	dune build @src/fmt --auto-promote src --profile dev # Uses dev profile for performance reasons. It may be worth it to retest since the ocaml upgrade

echo-html-dir:
	@echo $(HTML_DIR)

serve:
	cd $(HTML_DIR); python3 -m http.server 8000 --bind 0.0.0.0

hot:
	npx vite

serve2:
	cd $(HTML_DIR); python3 -m http.server 8001 --bind 0.0.0.0

repl:
	dune utop src/haz3lcore

test:
	dune fmt --auto-promote || true
	dune build @ocaml-index @src/fmt @test/fmt @runtest --auto-promote --profile dev

test-quick:
	dune build @ocaml-index @src/fmt @test/fmt @test-quick --auto-promote --profile dev

watch-test:
	dune build @ocaml-index @fmt @runtest @default --profile dev --auto-promote --watch

coverage:
	dune build @src/fmt @test/fmt --auto-promote src test --profile dev
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report summary

# Report definitions that nothing references, using the .ocaml-index files dune
# already builds. See scripts/find_dead_code.py for the predicate and its limits.
# Analyses whichever ExerciseSettings/TutorialSettings variant is currently in
# place; deliberately does not run setup-instructor, which would flip a student
# checkout. Do not point this at a bisect_ppx-instrumented build.
dead-code:
	dune build @ocaml-index --profile dev
	python3 scripts/find_dead_code.py --no-build

dead-code-json:
	dune build @ocaml-index --profile dev
	python3 scripts/find_dead_code.py --no-build --format=json

dead-code-summary:
	dune build @ocaml-index --profile dev
	python3 scripts/find_dead_code.py --no-build --format=markdown

# The CI entry points. Unlike `test` / `test-quick` these never --auto-promote:
# in CI a formatting or expect-test violation should be reported, not silently
# rewritten into a checkout that gets thrown away.
#
# There is deliberately no `dune build --profile dev` here. It used to precede
# the instrumented runtest, but dev is the *lax* profile (`-warn-error -A`, see
# the (env) stanzas in src/*/dune) while release is strict, so it caught nothing
# the release build misses, and `dune runtest` builds its own dependencies.
ci: setup-zarith
	dune runtest --instrument-with bisect_ppx --force

# @test-quick runs the suite under alcotest's -q filter, skipping the
# Slow-tagged QCheck property tests that dominate the full suite's runtime.
ci-quick: setup-zarith
	dune build @test-quick --profile dev

generate-coverage-html:
	bisect-ppx-report html

clean:
	dune clean
