HTML_DIR="$(shell pwd)/_build/default/src/web/www"
SERVER="http://0.0.0.0:8000/"

.PHONY: coverage-check all deps change-deps setup-instructor setup-student dev dev-helper dev-student fmt watch watch-release release release-student echo-html-dir serve serve2 hot repl test test-quick watch-test coverage generate-coverage-html ci dead-code dead-code-json dead-code-summary clean setup-zarith

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

ci: setup-zarith
	dune build --profile dev
	dune runtest --instrument-with bisect_ppx --force

generate-coverage-html:
	bisect-ppx-report html

# Guard: every .re/.ml under src/ must appear in the coverage report, except the
# files grouped below. Run after `make coverage`, which produces the data.
#
# This exists because a file ABSENT from the report is not a file at 0% -- it is
# in neither the numerator nor the denominator. bisect_ppx registers a module's
# points when the module initialises, and OCaml drops library modules that
# nothing references. So a library which loses its
# `(instrumentation (backend bisect_ppx))` stanza breaks no test: it silently
# stops being measured and the summary reads as though nothing is wrong.
# `--expect` turns that into an error.
#
# Deliberately NOT solved with `-linkall`. That works -- it forces unreferenced
# modules into the link so they report at 0%, cutting these exclusions from 44
# files to 14 -- but `library_flags` is profile-independent, so those modules
# would ship in `make release` too, and on util/language/haz3lcore it adds 5.5MB
# to worker.js (measured), which the browser refetches after a worker respawn.
# Shipping dead code to improve a metric is the wrong trade. This list is the
# honest version of the same information. See docs/coverage.md.

# Not a library, so the test binary cannot depend on it. Untested by choice.
COVERAGE_SKIP_CLI = --do-not-expect src/CLI/

# Entry points: excluded from the web library by design, nothing to cover.
COVERAGE_SKIP_ENTRY = \
  --do-not-expect src/web/Main.re \
  --do-not-expect src/web/Worker.re

# Build variants: only whichever copy is in place gets compiled, never both.
COVERAGE_SKIP_VARIANTS = \
  --do-not-expect src/web/exercises/settings/ExerciseSettings_instructor.re \
  --do-not-expect src/web/exercises/settings/ExerciseSettings_student.re \
  --do-not-expect src/web/exercises/settings/TutorialSettings_instructor.re \
  --do-not-expect src/web/exercises/settings/TutorialSettings_student.re

# Nothing to instrument. These declare rather than compute -- module types,
# module aliases, bare type definitions, constant data, or (two of them) an empty
# file. bisect_ppx instruments expressions, so these have no coverage points, and
# `open Foo` for a signature is a compile-time dependency that generates no
# runtime reference for the linker to follow. NOT dead code: StepInterface has 8
# referrers and Drv is named on ~2000 lines. No test can cover them, and forcing
# them into the link makes them report a vacuous 100% (0/0), which inflates the
# total rather than measuring anything.
COVERAGE_SKIP_NO_CODE = \
  --do-not-expect src/web/app/editors/stepper/StepInterface.re \
  --do-not-expect src/web/app/sidebar/DebugSection.re \
  --do-not-expect src/language/derivation/Drv.re \
  --do-not-expect src/web/PersistentData.re \
  --do-not-expect src/web/view/agentCore/AgentResult.re \
  --do-not-expect src/language/term/FreeVariables.re \
  --do-not-expect src/web/app/editors/stepper/AssumptionView.re \
  --do-not-expect src/web/exercises/Specs.re \
  --do-not-expect src/web/exercises/examples/BlankCodeExercise.ml \
  --do-not-expect src/web/exercises/examples/BlankDerivationExercise.ml \
  --do-not-expect src/web/exercises/examples/BlankTheoremExercise.ml \
  --do-not-expect src/haz3lcore/CompositionCore/ToolJsonDefinitions/ReadTools.re

# Real code that nothing references, so it is compiled but never linked. THESE
# are the dead-code candidates -- cross-check with `make dead-code` before adding
# one. src/pretty is an entire library nothing references (its MemoTbl is
# signature-only, but the whole directory is excluded here).
COVERAGE_SKIP_UNREFERENCED = \
  --do-not-expect src/pretty/ \
  --do-not-expect src/util/BonsaiUtil.re \
  --do-not-expect src/util/Either.re \
  --do-not-expect src/util/FloatingElement.re \
  --do-not-expect src/util/Monads.re \
  --do-not-expect src/util/StateMonad.re \
  --do-not-expect src/util/Unicode.re \
  --do-not-expect src/web/app/LogEntry.re \
  --do-not-expect src/web/app/input/FailedInput.re \
  --do-not-expect src/web/debug/DebugMode.re \
  --do-not-expect src/web/exercises/ExerciseUtil.re

COVERAGE_NOT_EXPECTED = \
  $(COVERAGE_SKIP_CLI) \
  $(COVERAGE_SKIP_ENTRY) \
  $(COVERAGE_SKIP_VARIANTS) \
  $(COVERAGE_SKIP_NO_CODE) \
  $(COVERAGE_SKIP_UNREFERENCED)

coverage-check:
	bisect-ppx-report summary --expect src/ $(COVERAGE_NOT_EXPECTED)

clean:
	dune clean
