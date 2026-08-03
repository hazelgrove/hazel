HTML_DIR="$(shell pwd)/_build/default/src/web/www"
SERVER="http://0.0.0.0:8000/"

.PHONY: all deps change-deps pin-opam-repos update-opam-pins setup-instructor setup-student dev dev-helper dev-student fmt watch watch-release release release-student echo-html-dir serve serve2 repl test clean setup-zarith ci ci-quick

all: dev

# Install native BigInt runtime for zarith_stubs_js to fix WebWorker postMessage serialization.
# The vendored runtime.js uses native JS BigInt (from zarith_stubs_js v0.17.0) which survives
# structured clone, unlike the BigInteger.js library used in older versions.
setup-zarith:
	@echo "Installing native BigInt zarith runtime..."
	@cp vendor/zarith_native_bigint_runtime.js "$$(opam var lib)/zarith_stubs_js/runtime.js"

OPAM_PINS = .github/opam-pins.env

# Point this switch's opam repositories at the commits recorded in $(OPAM_PINS).
#
# hazel.opam.locked pins which package *versions* the solver picks. It cannot pin
# their *metadata*: opam has no content hashes, and upstream routinely edits
# already-published versions in place. In June 2026 such an edit made
# conf-libssl.4 require conf-pkg-config >= 5 while our lock file pinned
# conf-pkg-config = 4, breaking every build with no change on our side. Pinning
# the repository commit closes that hole -- see issue #2334.
#
# Scoped to the current switch (opam's default for `repo add`), so the shared
# `default` repository definition is untouched and other OCaml projects on this
# machine are unaffected. `default` stays below these as a fallback.
pin-opam-repos:
	@set -e; . "$(OPAM_PINS)"; \
	arch="git+https://github.com/ocaml/opam-repository-archive#$$OPAM_REPOSITORY_ARCHIVE_SHA"; \
	main="git+https://github.com/ocaml/opam-repository.git#$$OPAM_REPOSITORY_SHA"; \
	opam repo add archive "$$arch" --rank 1 2>/dev/null || opam repo set-url archive "$$arch"; \
	opam repo add hazel-locked "$$main" --rank 2 2>/dev/null || opam repo set-url hazel-locked "$$main"; \
	opam repo list

# Move $(OPAM_PINS) forward to each repository's current HEAD.
# The awk filter is load-bearing: `ls-remote <url> HEAD` also matches the stale
# refs/remotes/origin/HEAD that opam-repository publishes, so without it you get
# two SHAs and a malformed URL.
update-opam-pins:
	@set -e; \
	main=$$(git ls-remote https://github.com/ocaml/opam-repository HEAD | awk '$$2 == "HEAD" { print $$1 }'); \
	arch=$$(git ls-remote https://github.com/ocaml/opam-repository-archive HEAD | awk '$$2 == "HEAD" { print $$1 }'); \
	for sha in "$$main" "$$arch"; do \
	  echo "$$sha" | grep -qE '^[0-9a-f]{40}$$' || { echo "Not a single commit sha: '$$sha'" >&2; exit 1; }; \
	done; \
	sed -i'.old' \
	  -e "s|^OPAM_REPOSITORY_SHA=.*|OPAM_REPOSITORY_SHA=$$main|" \
	  -e "s|^OPAM_REPOSITORY_ARCHIVE_SHA=.*|OPAM_REPOSITORY_ARCHIVE_SHA=$$arch|" \
	  $(OPAM_PINS); \
	rm -f $(OPAM_PINS).old; \
	grep -E '^OPAM_' $(OPAM_PINS)

# Reproduce: pinned repositories + the lock file. Identical to what CI installs.
deps: pin-opam-repos
	opam update
	opam install ./hazel.opam.locked --deps-only --with-test --with-doc
	npm install
	$(MAKE) setup-zarith

# Update: move the pins to the current heads, repin, then re-lock against them,
# so hazel.opam.locked and $(OPAM_PINS) always describe the same repository
# state. The repin is required -- without it this would resolve against the old
# pinned commit and silently find nothing new.
change-deps:
	$(MAKE) update-opam-pins
	$(MAKE) pin-opam-repos
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
