# Coverage

```
ALCOTEST_QUICK_TESTS=1 make coverage   # instrument, run tests, print the summary
make generate-coverage-html            # writes _coverage/index.html
make coverage-check                    # guard: nothing silently dropped out
```

`ALCOTEST_QUICK_TESTS=1` is the env-var form of the `-q` that the `test-quick`
alias passes, so the QCheck property tests are skipped. Without it the run takes
several minutes instead of ~50s. The numbers below were measured with it, so
anything covered mainly by property tests reads low — notably
`menhirParser/AST.re` and `Parser.ml`, which the fuzz and corpus tests do
exercise.

## Absent is not 0%

A file missing from the report is not the same as a file at 0%, and the
difference is easy to misread as good news.

bisect_ppx registers a module's coverage points when that module initialises. A
module that is never linked never initialises, so its points are never
registered and the file does not appear in the report **at all** — it is not in
the numerator or the denominator. OCaml drops library modules that nothing
references, so "nobody calls this" and "this is fully covered" look identical
from the summary: both are silence.

Two mechanisms address the two halves of that.

### `-linkall` — make unreferenced modules appear

`(library_flags (:standard -linkall))` forces every module of a library into the
link, so unreferenced ones register their points and show up at 0%. It is on for
`web`, `pretty`, `docslides` and `b2t2`, which takes the report from 477 to 505
of 519 files.

It is deliberately **not** on `util`, `language` or `haz3lcore`. Those are linked
by the worker, and forcing their unreferenced modules in drags web-only code and
its virtual_dom dependencies along: measured, `worker.js` goes from 22.4MB to
27.9MB. The browser refetches and reparses that file after a worker respawn, so
it is not a cost worth three files. The four libraries it *is* on are not linked
by the worker; they cost `hazel.js` +0.25% and `worker.js` nothing.

Note this is OCaml's `-linkall`, not js_of_ocaml's `--linkall`, which the dev
profile already passes and which does something else.

### `--expect` — catch files dropping out

`make coverage-check` runs

```
bisect-ppx-report summary --expect src/ $(COVERAGE_NOT_EXPECTED)
```

`--expect` is a check, not a filler: it errors on a file that should be in the
report and is not. That covers the failure this whole section is about — a
library losing its `(instrumentation (backend bisect_ppx))` stanza breaks no
test, it just quietly stops being measured. Verified by deleting that stanza from
`docslides`: the guard fails with `expected file 'src/docslides/Slides.re' is not
included in the report`.

`COVERAGE_NOT_EXPECTED` in the Makefile is the allowlist of files that
legitimately cannot appear:

| exclusion | why |
|---|---|
| `src/CLI/` | an `executable`, so the test binary cannot depend on it |
| `Main.re`, `Worker.re` | app entry points, excluded from the `web` library by design |
| the rest (9 files) | unreferenced modules in `util`/`language`/`haz3lcore`, where `-linkall` costs worker bundle size |

**Every line in that allowlist is a dead-code candidate.** `Either`, `Monads`,
`StateMonad`, `BonsaiUtil`, `FreeVariables`, `Drv` are unreferenced today; the
reason they are excluded is that shipping them to make a metric look complete is
the wrong trade. Cross-check with `make dead-code` before adding to the list.

To cover `src/CLI/` for real it would have to be split into a library plus a thin
executable. That is worthwhile on its own merits — `hazel run`, `hazel test` and
`hazel grade` are user-facing and have no coverage — but it is a build change,
not a reporting one.

## Reading the numbers

Overall is ~42%, and per-file numbers understate the update layer, because
`Model`, `Update` and `View` share one file and no view code is tested. `Page`
reads 9% while the `calculate` fan-out it exists for is covered and
mutation-checked. Splitting `View` out per component would make the percentages
mean something; see docs/ui-architecture.md.

`make coverage` runs `dune build @src/fmt @test/fmt --auto-promote` first. If a
dune or source file needs reformatting it promotes the fix and exits non-zero, so
the run stops before doing anything. That is not a failure — run it again.
