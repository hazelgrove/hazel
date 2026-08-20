# Coverage

```
ALCOTEST_QUICK_TESTS=1 make coverage   # instrument, run tests, print the summary
make generate-coverage-html            # writes _coverage/index.html
make coverage-check                    # guard: nothing silently dropped out
```

`ALCOTEST_QUICK_TESTS=1` is the env-var form of the `-q` the `test-quick` alias
passes, so the QCheck property tests are skipped and the run takes ~50s instead
of several minutes. Anything covered mainly by property tests therefore reads
low — notably `menhirParser/AST.re` and `Parser.ml`, which the fuzz and corpus
tests do exercise.

## Absent is not 0%

A file missing from the report is not a file at 0%, and the difference is easy to
read as good news.

bisect_ppx registers a module's coverage points when that module initialises. A
module that is never linked never initialises, so its points are never registered
and the file is in **neither the numerator nor the denominator** — it is absent,
not zero. OCaml drops library modules that nothing references, so "nobody calls
this" and "this is fully covered" look identical in the summary: both are silence.

44 of 519 files are absent for this reason. They are listed explicitly in
`COVERAGE_NOT_EXPECTED` in the Makefile, in four groups:

| group | why |
|---|---|
| `src/CLI/` | an `executable`, so the test binary cannot depend on it. Untested by choice. |
| `Main.re`, `Worker.re` | app entry points, excluded from the `web` library by design |
| `ExerciseSettings_*`, `TutorialSettings_*` | build variants; only whichever copy is in place gets compiled |
| the remaining 23, incl. all of `src/pretty/` | unreferenced modules: compiled, never linked |

**Every entry in the last group is a dead-code candidate.** `src/pretty` is an
entire library nothing references; `Either`, `Monads`, `StateMonad`, `BonsaiUtil`,
`FreeVariables` and `Drv` are unreferenced today. Cross-check with
`make dead-code` before adding to that group — a file landing there is a signal,
not paperwork.

## `make coverage-check`

```
bisect-ppx-report summary --expect src/ $(COVERAGE_NOT_EXPECTED)
```

`--expect` is a check, not a filler: it errors on a file that should be in the
report and is not. It will **not** manufacture 0% rows for unlinked files.

What it buys is protection against the silent failure this whole page is about. A
library that loses its `(instrumentation (backend bisect_ppx))` stanza breaks no
test — it just stops being measured, and the summary looks fine. Verified by
deleting that stanza from `docslides`: the guard fails with
`expected file 'src/docslides/Slides.re' is not included in the report`. The
exclusion list is load-bearing too — dropping one entry makes the guard error on
the file it covered.

## Why not `-linkall`

`(library_flags (:standard -linkall))` forces every module of a library into the
link, so unreferenced ones register their points and appear at 0%. It works: on
`web`, `pretty`, `docslides` and `b2t2` it takes the report from 477 to 505 of
519 files.

It is not used here, on purpose. `library_flags` is profile-independent, so those
modules would ship in `make release` too — the metric improves by shipping dead
code to users. And on `util`, `language` or `haz3lcore` the cost is much worse:
those are linked by the worker, and forcing their unreferenced modules in drags
web-only code and its virtual_dom dependencies along, taking `worker.js` from
22.4MB to 27.9MB (measured). The browser refetches that after a worker respawn.

Two related dead ends, recorded so they are not retried:

- As an **executable** `link_flags`, `-linkall` also pulls in third-party
  archives and dies on an assertion in `ConcurrentUnionFind`'s initialiser.
- Dune's `env` stanza does not support `library_flags`, so there is no clean way
  to scope `-linkall` to a coverage profile. It would need `(:include)` plus a
  profile-gated rule in each library.

Note this is OCaml's `-linkall`, not js_of_ocaml's `--linkall`, which the dev
profile already passes and which does something else.

## Reading the numbers

Overall is ~42%. Per-file numbers understate the update layer, because `Model`,
`Update` and `View` share one file and no view code is tested: `Page` reads 9%
while the `calculate` fan-out it exists for is covered and mutation-checked.
Splitting `View` out per component would make the percentages mean something —
see docs/ui-architecture.md.

`make coverage` runs `dune build @src/fmt @test/fmt --auto-promote` first. If
anything needs reformatting it promotes the fix and exits non-zero, so the run
stops before doing any work. That is not a failure — run it again.

Do not run `make -p` to inspect variables: with no explicit target it also runs
the default goal, which rebuilds and deletes the `.coverage` data.
