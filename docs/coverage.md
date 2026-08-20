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

42 of 517 files are absent, and they are grouped in the Makefile by *why*,
because the reason determines what you would do about it — and for most of them
the answer is nothing.

| group | n | why | action |
|---|---|---|---|
| unreachable root | 7 | reachable only from `src/CLI/`, `Main.re` or `Worker.re`, none of which the test binary links | link an entry point, not write a test |
| build variants | 4 | only whichever `ExerciseSettings`/`TutorialSettings` copy is in place gets compiled | none possible |
| nothing to instrument | 9 | module types, aliases, bare types, constant data | none possible |
| opted out | 1 | `[@coverage exclude_file]` in the source | none — intentional |
| dead | 21 | real code nothing references, incl. all of `src/pretty/` | **delete** |

Only the last row is actionable, and `make dead-code` independently reports every
function in those files as dead — two tools agreeing is a good signal to act on.
`DebugMode` and `Specs` were in that group until traced properly: `DebugMode` is
reached by `Main.re` via the `#debug` fragment and `Specs` by `CLI/Grade.re`, so
labelling them dead would have pointed someone at deleting live code.

### Worked example: `StepInterface.re`

Worth spelling out, because it is the case that looks most like a coverage gap
and is least like one.

`src/web/app/editors/stepper/StepInterface.re` is 114 lines and absent from the
report. It is not dead: eight modules `open` it, and **all eight are in the
report** — `StepperBase`, `SingleStep`, `AxiomStep`, `AlgebriteStep`,
`ForallStep`, `InductionStep`, `InductionCase`, `StepperTargetBox`. So the
obvious diagnosis, "nothing links it because nothing uses it", is wrong.

What the file contains is two `module type` declarations (`STEP` and `STEPPER`)
and nothing else — zero value definitions. Two consequences:

1. A `module type` generates no runtime code, so `open StepInterface` is a
   compile-time dependency only. There is no runtime reference for the linker to
   follow, so the module is compiled and never linked.
2. bisect_ppx instruments *expressions*. There are none, so there are no coverage
   points to register even if it were linked.

**So nothing upstream would fix this.** Linking it (via `-linkall`) does make it
appear — at `100.00%`, from `0/0` points. That is worse than absent: it is a
vacuous 100% padding the file count while measuring nothing.

`Drv.re` is the same shape and makes the point harder to miss: five lines of
`module Exp = DrvTerm.Exp;`-style aliases, named on roughly two thousand lines
across the tree, and equally uncoverable. `FreeVariables.re` and
`AssumptionView.re` are one byte each — genuinely empty files, and the only
members of this group that are safe to simply delete.

## `make coverage-check`

```
bisect-ppx-report summary --expect src/ $(COVERAGE_NOT_EXPECTED)
```

`--expect` is a check, not a filler: it errors on a file that should be in the
report and is not. It will **not** manufacture 0% rows for unlinked files.

It also does **not** respect `[@coverage exclude_file]`. That attribute stops
bisect_ppx measuring a file, but the file then fails `--expect` like any other
absentee, so an intentionally-unmeasured file needs *both* the attribute and a
`--do-not-expect` entry. `src/util/Unicode.re` is the one file in that state.

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

`_coverage/` is not cleaned between runs, so a stale page can outlive the data
that produced it — after changing what gets linked, `rm -rf _coverage` before
regenerating, or the file count you read will be the previous run's. (`index.html`
and the generated pages agree at 477 on a clean run; they disagreed by 36 when I
forgot.)

`make coverage-check` reads the `.coverage` data `make coverage` leaves behind,
so it has to follow it. A plain `make` or `./run_tests` in between rebuilds
without instrumentation and the data goes away — the guard then fails with
`no *.coverage files found`, which means "re-run coverage", not "a file is
missing". Nor should you run `make -p` to inspect these variables: with no
explicit target it runs the default goal, which has the same effect.
