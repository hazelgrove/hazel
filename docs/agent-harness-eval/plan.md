# Plan

Ordered so that each step is usable on its own. Effort: S < half a day,
M about a day, L several days.

## Phase 0: make the primitives trustworthy

1. **Fix `hazel test` exit codes** (B1, B2, B3). S.
   Non-zero on zero tests, on any indeterminate test, and on fast-parse
   failure. `SLOW PARSE` to stderr.
2. **Add `hazel test --json`.** S.
   Serialize `TestResults.t` (already derives yojson) so the scorer never
   scrapes English prose.
3. **Fix the two prompt bugs** (B4, B5). S.
   Filbert is one arm of the comparison; it should not be handicapped by
   a bad path example or have extra tools in converse mode.

## Phase 1: one shared language reference

4. **Move the two stray language sections into `HazelSyntaxNotes`.** S.
   `CompositionPrompt.re:125-145` (program shape) and `:348-356` (comments).
   Delete the three editor sentences at `HazelSyntaxNotes.re:4-5, 208`.
   `CompositionPrompt.self` keeps the same output modulo those moves.
5. **Decide on `HazelDocumentation.re`** (N1). S.
   Recommended: strip its five editor sentences and append it to the shared
   ref. Otherwise delete it. Either way it stops being dead code.
6. **Emit the language ref as a file.** S.
   A `hazel lang-ref` subcommand (or a dune rule) that prints
   `HazelSyntaxNotes.self` to stdout, so the external harness and Filbert
   read the same bytes. `Test_PromptFactory` keeps validating it.

## Phase 2: the external-harness "editor" addendum

7. **Write `docs/agent-harness-eval/cli-addendum.md`.** S.
   What an outside agent is told: your program is one `.hz` file in this
   folder; `hazel analyze` (typecheck), `hazel test` (run tests),
   `hazel run` (evaluate), `hazel probe` with `^^probe(e)`; what each exit
   code means. Do **not** mention hidden tests or the scorer.
8. **Sandbox-friendly CLI entry.** S.
   A prebuilt `cli.bc.js` plus a thin `hazel` shim on `PATH` that skips
   `dune build`, so an agent restricted to a scratch folder can run it.
9. **Rewrite `src/CLI/README.md`** (N10). S.

## Phase 3: exercises on disk

10. **Exercise extractor.** M.
    From each `CodeExercise.spec` emit
    `exercises/<name>/task/{prompt.md, starter.hz}` (agent-visible) and
    `exercises/<name>/hidden/{tests.hz, solution.hz}` (never mounted).
    Stitching order must match `CodeExercise.stitch_term`.
11. **Scorer script.** M.
    `analyze` must exit 0; then
    `cat result.hz hidden/tests.hz | hazel test --json -` must have
    `total > 0 && failing == 0 && unfinished == 0`. Emit one JSONL row per
    run: exercise, arm, model, harness, pass/fail, counts, wall time, tokens
    if available.

## Phase 4: run both arms

12. **External runner.** M.
    Script: copy `task/` to a scratch dir, launch the harness (opencode / pi
    first, OpenRouter key) with system prompt = lang-ref + cli-addendum,
    write access limited to the scratch dir, timeout, then score.
13. **Filbert arm.** M.
    Extend `src/CLI/AgentEval.re` (currently on `cost-display-refinements`)
    to load `starter.hz` into the editor, feed `prompt.md` as the user turn,
    drain, print the resulting program, and hand it to the same scorer.
14. **Decide the feedback-loop asymmetry.** Discussion, not code.
    Filbert sees typecheck/test results only in the next context snapshot;
    the outside agent gets them from the command it ran. Either accept it as
    part of "action language" or add a Filbert tool that returns
    `analyze`/`test` output directly.

## Phase 5: results

15. **Results format + dashboard.** M.
    Reuse the `caching-eval` JSONL + CSV pattern; a small static HTML or
    markdown table generator like `bench/compare.js` is enough to start.

## Cleanup (any time)

16. N2-N9, N11, N12 from `bugs.md`. S each.
