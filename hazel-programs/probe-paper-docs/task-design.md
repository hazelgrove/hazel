# Study Task Design

Distilled from `writing-tasks-plan.md`, `study-programs.md`, and
`probes-user-study/writing-tasks-analysis.md`.

## Bug design requirements (debugging tasks)

Each debugging program has a working version with comprehensive tests and a
buggy version with a minimal failing test set. Requirements:

- **Probe-debuggable**: discoverable by placing probes and observing values.
- **Realistic**: bugs feel like natural mistakes, not contrived puzzles; they
  compile and typecheck; many tests still pass.
- **Information balance**: tests reveal *that* something is wrong, not *what* —
  participants must explore.
- **Difficulty range**: single-character fix up to 2–3 locations of 1–2 lines.
- Architecture: MVU-style (Model, Action, `fold_left(update)`) for stateful
  programs, 150–500 lines; themed with light personality so programs are
  memorable.

Bug classes well-suited to probe debugging: wrong variable used (probe shows
unexpected indices/values); off-by-one (probe shows boundary conditions); wrong
accumulator/base case (probe shows evolution over iterations); state
transformation errors (before/after visible); condition-logic bugs (probe shows
which branch ran, `∅` for untaken).

Difficulty is rated on three axes: lines to change (1 char / one location /
multiple locations), conceptual difficulty (obvious once seen / requires
tracing / subtle interaction), and probe skill needed (one probe / navigation /
pinning + step-into).

## Writing-task categories

1. **Tiny** (1–3 lines): isolate one probe benefit in a minimal context; a
   single function body given tests; ~2–5 min.
2. **Small** (5–10 lines): multiple internal bindings, iteration via
   map/filter/fold; several probe-feedback points; ~5–10 min.
3. **Modification** (1–5 lines changed in a 20–100 line program): understand
   existing code and make a targeted change; auto-probe supports comprehension
   plus verification.

## Error patterns targeted by writing tasks

Each task targets patterns where probes help the writer notice the error:

1. **Parameter-order ambiguity** — e.g. `string_split(sep, str)` vs
   `(str, sep)`: the split result is visible immediately, no docs needed.
2. **Off-by-one** — `nth(list, 0)` vs `1`; `string_sub` bounds: the extracted
   element/substring is visible.
3. **Fold accumulator/initial-value errors** — accumulator evolution is visible
   step by step.
4. **Condition boundary errors** — `<` vs `<=`: branch taken is visible for
   boundary tests.
5. **List construction errors** — cons direction, append order, forgotten
   reverse: intermediate list state is visible.
6. **Pattern-match coverage** — missing case, wrong destructuring: which
   pattern matched and its bindings are visible.
7. **Type conversion oversights** — the actual intermediate value/type is
   visible.
8. **Scope/shadowing confusion** — the environment display shows the actual
   bindings in scope.

## Scaffolding decisions

Per task: provide the type signature when it is spec rather than puzzle;
provide a slightly-too-large list of stdlib functions (names, or names +
signatures) so discovery is part of the task; always provide tests for writing
tasks (in debugging tasks, tests instead reveal the bug's existence).

## Auto-probe granularity principle

Auto-probe places one probe per line, on the line's terminal expression — so
line-break placement determines which intermediate values are visible. Study
programs format tests to expose the actual computed value and the comparison
separately:

```
test
  function_call(args)
  == expected_value
end
```

and use one `let` binding per line so each pipeline stage yields a sample.
The same principle is taught to participants in the tutorial's auto-probe
slide.

## Task selection rationale (writing)

Analysis of candidate tasks against the RQs found the strong tasks were those
with pipeline visibility, API discovery, or accumulator evolution: mentions
extractor (split → filter → map; `string_split` arg order), running sum
(accumulator evolution across a fold), grid-extension tasks (transformations
visible while building). Too-simple tasks (clamp, safe-head: single
conditional or match, no iteration) exercise probes weakly and were kept only
as educational warmups or cut. A noted gap — no writing task exercising ADT
transformations — motivated the harvest/quality-themed tasks.

## Potential downsides monitored

1. Information overload (too many values at once).
2. Reduced planning (trial-and-error in place of thinking first).
3. Distraction (values updating while typing).
4. False confidence (correct values on test inputs ≠ correct logic).
