# Performance Benchmarks

Measures key editor operations at various program sizes using single-shot
timing with `performance.now()`. Each measurement is repeated across
multiple runs (default: 7) with structurally unique inputs, and the
**median** is reported.

## Scenarios

Each program size is benchmarked across four scenarios:

| Scenario | What it measures |
|:---|:---|
| **cold** | First run with fresh input — no memoization cache hits |
| **warm** | Immediate re-run with identical input — measures cache-hit overhead |
| **move** | After `Move(Left)` cursor movement — incremental update cost |
| **modify** | After `Insert("x")` content edit — incremental update cost |

### Pipeline phases

| Phase | What it measures |
|:---|:---|
| `Perform` | Execute the action on the zipper (move/modify only) |
| `MakeTerm` | Parse segment into AST |
| `Measured` | Layout measurement (line/column coordinates) |
| `Statics` | Type checking |
| `Elaborate` | Elaboration (produce DHExp for evaluation) |
| `Evaluate` | Program evaluation |
| **Total** | Sum of all phases above |

### Program sizes

- `let100` / `let500` — let-chains with 100/500 bindings (~5-10 AST nodes each)
- `case100` — nested case expressions with 100 functions (~15 AST nodes each)

### Cache isolation

Each program is parsed once and then cloned with fresh IDs per repetition
(via `Segment.IDs.replace_piece`). Fresh IDs ensure `Core.Memo.general`
(structural equality) misses across repetitions, and fresh allocations
ensure `WeakMap`-based caches (physical identity) also miss. This avoids
the cost of re-parsing large programs each repetition.

## Running locally

### Quick run (current branch only)

```
dune build bench/hazel_bench.bc.js
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js
```

Add `--json` for machine-readable output. Add `--reps N` to change
repetition count (default: 10).

### Filtering benchmarks

```
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --filter let500
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --filter cold --filter modify
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --filter let100/cold
```

Filters match benchmark names as substrings (case-sensitive). Multiple
`--filter` flags are OR'd together.

### Comparison against a base branch

```
bench/run-comparison.sh                              # compare against dev
bench/run-comparison.sh main                         # compare against main
bench/run-comparison.sh abc123                       # compare against a commit
bench/run-comparison.sh dev --filter cold             # filtered comparison
```

Requires a clean worktree (no uncommitted changes). Uses the current
branch's benchmark code for both branches (matching CI behavior), checks
out the base as a detached HEAD (to avoid worktree conflicts), and prints
a comparison table. Restores the head branch and its dependencies on exit.

## GitHub Actions (`/perf`)

Comment `/perf` on a PR to trigger the benchmark workflow. It will:

1. Post a comment with a link to the running workflow
2. Build and run benchmarks on both the base and PR branches
3. Update the comment with a comparison table

The workflow lives at `.github/workflows/perf.yml` and must exist on the
repo's default branch to be triggered by PR comments.

## Shared scripts

Both local and CI flows use the same underlying scripts:

| Script | Purpose |
|:---|:---|
| `bench/build-and-run.sh` | Install deps, build, run benchmarks (JSON to stdout) |
| `bench/compare.js` | Compare two JSON result files, group by scenario (`--markdown` for CI) |
| `bench/run-comparison.sh` | Local orchestration (checkout, run both, compare) |
