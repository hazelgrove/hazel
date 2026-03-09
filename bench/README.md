# Performance Benchmarks

Measures key editor operations at various program sizes using
[core_bench](https://github.com/janestreet/core_bench).

## Benchmarks

### Edit Cycle (per-keystroke pipeline phases)

Each phase of the editor pipeline is benchmarked individually using
pre-computed inputs from the previous phase. Iteration 1 is a cold call;
subsequent iterations may hit memo caches since the inputs are fixed.

The comparison output includes a computed **Total** row per program size
that sums all pipeline phases.

| Phase | What it measures |
|:---|:---|
| `Perform` | Insert a character into the zipper (action phase) |
| `MakeTerm` | Parse segment into AST |
| `Measured` | Layout measurement (line/column coordinates) |
| `Statics` | Type checking |
| `Elaborate` | Elaboration (produce DHExp for evaluation) |
| `Evaluate` | Program evaluation |
| **Total** | Sum of all phases above (computed by compare.js) |

### Memo-hit overhead

These measure the hot path — repeated calls with identical inputs.
Memoization caches always hit. Useful for understanding the cost of
cache lookup itself (e.g., re-rendering without edits).

| Benchmark | What it measures |
|:---|:---|
| `MakeTerm.go` | Memo lookup cost for term construction |
| `Measured.of_segment` | Memo lookup cost for layout measurement |
| `Statics.mk` | Memo lookup cost for type checking |
| `Elaborator.elaborate` | Memo lookup cost for elaboration |

### Program sizes

- `let100` / `let500` — let-chains with 100/500 bindings (~5-10 AST nodes each)
- `case100` — nested case expressions with 100 functions (~15 AST nodes each)

### GC stabilization

`Gc.compact()` is run between each benchmark test (via core_bench's
`~stabilize_gc_between_runs` flag) to prevent GC pressure from one
benchmark contaminating measurements of the next.

## Running locally

### Quick run (current branch only)

```
dune build bench/hazel_bench.bc.js
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js
```

Add `--json` for machine-readable output.

### Filtering benchmarks

```
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --filter let500
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js --filter Statics --filter Elaborate
```

Filters match benchmark names as substrings (case-sensitive). Multiple
`--filter` flags are OR'd together.

### Comparison against a base branch

```
bench/run-comparison.sh                              # compare against dev
bench/run-comparison.sh main                         # compare against main
bench/run-comparison.sh abc123                       # compare against a commit
bench/run-comparison.sh dev --filter Statics         # filtered comparison
bench/run-comparison.sh dev --filter memo --filter let500
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
| `bench/compare.js` | Compare two JSON result files, compute totals (`--markdown` for CI) |
| `bench/run-comparison.sh` | Local orchestration (checkout, run both, compare) |
