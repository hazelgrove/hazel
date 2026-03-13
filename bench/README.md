# Performance Benchmarks

Measures CachedSyntax and CachedStatics pipeline phases at various program
sizes. Each measurement is repeated across multiple iterations (default: 10)
and the **median** is reported.

## Scenarios

Each program size is benchmarked across four scenarios:

| Scenario | What it measures |
|:---|:---|
| **cold** | Caches cleared before each iteration |
| **warm** | Caches primed, measuring steady-state performance |
| **move** | Caches primed with original input, measuring after cursor movement |
| **modify** | Caches primed with original input, measuring after content edit |

### Pipeline phases

Phases are instrumented via `PhaseTiming` inside the library code. When
running on a branch without instrumentation, the bench harness falls back
to timing `CachedSyntax` and `CachedStatics` as whole units.

**Syntax phases** (CachedSyntax):

| Phase | What it measures |
|:---|:---|
| `syntax/Zip` | Convert zipper to segment |
| `syntax/MakeTerm` | Parse segment into AST |
| `syntax/ProjectorShapes` | Compute projector shapes |
| `syntax/Measured` | Layout measurement (line/column coordinates) |

**Statics phases** (CachedStatics):

| Phase | What it measures |
|:---|:---|
| `statics/MakeTerm` | Parse zipper for semantics |
| `statics/Stitch` | Stitch term into context |
| `statics/Statics` | Type checking |
| `statics/ErrorIds` | Collect error IDs from info map |
| `statics/WarningIds` | Collect warning IDs from info map |
| `statics/Elaborate` | Elaboration (produce DHExp) |
| `statics/Targets` | Compute probe targets |

### Program sizes

- `let100` / `let500` — let-chains with 100/500 bindings

### Cache control

Cold runs use `ResettableMemo.clear_all()` to reset all memoization caches
(including `Core.Memo.general` wrappers and `WeakMap`-based caches). Warm
runs leave caches populated from a priming pass. Move/modify runs prime
with the original input, then measure on the modified input.

## Running locally

### Quick run (current branch only)

```
dune build bench/hazel_bench.bc.js
node --stack-size=8192 --expose-gc _build/default/bench/hazel_bench.bc.js
```

Add `--json` for machine-readable output. Add `--reps N` to change
iteration count (default: 10).

### Filtering benchmarks

```
node ... --filter let500
node ... --filter cold --filter modify
node ... --filter let100/cold
```

Filters match benchmark names as substrings. Multiple `--filter` flags
are OR'd together.

### Run and store results

```
bench/run.sh                    # run, store as git note, display table
bench/run.sh --quiet            # run and store only (no table)
bench/run.sh --filter let100    # filtered run
```

Results are stored as git notes (`refs/notes/benchmarks`) keyed by commit
SHA. Retrieve with: `git notes --ref=benchmarks show <sha>`

### Compare stored results

```
bench/compare.sh                          # compare dev vs HEAD
bench/compare.sh main HEAD               # compare main vs HEAD
bench/compare.sh abc123 def456           # compare two specific commits
bench/compare.sh dev HEAD --markdown     # GitHub markdown output
```

Looks up stored benchmark results for both commits (from git notes).
If no stored results exist for a commit, exits with instructions.

### Run and compare

```
bench/run-and-compare.sh                          # run HEAD vs dev
bench/run-and-compare.sh --base main              # run HEAD vs main
bench/run-and-compare.sh --head my-branch --base dev
bench/run-and-compare.sh --filter let100          # pass filter to benchmarks
```

Runs benchmarks on both commits (using git worktrees for non-HEAD), stores
results as git notes, then compares.

## GitHub Actions (`/perf`)

Comment `/perf` on a PR to trigger the benchmark workflow. It will:

1. Build and run benchmarks on the PR branch
2. Build and run benchmarks on the base branch
3. Post a comparison table as a PR comment

The workflow lives at `.github/workflows/perf.yml`.

## Scripts

| Script | Purpose |
|:---|:---|
| `bench/run.sh` | Run benchmarks, store as git note, display table |
| `bench/compare.sh` | Look up stored results for two commits and compare |
| `bench/run-and-compare.sh` | Run benchmarks on two commits (via worktrees), then compare |
| `bench/build-and-run.sh` | Low-level: install deps, build, run benchmarks (JSON to stdout) |
| `bench/compare.js` | Low-level: compare two JSON files (`--markdown` for CI) |
| `bench/format-table.js` | Low-level: format single JSON result file as a table |
