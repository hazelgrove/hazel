# Performance Benchmarks

Measures key editor operations at various program sizes using
[core_bench](https://github.com/janestreet/core_bench).

## Benchmarks

### Edit Cycle (per-keystroke latency)

These measure the realistic cold path — what happens on every keystroke.
Each iteration inserts a character, producing a fresh segment with new UUIDs,
so all memoization caches miss.

| Benchmark | What it measures |
|:---|:---|
| `Insert` | Action phase only (insert character into zipper) |
| `Insert+CachedSyntax` | Insert + MakeTerm + Measured rebuild (dominant keystroke cost) |
| `Insert+Full` | Insert + CachedSyntax + Statics + Elaboration (complete pipeline) |
| `Move(Left)` | Cursor move for reference (should be near-zero) |

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

## Running locally

### Quick run (current branch only)

```
dune build bench/hazel_bench.bc.js
node --stack-size=8192 _build/default/bench/hazel_bench.bc.js
```

Add `--json` for machine-readable output.

### Comparison against a base branch

```
bench/run-comparison.sh          # compare against dev (default)
bench/run-comparison.sh main     # compare against main
bench/run-comparison.sh abc123   # compare against a specific commit
```

This uses the current branch's benchmark code for both branches (matching
CI behavior), checks out the base as a detached HEAD (to avoid worktree
conflicts), and prints a comparison table.

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
| `bench/compare.js` | Compare two JSON result files (`--markdown` for CI) |
| `bench/run-comparison.sh` | Local orchestration (checkout, run both, compare) |
