# Performance Benchmarking Guide

## Quick Start

```bash
cd /Users/andrewblinn/Dropbox/projects/hazel-projector-html

# Build and run benchmarks (~10 seconds)
dune build bench/bench.bc.js && node _build/default/bench/bench.bc.js

# Run tests to verify correctness (skip if only reading numbers)
dune build && node _build/default/test/haz3ltest.bc.js 2>&1 | grep FAIL
```

## How It Works

`bench/bench.re` is a standalone executable (separate from the test suite) that times each pipeline phase independently:

1. **Parse**: `Parser.to_term(str)` — tile editor path (char-by-char insertion)
2. **Statics**: `Statics.mk(settings, ctx, term)` — type checking, info map
3. **Elaborate**: `Elaborator.elaborate(info_map, term)` — produces DHExp
4. **Evaluate**: `Evaluator.evaluate(~env, elaborated)` — produces result

Each phase is timed separately using `performance.now()` (high-res). Parse is timed once; other phases are averaged over multiple iterations.

## Key Performance Knobs

### Typ.normalize cache (`src/language/term/Typ.re` line ~606)

```reason
let normalize_cache_enabled = true;  (* toggle this *)
```

- **ON**: Huge elaboration speedup (287ms → 6ms for counter), but causes 57 test failures due to unsound caching (keyed by type ID, ignores context)
- **OFF**: Correct but slow elaboration
- **Fix needed**: Make cache context-aware (key on `(id, ctx_hash)` or similar)

## Iteration Loop

1. Make a change (e.g., toggle cache, optimize a function)
2. `dune build bench/bench.bc.js && node _build/default/bench/bench.bc.js`
3. Compare numbers to baseline
4. If change looks good, run tests: `dune build && node _build/default/test/haz3ltest.bc.js 2>&1 | grep FAIL`
5. Expected: 1 failure (flaky Pattern Coverage test 45, node stack size issue)

## Adding Benchmarks

Edit `bench/bench.re`. Add program strings and call `bench("name", program)`:

```reason
let my_program = {|let x = 1 in x + 2|};
let () = bench("my_program", my_program);
```

For .hz files, paste their contents as inline strings.

## Baseline Results (2026-02-06)

All times in milliseconds. Programs sorted by size.

### Cache OFF (correct, current default)
```
program              parse    statics     elab     eval    total
---------------------------------------------------------------
simple_let            18.5      0.3      0.1      0.2     19.1
fibonacci             26.5      0.7      0.8      7.2     35.1
counter              195.3     41.3    421.0      0.6    658.1
mvu_counter          767.5     41.0    418.9     21.0   1248.4
keyboard_game        968.0     32.9    253.4     28.0   1282.3
animation           2816.0     15.4    320.6      1.6   3153.5
full_app            9313.3    942.3   1857.2     24.5  12137.4
```

### Cache ON (buggy, 57 test failures)
```
program              parse    statics     elab     eval    total
---------------------------------------------------------------
simple_let            17.7      0.3      0.1      0.1     18.2
fibonacci             22.7      0.5      0.1      4.4     27.7
counter              133.3     30.9      7.3      0.9    172.4
mvu_counter          502.2     44.9     15.2     21.4    583.7
keyboard_game        951.9     24.6     17.8     19.8   1014.2
animation           1259.5     12.5     21.8      2.8   1296.5
full_app            8537.4    179.1     31.1     23.7   8771.3
```

### Speedup from cache (cache OFF → ON)
```
program           elab speedup    statics   total speedup
---------------------------------------------------------
counter              58x          0.7x         3.8x
mvu_counter          28x          1.1x         2.1x
keyboard_game        14x          0.7x         1.3x
animation            15x          0.8x         2.4x
full_app             60x          5.3x         1.4x
```

### Analysis

**Bottleneck #1: Parsing** — dominates for all real programs
- full_app: 9.3 seconds to parse 5.7KB (1.6ms/byte!)
- animation: 2.8 seconds for 2.5KB
- The tile editor parser inserts chars one-by-one — O(n²) or worse
- This is the interactive parse path; menhir would be much faster but isn't used here

**Bottleneck #2: Elaboration** (without cache) — 60x slower without cache for full_app
- full_app: 1857ms → 31ms with cache (60x)
- counter: 421ms → 7ms with cache (58x)
- The elaborator calls `Typ.normalize` heavily; cache eliminates redundant work

**Bottleneck #3: Statics** — moderate, cache helps for large programs
- full_app: 942ms → 179ms with cache (5.3x)
- Smaller programs: statics is 15-41ms regardless of cache

**Evaluation is negligible** — always <30ms

### Priority Fixes (focus: statics + elab + eval, NOT parse)

Parse is a separate concern (will be addressed by menhir/incremental approach).
The interactive loop doesn't re-parse — it re-runs statics/elab/eval on each edit.

1. **Understand what's slow in statics and elaboration**
   - User reports ~500ms statics in browser for MVU counter, mostly in `Typ.meet`
   - Benchmark shows 41ms for statics — discrepancy may be due to browser term structure
   - `Typ.meet` on `Sum` types calls `ConstructorMap.meet` which traverses all constructors
   - HTML type has 40+ constructors — every meet of two HTML types is O(n) per constructor
   - Need to profile precisely before optimizing

2. **Fix normalize cache soundness** — big elab win
   - Elaboration: 60x speedup (1857ms → 31ms for full_app)
   - Bug: cache keyed by type ID, ignores context
   - Location: `src/language/term/Typ.re` line ~606, toggle: `normalize_cache_enabled`
   - Options: (a) key on `(id, ctx_hash)`, (b) scope-aware invalidation, (c) clear per-scope

3. **Large sum type performance** — systemic issue
   - HTML = 40+ variant sum type, Attr = 30+ variants, Sub/Cmd = 10+ each
   - Operations that are O(n) per constructor: `meet`, `normalize(Sum)`, `ConstructorMap.meet`
   - Potential approaches:
     - **Hash-based constructor maps** instead of association lists
     - **Memoize meet results** for builtin types (HTML meet HTML is always the same)
     - **Type identity tags** — mark builtin types so meet can short-circuit (HTML == HTML → skip traversal)
     - **Lazy normalization** — don't normalize until needed
   - What other compilers do: TypeScript uses structural type identity with caching; Elm uses nominal typing for ADTs (constructor lookup by name in scope); Rust uses DefId for nominal identity

## Profiling from Command Line

### Node.js CPU profiling with source maps

The bench executable is built with `--debuginfo --noinline --sourcemap` (configured in `bench/dune`).

```bash
# Generate a CPU profile (V8 format)
node --prof _build/default/bench/bench.bc.js
# Process the profile
node --prof-process isolate-*.log > profile.txt

# Or use Chrome DevTools protocol:
node --inspect-brk _build/default/bench/bench.bc.js
# Then open chrome://inspect in Chrome, connect, and use the Performance tab

# Or use clinic.js for flamegraphs:
npx clinic flame -- node _build/default/bench/bench.bc.js
```

### Manual instrumentation

Already available: `Util.TimeUtil.timed("label", () => expr)` wraps any expression with timing.
Add calls around suspected hotspots in `Statics.re`, `Typ.re`, `Elaborator.re`.

Key functions to instrument:
- `Typ.meet` (line ~735 in Typ.re) — called heavily during statics
- `Typ.normalize` (line ~623) — called during elaboration
- `ConstructorMap.meet` — called for every Sum type meet
- `Statics.re` main traversal — find where meet is called most

## Key Files

- `bench/bench.re` — standalone benchmark executable
- `bench/dune` — build config with source maps
- `src/language/term/Typ.re` — normalize (line ~623), meet (line ~735), cache toggle (line ~606)
- `src/language/statics/Statics.re` — main statics traversal
- `src/haz3lcore/derived/CachedStatics.re` — browser entry point for statics+elab+eval
- `src/language/builtins/BuiltinsADT.re` — HTML/Attr/Sub/Cmd type definitions (lines 201+)
- `src/util/TimeUtil.re` — timing utilities
