# Evaluator benchmark: js_of_ocaml vs wasm_of_ocaml

**Status: SPIKE. Not for merge.** This branch trades correctness of the
livelit feature for speed of getting a number. See "What this gives up".

## The question

Hazel's evaluator is compute-bound and already runs off the UI thread in a
Web Worker. Wasm GC is typically faster than JS for allocation-heavy OCaml,
but slower across JS boundary crossings. The Worker is nearly pure compute,
so it is the part of Hazel most likely to benefit. This measures whether it
actually does.

## The constraint that shapes everything

`virtual_dom` v0.16 requires `js_of_ocaml < 6.0.0`. `wasm_of_ocaml` is only
released from 6.0.1 onward. **They cannot coexist in one opam switch.**

Everything below follows from that:

- the benchmark links only `language` + `menhirParser`, never `haz3lcore`
  or `web`;
- parsing goes through `menhirParser` rather than the zipper path the CLI
  uses, because `PersistentZipper`/`MakeTerm` live in `haz3lcore`;
- `language` had to be made Bonsai-free, which is what the `util` /
  `util_web` split and the livelit removal are for;
- the measurement runs in a second opam switch that has no Bonsai at all.

## Running it

```
bench/wasm/setup-switch.sh          # one-time; ~20-60 min of compiling
bench/wasm/run.sh hazel-wasm 20
```

`run.sh` builds both backends from the *same* compiler version (6.2.0) and
runs both on node 22, so a measured delta is the backend and not the
toolchain. Node 22 is required: Wasm GC did not ship until well after
node 18, which is what this repo otherwise builds against.

Output is the JSON shape `bench/compare.js` already consumes, so:

```
bench/wasm/run.sh hazel-wasm 20 > head.json
node bench/compare.js base.json head.json --markdown
```

## What this gives up

To break `language`'s dependency on Virtual_dom, this branch:

- **deletes the three builtin livelits** (`src/language/Livelit.re`) and the
  `view` field of `LivelitCtx.raw_livelit`. `Ctx.LivelitEntry` still exists
  so its pattern matches typecheck, but nothing populates it, and
  `LivelitProj` renders a placeholder. `Test_Evaluator_Livelit` is removed.
- **splits `util`** into `util` (pure + js_of_ocaml only) and `util_web`
  (Bonsai/Virtual_dom). `Util_web` re-exports all of `Util`, so consumers
  mostly changed `open Util` to `open Util_web`.
- `JsUtil` and `Key` were each split rather than moved, keeping the
  js_of_ocaml-only majority in `util`. That alone avoided ~48 files of churn.

The `util` split is worth keeping. The livelit deletion is not — the
principled version registers livelit views from the web layer instead.

## RESULT: ~4x faster, on fixed-precision workloads

Both backends built from js_of_ocaml **6.2.0**, run on node 22, 20
iterations. **Result checksums match across backends**, so this measures the
same computation rather than a silently-stuck one.

| workload | js_of_ocaml | wasm_of_ocaml | speedup |
|---|---|---|---|
| `sint-fib` plain | 1050 ms | 285 ms | 3.7x |
| `sint-fib` incr | 1053 ms | 276 ms | 3.8x |
| `float-numeric` plain | 290 ms | 74 ms | 3.9x |
| `float-numeric` incr | 303 ms | 71 ms | 4.3x |
| `sint-list` plain | 168 ms | 42 ms | 4.0x |
| `sint-list` incr | 162 ms | 41 ms | 3.9x |

The differences *between* workloads are not resolvable from this data: across
two runs the ordering flipped (`sint-list` was the lowest ratio in one and
the highest in the other). The ~4x headline is solid; do not read a profile
effect into the breakdown without more runs.

### Why fixed precision

Hazel's `Int` and `Nat` are `Bigint`-backed, and bignum is exactly what
cannot compile to Wasm here -- 27 `ml_z_*` primitives, plus the Jane Street
Core C-stub surface behind it. `SInt` (machine int) and `Float` avoid it
entirely, so the workloads in `bench/fixed-*.hz` use only those.

Two things make this work:

  * `BigIntWasmStub.re`, swapped in by `run.sh`, drops bignum. Construction
    and comparison stay total because `Builtins.ctx_init` needs them to
    boot; **arithmetic raises**, so a workload that reaches Int or Nat fails
    loudly rather than quietly benchmarking 63-bit math.
  * `Core.Memo.general` in `Statics.re` was `language`'s *only* use of Jane
    Street Core. Replacing it with `Util.Memo` removed Core from the
    evaluator's cone altogether.

### Still blocked: arbitrary precision

Full-fidelity Hazel in Wasm still needs a bignum. The most promising route
is the one Motoko took -- a bignum compiled to Wasm, linked against the
compiler output. The wrinkle is that wasm_of_ocaml represents OCaml values
as **Wasm GC** structs while a C bignum lives in **linear memory**, so
unlike Motoko (which controlled both sides) this needs a copy-in/copy-out
shim at the boundary. wasm_of_ocaml's `.wat` runtime hook looks like the
seam to try.

### Note on recursion depth

Hazel's evaluator is explicitly trampolined (`Trampoline.re`): object-language
call depth lives in a heap-allocated callstack, not the host stack. Measured
under js_of_ocaml, both tail and non-tail Hazel recursion reach depth 20000+.
Deep recursion is therefore *not* a backend risk, and the workloads here are
sized for compute volume rather than to dodge a stack limit.

## Caveat on the numbers

`eval-bench.hz` is one workload: binder-heavy recursion, list traversal and
case matching. It is a reasonable proxy for the Worker's job, but it is not
a spread. Treat the result as a first signal, not a characterization — and
note that under `wasm_of_ocaml` the OCaml heap is managed by the browser's
GC rather than OCaml's, so allocation behavior differs in kind, not just
degree.

## Isolation

`run.sh` edits `bench/wasm/dune` in place (restoring it on exit) and builds
into `_build-wasm` rather than the shared `_build`, so it will not force a
full rebuild of your normal switch's artifacts. If you are working on other
branches in this checkout at the same time, run it from a linked worktree:

```
git worktree add --detach ../hazel-wasm-bench wasm-eval-bench-spike
cd ../hazel-wasm-bench && bench/wasm/run.sh hazel-wasm 20
```

Remove it afterwards with `git worktree remove ../hazel-wasm-bench`.
