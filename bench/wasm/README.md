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

## RESULT: blocked on zarith

The Wasm module **builds and loads**, then fails at link with 35 missing
primitives. About thirty of them are `ml_z_*` -- zarith.

Hazel's `Int` and `Nat` are arbitrary-precision: `language` reaches
`Bigint` from six files, including `BuiltinsBase.re`, where the integer
builtins are defined. So zarith is load-bearing for the *semantics*, not an
incidental dependency that could be dropped for a benchmark.

zarith has no C implementation available to `wasm_of_ocaml` here; under
`js_of_ocaml` it is satisfied by `zarith_stubs_js`, a hand-written JS
runtime (which Hazel further patches -- see `make setup-zarith`). Handing
those JS runtimes to the wasm build via `(wasm_of_ocaml (javascript_files
...))` does help -- it takes the missing list from 54 to 35, resolving the
`bin_prot`, `expect_test_collector` and most `Base_*` stubs -- but the bulk
of the zarith primitives remain.

**This is the finding.** Compiling Hazel's evaluator to Wasm is blocked on
arbitrary-precision integers, not on anything about the evaluator itself.
Clearing it means one of:

  1. a `wasm_of_ocaml`-compatible zarith runtime (upstream work, or port
     `zarith_stubs_js` to the wasm calling convention);
  2. building real zarith/GMP for Wasm and linking it;
  3. a Wasm-native bigint backend for `Bigint`, e.g. over JS `BigInt`.

Note this same class of problem -- hand-written stubs not carrying across
backends -- is what would also block an Internet Computer port, where there
is no JS host to fall back on at all.

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
