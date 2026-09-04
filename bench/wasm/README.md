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

## RESULT: blocked on missing C stubs

The Wasm module **builds and loads**, then fails at link with **53 missing
primitives**:

| group | count | what it is |
|---|---|---|
| `ml_z_*` | 27 | zarith (arbitrary-precision integers) |
| `Base_*` | 19 | Jane Street Base: hashing, int math intrinsics |
| `bin_prot_*` | 2 | Bin_prot blit stubs |
| `expect_test_collector_*` | 2 | Jane Street test collector |
| other | 3 | `caml_csel_value`, `caml_out_channel_pos_fd`, `time_now_*` |

Passing the existing JS runtimes to the wasm build via `(wasm_of_ocaml
(javascript_files ...))` made **essentially no difference** (54 to 53). Dune
accepts the field, but the stubs were not satisfied; whether the files are
honoured at all was not confirmed. The copy rules are left in place because
they are the right shape for a fix, not because they worked.

The root cause is that `bignum` -- needed because Hazel's `Int` and `Nat`
are arbitrary-precision, with `language` reaching `Bigint` from six files
including `BuiltinsBase.re` -- drags in zarith *and* the whole Jane Street
`Core`/`Base` C-stub surface. Under `js_of_ocaml` these are satisfied by
hand-written JS runtimes (`zarith_stubs_js`, which Hazel further patches --
see `make setup-zarith`). Those do not port to `wasm_of_ocaml`.

**This is the finding.** Compiling Hazel's evaluator to Wasm is blocked on
its numeric tower and the Jane Street runtime beneath it, not on anything
about the evaluator itself. Clearing it means one of:

  1. wasm-compatible runtimes for all 53, most plausibly by porting the
     existing JS stubs to wasm_of_ocaml's calling convention;
  2. building real zarith/GMP for Wasm and linking it -- this addresses the
     27 `ml_z_*` but not the 26 Jane Street ones;
  3. removing `bignum` from the evaluator's cone: a `Bigint` backend over
     JS `BigInt`, which would drop zarith *and* Core in one move.

Option 3 looks the most tractable and is the only one that also helps an
Internet Computer port -- where this same class of problem is worse, since
there is no JS host to fall back on at all.

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
