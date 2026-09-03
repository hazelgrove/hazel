# Testing and debugging projectors

Notes from building the html rich probe, written down because most of the time
went into *finding out what was wrong* rather than fixing it. Read this before
reaching for a browser.

## `test/EditorCycle.re` — drive the editor, not the browser

The harness for anything that commits syntax. It runs the editor's own update
cycle in-process, so a test can ask for *the term the editor would have sent to
the worker* and then run it the way the worker runs it:

| Function | What it gives you |
|---|---|
| `of_text` | a `CodeWithStatics.Model.t` with statics calculated the editor's way |
| `calculate(~is_edited)` | the pass the editor runs after every edit |
| `perform(action, model)` | `Perform.go` + that pass, i.e. one turn of the cycle |
| `probe_info(model, id)` | the `info` a probe's renderer actually receives, via `RefractorView.mk_data` — its `syntax` is unparenthesized, trimmed and re-parenthesized, *not* the raw `TermData` segment |
| `refractor_idx` | the index `Action.SetSyntax` wants |
| `request(model)` | `(elaborated, EvalInfo.t)` — from `CachedStatics`, not a fresh `Statics.mk` |
| `evaluate_as_worker(~prev)` | `start_yielding_evaluation` + `run_yielding_slice` at the worker's own 5000-step budget |

A full round trip is then: `of_text` → `probe_info` → the renderer's real
`commit_syntax` → `perform(Project(SetSyntax(...)))` → `error_ids` →
`evaluate_as_worker`. Every step is the editor's own code.

Two details it exists to stop you getting wrong, both of which cost us a day:
`info.syntax` is not the raw segment, and the msg a handler dispatches is the
value sitting in the *rendered* tree (post-substitution inside its enclosing
function), not the same expression evaluated at top level.

## Test the seam, not the screen

Almost everything a projector does is checkable in-process. `test/Test_HtmlRenderer.re`
covers the commit path at nine levels in ~10 seconds:

| Level | What it uses |
|---|---|
| parsed syntax | `Parser.to_segment` |
| the commit shape | a pure `handler_syntax` / `spliced` split, so no `info` is needed |
| the real lift | `ProjectorInfo.utility.lift_syntax` — the editor's own function and settings |
| document + selection | `ProjectorPerform`'s refractor steps, then `Measured.of_segment` |
| evaluation | `elaborate` + `evaluate` |
| incremental | `EvaluatorState.get_incr_eval` threaded as `~prev` |
| probe-instrumented | `EvalInfo.of_targets(targets_of_zipper(...))` |
| sliced | `start_yielding_evaluation` + `run_yielding_slice(~step_budget=5000)` |
| the whole cycle | `EditorCycle` — real `info`, real `commit_syntax`, real `Perform.go`, `CachedStatics`, worker-style slices |

Two things made that possible and are worth copying:

**Split a pure core out of anything that needs `info`.** `info` is expensive to
fabricate, so a function taking it is effectively untestable. Take the one thing
you need from it as a parameter instead — `handler_syntax(~bound: string => bool, msg)`
rather than reading `info.statics` inline — and the projector keeps a thin
wrapper that supplies it.

**Reproduce the editor's assertions, not its rendering.** The crash we chased was
`Highlight.of_tile: shard mismatch`, raised while drawing. The assertion behind
it is arithmetic — a tile with N children must have N+1 measured shards — and
`Measured.find_shards` is callable from a test. Copy the invariant; don't drive
the thing that checks it.

Slide text is reachable too: `Charts.Slides.all_slides` gives `backup_text`, so a
test can run the *real* program rather than a hand-written analogue. Ours passed
on a small analogue and failed on the real slide.

## A test that can't fail

Three of our id tests passed with the fix reverted, because:

- they compared ids across **two separate parses**, which mint disjoint ids by
  construction. Parse once, then `elaborate`/`evaluate` *that term*.
- they used `exp_to_segment` with `CoreSettings.on`, where the editor uses
  `any_to_segment` with `CoreSettings.off` plus overrides. If you are testing a
  path the editor takes, call the editor's function.

**Always revert the fix and watch the test fail.** Every one of those would have
been caught in thirty seconds.

## What a browser costs

- **Coordinates are scaled.** The click tool's space was 1336x920 while the
  viewport was 1504x1036 — a 0.888 factor. Coordinates from
  `getBoundingClientRect` land ~12% off, silently. Compute
  `1336 / window.innerWidth` and scale, or take coordinates off a screenshot.
  A mis-scaled click that hits nothing is indistinguishable from a click whose
  handler did nothing, and we mis-attributed a crash to the commit path because
  of it.
- **`element.click()` does not fire vdom handlers.** Real events only.
- **Chrome caches the 87MB `hazel.js`.** A rebuild is not picked up by a plain
  reload; `fetch(url, {cache:'reload'})` for `hazel.js` and `worker.js`, then
  reload. Stale-worker symptoms look like bugs in whatever you just changed.
- **Persisted state survives.** Doc slides are stored in IndexedDB, so an edit
  from an earlier session is still there. Clear `localStorage` and
  `indexedDB.deleteDatabase` before believing anything.
- **Grep the page for your own fixture.** We "confirmed" a commit by finding
  `calc(0) |> pressDigit(_, "1")` in the page text — it was in the slide's own
  inline tests. Check the specific element or the tail of the program.
- **Refractors are viewport-culled**, so a probe below the fold renders nothing.

## Costs that show up as bugs

Two performance cliffs in the html/probe path present as breakage, not slowness:

- **An annotation naming a builtin recursive alias** (`HTML`, `Attr`, `Cmd`,
  `Sub`) drags the expanded sum type into every closure that reaches the
  binding. One calculator pad went from 24KB to 1.2MB, and the editor reported
  "Evaluation timed out" — which reads as an infinite loop.
- **Handler closures are closed syntax.** Evaluation substitutes environments
  away, so a handler value carries the transitive closure of every helper it
  references: 60KB for one key. Splicing an anonymous handler into the source
  costs that per press.

Both are worth measuring with `./hazel run <file> | wc -c` before assuming a
logic error.

## The bug that outlived nine levels — and why

**Symptom.** A handler written as a deferred application (`pressDigit(_, "1")`)
commits correctly — the source really does become
`(calc(0) |> pressDigit(_, "1"))` — and the program then evaluates to an
*indeterminate* `setState(...)` in the editor and stays there, surviving a slide
switch. The equivalent named handler worked. `./hazel run` on the same committed
text reduced it. A page reload fixed it.

**Root cause.** `IncrEval.reuse_check` handed back a cached value that was a
bare `Fun` — no `Closure` wrapper, so no environment.

`Transition`'s `Closure` rule evaluates subterms with `~in_closure`, and
`wrap_closure_when_done` therefore *suppresses* the `Closure` wrapper for a
function value nested under a `Closure`. That is correct in place: the enclosing
`Closure` supplies the environment. But a cache entry is keyed by id alone and is
replayed at top level (`reuse_check` requires `call_stack.stack == []`), where
that enclosing `Closure` is gone. Applying the replayed function then lands in

```reason
| FunNoEnv(dp, d3) when mode == `Substitution => …step…
| FunNoEnv(_) => Indet          // ← environment mode: silently final
```

so the application is final-but-stuck and never reduces again. The fix is a
`carries_env` guard in `reuse_check`: don't reuse a value that carries no
environment of its own.

**Why the harness missed it — the actual lesson.** `EditorCycle.request` built
its `eval_info` with `EvalInfo.of_targets(model.statics.targets)`. The editor
builds it with `EvalInfo.of_info_map(~probe_all, ~targets, statics.info_map)`.
Only `of_info_map` populates the per-id `statics` field (`elab_term`, `co_ctx`,
`probe_targets`) — and `reuse_check` begins with
`EvalInfo.find_opt(id, eval_info)`, so with `of_targets` **reuse never happens at
all**. Nine levels of harness were evaluating with the incremental evaluator
switched off, and no amount of extra levels below that would have found it.

Two ingredients are each necessary, which is why nothing simpler reproduced it:
a non-empty `prev`, and an `eval_info` built the way the editor builds it.

Ruled out along the way, all in-process: segment well-formedness
(`Highlight.of_tile`'s own assertion), document id reuse (`dup_ids=0` in the live
editor), the real `lift_syntax`, ProjectorPerform's refractor branch including
the leftover selection, plain evaluation, the worker's sliced loop at its own
5000-step budget, streaming, and `Statics.Map.error_ids`. Forcing the browser
onto the main-thread `evaluate_sync` path still reproduced it, which exonerated
the worker, slicing and streaming in one move — worth doing early next time.

**What made it debuggable in the end**, in order of value:

1. **Instrument the real thing and read the console.** `prerr_endline` reaches
   `console.error` from jsoo, and `read_console_messages` with a `pattern` reads
   it. One build that printed what `RichProbe.parse` was rejecting settled a
   question three rounds of structural reasoning had not.
2. **Force the sync path** (`queue_worker = None` in `ScratchMode`) to delete the
   worker, slicing and streaming from the picture.
3. **Bisect by disabling, not by reading.** `reuse_check |> always None` turned
   "reuse is involved somehow" into a fact in one run.
4. **Compare the harness's inputs against the real call site field by field.**
   The bug was in an argument the harness constructed differently, not in the
   code under test.

Two traps that cost real time and will recur:

- **A second parse mints disjoint ids.** `parse_exp(program)` produces a term
  whose ids cannot collide with the model's, so a msg taken from it exercises a
  commit the browser never performs. Take the pad from the model's own
  evaluation.
- **`use` is a reserved word.** A `.hz` program that binds it fails to parse, and
  `./hazel run` reports the failure as a line of echoed source that reads exactly
  like a stuck evaluation. Six "reproductions" of a nonexistent `DeferredAp` bug
  came from that. Read the whole output, not `tail -2`.

## Still missing

`WorkerServer`'s update function has no test coverage: request scheduling,
`is_latest` abandonment and streaming are only exercised through the browser.
