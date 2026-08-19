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

## Still missing

One bug outlived every level above, and the shape of the search is the lesson.

**Symptom.** A handler written as a deferred application (`pressDigit(_, "1")`)
commits correctly — the source really does become
`(calc(0) |> pressDigit(_, "1"))` — and the program then evaluates to an
*indeterminate* `setState(...)` in the editor and stays there. The equivalent
named handler works. `./hazel run` on the same committed text reduces it.

**Ruled out, all in-process:** segment well-formedness (`Highlight.of_tile`'s own
assertion), document id reuse, the real `lift_syntax`, ProjectorPerform's
refractor branch including the leftover selection, plain evaluation, `IncrEval`
reuse, probe instrumentation via `EvalInfo.of_targets`, the worker's sliced
loop at its own 5000-step budget, sliced *and* reusing the previous pass's
`IncrEval`, and `Statics.Map.error_ids` on the committed program. The real slide
text, not an analogue, at every level.

`EditorCycle` was built to close the two gaps that were then outstanding — the
editor's `CachedStatics` elaboration and the worker's sliced evaluation — and it
covers both. **It still does not reproduce this.** With the real `info`, the real
`commit_syntax`, the msg taken out of the rendered pad, the commit performed as
an editor action, and evaluation sliced at the worker's budget, the committed
program evaluates to html.

So whatever is left is outside everything the harness models: not the commit,
not elaboration, not evaluation. That points at the live UI layer — view
construction, `HazelDOM`'s dispatch, or `WorkerServer`'s request scheduling
(`is_latest` abandons superseded requests; one abandoned without a replacement
completing would leave the last streamed partial on screen forever, which looks
exactly like a stuck evaluation).

The next thing to build, if it comes back: drive `WorkerServer`'s update
function over a scripted request sequence, so abandonment and streaming are
observable. That is the one layer with no test coverage at all.
