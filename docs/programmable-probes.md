# Programmable probes

Fumola instances, as instruments for Hazel.

Status: a design note and a proposal. The integration it describes is built and
running on `experimental-lang-integration`; what is proposed here is what to do
with it next. This PR is prose only — no code.

## The claim

A Hazel probe tells you what your program computed. A Fumola instance tells you
what Hazel did.

The second sentence was not the plan. Fumola went into Hazel to give Hazel
programs an incremental store — a place to keep an expensive computation and
repair it rather than recompute it. It does that. But the property that makes
the store useful for incremental computing makes it useful for something else,
and the something else is the more interesting result:

> A Fumola instance is the first state reachable from a Hazel program that
> Hazel's own evaluator does not control.

Everything else in Hazel is downstream of the evaluator. A probe sample is
produced by the evaluation being measured and cleared along with it, so a probe
can only ever report on the program. A Fumola instance outlives the evaluation.
So it can report on the evaluation: how many times an expression ran, in what
order, under which pass, and whether the value on screen still corresponds to
anything.

For this use case the name is **notebook semantics probe** — an instrument for
the question every cell-based live environment has to answer, and that none of
them answer in writing: *when I edit this, what actually runs?*

## Two halves of a spreadsheet

Hazel and Fumola are close relatives. Both are ML-family, both have variants and
records and thunks, both take evaluation order seriously enough to have made it
a design question rather than an accident. That closeness is why the
integration is a sort in the tile grammar and a value translation, rather than a
foreign function interface.

Both also have a spreadsheet in them. They are *different halves* of one, which
is why putting them together is interesting rather than redundant.

| | Hazel's half | Fumola's half |
|---|---|---|
| The good part | **liveness** — a cell shows a value while the formula is half-written; holes do not stop evaluation; the display is continuous in the edit | **dependency** — a cell knows who read it, a change reaches exactly those readers, and recomputation is proportional to the change |
| Built around | editing, typing, rendering, responding | forcing, signaling, realignment, repair |
| What it lacks | memory: it re-runs the world on every keystroke and forgets | a surface: it knows precisely what changed and has no way to show anyone |

Excel has a weak version of Fumola's half and none of Hazel's: a half-written
formula shows an error, not a value. Jupyter has neither, and names the gap
*Restart and Run All*.

Hazel re-runs and forgets. Fumola remembers and cannot show you. Neither half is
a criticism — each is a language specialized for its domain, and the
specializations are compatible.

## What changed: the store became an instrument

We put the store in the program to make the program incremental. What we got was
an instrument that reads the editor.

The mechanism is simple enough to state in one paragraph. A `fumola <mode> as
<name> in … end` expression claims an instance *by name*, so the same name
answers with the same runtime across every edit that leaves the name alone.
Running the expression performs effects on that instance's adapton store —
nodes, edges, forces, realignments — and the store keeps them. Hazel's evaluator
decides when to run the expression, how often, on which thread, and whether to
skip it in favour of a cached result. The store records the consequences of
every one of those decisions. So the store is a log of the evaluator's
behaviour, written in a vocabulary that already has a viewer.

Two loops, one of which can now see the other:

```
Hazel      edit ──▶ elaborate ──▶ evaluate ──▶ display
                        │             │
                        └──── put ────┴──▶  ┐
                                             │   (the only arrow that
Fumola     put ──▶ signal ──▶ force ──▶ repair   survives the next edit)
                                  └──▶ store ┘
```

## What the instrument has already read

These are recorded on `experimental-lang-integration`, in
[`src/language/fumola/README.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/src/language/fumola/README.md) and in commit
messages, as things that were measured. Several of them corrected a belief that
had been held confidently first.

**A Fumola cell runs on the main thread, and the reasoning that said it could not
run at all was wrong.** Instrumenting the evaluator reported `runtime=absent
ctx=worker` and `runtime=present ctx=main-thread ms=0.7`. The first line was
read once as "so the program cannot run during evaluation." It does not say
that: the worker is a choice per cell, and a cell whose statics saw a Fumola
term is routed to `evaluate_sync` on the main thread instead. Finding that out
needed an effect to count, not a closer reading of the evaluator.

**Elaboration and evaluation are observably different times, for the same
program.** A livelit expands during elaboration; a `fumola … end` form runs
during evaluation. The difference is invisible in a pure setting and plain once
the store is there — it is what lets `hazel m end` carry the *value* `m` is
bound to rather than the variable.

**Hazel's cache means fewer effects than we assumed, not more.** A re-evaluation
that reuses a cached cell does not re-run the program. The elaboration-time
route erred the other way, re-running on nearly every keystroke. So the effect
count per edit is bracketed by the two routes and equal to neither, and nobody
had written down which one the semantics intends.

**The display and the store can disagree.** The store is mutable state outside
Hazel's incremental model, so it can move on without the displayed value
following it. This is precisely the notebook honesty problem, reproduced inside
a language that was designed to not have it — and it is now visible instead of
theoretical.

**On one slide, the editor caused 33 of 34 events.** Fixing the filter that
separates the editor's traffic from the program's took a slide from 34 events,
of which the filter wrongly left 20, down to exactly one surviving event: the
`get` from `myThunk` to `myCell`, the single thing on that slide the *program*
did. Everything else in the store was the editor being live.

That last number is a demonstration, not a measurement of Hazel. It is one small
slide, where a program has little to do and an editor has as much to do as ever,
so the ratio should be expected to be extreme and says nothing yet about a real
program. Turning it into a measurement is M1.

## Why nothing else in Hazel could have read this

Worth being precise about, because it explains why the instrument is a Fumola
instance and not something cheaper.

Hazel's evaluator is deterministic and its language is pure. Running an
expression twice is, by construction, indistinguishable from running it once —
that is the property the whole live-programming story rests on, and it is
exactly what makes the schedule unobservable from inside. Re-running is free
because it is invisible; it is invisible because it is free.

So:

- **Probes cannot do it.** A probe's samples are an output of the evaluation
  being measured. They are re-derived each time and tell you about the program,
  not about how many times the program ran.
- **Livelits cannot do it.** A livelit model persists, but expansion happens
  during elaboration, so a livelit measures the elaborator and cannot see the
  evaluator at all.
- **Logging from OCaml can do it, and does not scale.** `print_endline` in the
  evaluator is how the worker routing above was found. It requires a rebuild per
  question, it is invisible to anyone who is not running a dev build, and its
  output is not a value, so nothing in Hazel can compute with it.

A Fumola instance is the one option where the record is durable, the
instrumentation is *written in the editor*, and the result comes back as a typed
Hazel value that Hazel's own view layer can render. That last property is what
makes it an instrument rather than a debug print: the thing being measured and
the reading of the measurement share an editor.

## The correspondence

The reason "programmable probe" is the right phrase, rather than a metaphor:
Hazel's probe system and Fumola's store are already the same shape.

| Hazel probe system | Fumola | What the pairing buys |
|---|---|---|
| a probe placed on an expression | a `put` into a named cell | the sample survives the edit that produced it |
| a probe's sample list | the cell's revision history | a series across edits, not within one evaluation |
| a rich-probe renderer (`parse`/`init`/`update`/`render`) | a `thunk` reading cells | the renderer is a program in the editor, not an OCaml module, a `.rei`, a registry line and a rebuild |
| re-rendering a probe | realignment | a derived view over a long history repairs rather than recomputes |
| the call-stack navigation in `ProbeFocus` | the DCG's edges | provenance: *why* this value, not just *what* |
| `AutoProbe.All` — one probe per source row | the DCG itself | a graph already records every force and every dependency, which is a probe on everything, placed by the computation, keeping the edges a per-row probe throws away |

The last row is the one to sit with. Auto-probing every row is an approximation
of what a demanded computation graph already is, minus the structure.

## What is unresolved

Named as open, not hidden in a milestone.

**1. The composition law.** Two incremental systems, one nested in the other,
with different notions of "changed." Hazel decides whether to re-run; Fumola
decides whether to repair; neither knows about the other's decision. Three
candidate disciplines, in increasing order of how much I believe them:

- *Change Hazel's cache* so a cell containing a Fumola form is never skipped.
  Honest, and it gives up the cache exactly where programs will be slowest.
- *Make the effect count part of the semantics* — "at most once per edit that
  changes the program text" — and hold the evaluator to it.
- *Make the program idempotent and stop caring.* Adapton is already idempotent
  in the right way: forcing an aligned thunk is free, and a `put` of a value a
  cell already holds signals nobody. A program written in archivist style may
  already be safe under arbitrary re-running, in which case the composition law
  is a discipline on the Fumola program and Hazel's evaluator needs no change at
  all.

I think the third is right, and I do not think it is established. M1 and M2 are
what would establish or refute it.

**2. Effects during stepping.** The stepper runs expressions under a different
schedule again, and undo runs them backwards. Neither has been looked at.

**3. Main thread only.** The runtime is a property of `window`, so a Fumola cell
cannot use the worker, and `evaluate_sync` runs to completion rather than in
5000-step slices. Measured costs on the shipped slides are small — ~0.4 ms
median per run, ~65 ms once per instance for `claim` and `ensureMode`. A Fumola
program sharing a cell with a heavy Hazel computation blocks the UI for the
whole of it.

**4. A mode change discards the store.** `ensureMode` resets an instance when
the mode differs from the one it has. An instrument that silently loses its
history is worse than no instrument, and this is the most likely way to lose
one.

**5. Instance names are global.** Two programs that both say `store` share one
runtime. That is a feature for the panel and a hazard for anything else.

## Proposal: six milestones

Each says what lands, why it is next, and how we would know it worked. The
checks matter more than the deliverables; every one of the findings above began
as a confident belief that measurement corrected.

### M1 — The effect schedule, written down

A corpus whose only job is to count. One instance, one cell, one `put` per
visit, and a rendered count — then a table: for each editor gesture (a keystroke
inside the cell, a keystroke elsewhere, a cell added, a reload, a step, an undo,
a focus change), how many effects, in what order, under which pass.

*Why now.* It needs no new machinery, and everything else in this document
assumes an answer to it that nobody has.

*How we would know.* The table is produced by running the instrument and reading
the panel, never by reading `Evaluator.re` — and it is reproducible by someone
else from the document alone. A row we cannot reproduce is a finding about the
instrument, which is just as valuable at this stage.

*Deliverable.* `docs/hazel-effect-schedule.md`, explicitly measured rather than
derived, with the build and the runtime version it was measured against.

### M2 — Attribution by declaration, not by heuristic

Today the editor's own traffic is separated by a property of the store's shape:
an edge is the editor's if its source is the root, `(Here, Now, _)`. That works
and it is an inference. Make it a declaration instead — the run carries which
pass it came from, so an event can say *elaboration*, *evaluation*, *stepper*,
*panel peek*.

*Why now.* M1 can count effects but cannot fully say *why* each one happened,
only that the editor rather than the program caused it. Without attribution the
schedule table has a column it cannot fill.

*How we would know.* On a slide with a schedule established in M1, every event
is attributed and the per-pass counts match. The heuristic and the declaration
are then run against each other, and the interesting result is a disagreement.

### M3 — A probe that survives the edit

The first new user-facing capability: a probe whose samples accumulate across
edits, because they live in a named instance rather than in an evaluation. "The
last fifty values this expression took, as I edited it" is an observation Hazel
cannot currently make at all.

The design question is *who names the series*, and the answer should be the one
already settled one level up: the user names it, in the source. A series keyed
by `Id.t` loses its history on exactly the edits worth watching — this is the
same failure the instance name was introduced to avoid, and it recurs here for
the same reason.

*How we would know.* Edit the probed expression; the earlier samples are still
there, still labelled with the program text that produced each one. Rename the
series; the history starts over, visibly and on purpose.

### M4 — A rich probe written in Fumola

`RichProbe` becomes an interface a Fumola thunk can implement, instead of an
OCaml module plus a mandatory `.rei` plus a registry line plus a rebuild. This
is the livelit argument applied to observation rather than to input, and it is
the milestone that earns the word *programmable*.

*How we would know.* A new renderer is added, and rendered, without rebuilding
Hazel. The existing `TableRenderer` is the control: the Fumola version of it
should be recognisably the same program and should handle the same values.

### M5 — Provenance over the user's own computation

Point the Nodes and Edges views at a user's instance rather than at a demo, with
navigation from a sample back to the edges that produced it. *Why is this value
this value* becomes a question the editor can answer with a path rather than a
number.

*How we would know.* On a computation whose dependency structure we wrote down
in advance, the path the panel shows is the path the program takes. Prefer an
example where the obvious answer is wrong — a memoized call whose result came
from a revision older than the edit that appears to have caused it.

### M6 — Scenes as Hazel values

DCG scenes rendered through Hazel's own view layer rather than a bespoke panel,
starting with the List rung. This is where the two halves finally close: the
graph that Fumola maintains incrementally is displayed by the system that is
good at displaying things, as an ordinary value of an ordinary type.

*How we would know.* The scene is a Hazel value with a declared Hazel type, and
the panel is one renderer over it rather than the only way to see it.

## The question the milestones are really asking

M1 and M2 are instrumentation, and they are also the empirical part of a claim
worth making in public: **an operational account of Hazel's cell semantics,
derived by measuring the implementation rather than by reading it.**

Live programming environments are specified by their front ends and
under-specified by their schedules. Notebooks are the well-known case, and the
usual response is to observe that notebooks are bad. Hazel is a system built
specifically to not have that problem, with a real semantics and a real type
system behind it — and the effect schedule of its cells was still, until this
month, an open empirical question that nobody had the instrument to ask.

Having the instrument is the contribution. It is also a nice inversion to be
able to state: incremental computing was supposed to be the payload, and it
turned out to be the microscope.

## What this does not propose

- **Changing Hazel's evaluator.** The instrument's job is to describe the
  schedule. If the schedule turns out to be wrong, that is a separate decision
  needing separate evidence, and M1 is what would supply it.
- **Retiring Hazel's probes.** They answer a different question — what the
  program computed — and they answer it better, with no store to name and
  nothing to survive.
- **A type system for Fumola.** Examples first. The contracts that matter here
  belong in this document and in the round-trip checks.
- **Merging the branch chain.** `fumola-tiles-mvp` → `fumola-tiles-and-livelits`
  → `experimental-lang-integration` remains a demo track. What lands on `dev`,
  and when, is a separate conversation that this document is trying to inform
  rather than pre-empt.

## Background

Built and running on `experimental-lang-integration`:

| Where | What |
|---|---|
| [`src/language/fumola/README.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/src/language/fumola/README.md) | the grammar, the printer contract, running, the event list, the parser and the round trip |
| [`docs/fumola-tiles-design.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/docs/fumola-tiles-design.md) | why Fumola is a tile sort, and why the instance is named in the syntax |
| [`docs/fumola-runtime-changes.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/docs/fumola-runtime-changes.md) | the unpinned wasm dependency, and the six exports Hazel actually calls |
| [`docs/rich-probes.md`](rich-probes.md) | the renderer plug-in layer M4 would open to Fumola |
| [`docs/livelits.md`](livelits.md) | the elaboration-time route, and what it cannot see |
