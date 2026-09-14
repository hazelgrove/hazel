# Notebook semantics

What actually runs when you edit, and how we found out.

Status: a design note and a proposal. The instrument it describes is built and
running on `experimental-lang-integration`, and the observations it reports were
made there. What is proposed here is how to turn them into a result. Prose only
— no code.

Companion document: [Programmable probes](programmable-probes.md), which is the
same instrument pointed at a user's program rather than at the editor.

## The question

Every cell-based live environment has to answer one question, and none of them
answer it in writing:

> When I edit this, what actually runs?

Jupyter's answer is *Restart and Run All* — an admission that the notebook's
visible state and the kernel's actual state have diverged and that nothing short
of starting over can reconcile them. Excel's answer is a dependency graph it
does not show you, over a formula language deliberately too small to have a
schedule worth asking about.

Hazel was built so as not to have that problem. Holes do not stop evaluation,
statics keep running on a program that does not type, and the value on screen is
supposed to correspond to the program on screen at all times. That is the
design. Whether the *implementation's* effect schedule matches it is a separate,
empirical question — how many times an expression actually runs per edit, under
which pass, on which thread, and whether a cached display still corresponds to
anything.

Until this month there was no way to ask.

## Why Hazel cannot answer it about itself

This is the crux, and it is worth being precise about, because it explains why
the instrument had to come from outside the language.

Hazel's evaluator is deterministic and its language is pure. Running an
expression twice is, by construction, indistinguishable from running it once.
That is the property the whole live-programming story rests on — it is what
makes it safe to re-evaluate on every keystroke — and it is exactly what makes
the schedule unobservable from inside. Re-running is free because it is
invisible; it is invisible because it is free.

So none of the obvious instruments work:

- **Probes cannot do it.** A probe's samples are an *output* of the evaluation
  being measured, re-derived each time and cleared with it. A probe tells you
  what the program computed, not how many times the program ran.
- **Livelits cannot do it.** A livelit model persists across edits, but
  expansion happens during *elaboration*. A livelit measures the elaborator and
  cannot see the evaluator at all.
- **Logging from OCaml can do it, and does not scale.** `print_endline` in the
  evaluator is how the worker routing below was first found. It costs a rebuild
  per question, it is invisible to anyone not running a dev build, and its
  output is not a value, so nothing in Hazel can compute with it.

What is needed is state that a Hazel program can reach and that Hazel's
evaluator does not control. Before Fumola, there wasn't any.

## The instrument

A `fumola <mode> as <name> in … end` expression claims a Fumola VM instance *by
name*, so the same name answers with the same runtime across every edit that
leaves the name alone. Running the expression performs effects on that
instance's adapton store — nodes, edges, forces, realignments — and the store
keeps them. Hazel's evaluator decides when to run the expression, how often, on
which thread, and whether to skip it in favour of a cached result. The store
records the consequence of every one of those decisions.

So the store is a log of the evaluator's behaviour, written in a vocabulary that
already has a viewer: `FumolaHistory` reads an instance's events, nodes and
edges back as ordinary Hazel values of declared Hazel types, and the Fumola
sidebar renders them. The thing being measured and the reading of the
measurement share an editor.

Two loops, one of which can now see the other:

```
Hazel      edit ──▶ elaborate ──▶ evaluate ──▶ display
                        │             │
                        └──── put ────┴──▶  ┐
                                             │   (the only arrow that
Fumola     put ──▶ signal ──▶ force ──▶ repair   survives the next edit)
                                  └──▶ store ┘
```

The inversion is worth stating plainly, because it is not what the integration
was for. Fumola went into Hazel to give Hazel programs an incremental store.
Incremental computing was supposed to be the payload; it turned out to be the
microscope.

## What the instrument has already read

Recorded on `experimental-lang-integration`, in
[`src/language/fumola/README.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/src/language/fumola/README.md)
and in commit messages, as things that were measured. Three of the five
corrected a belief that had been held confidently first, which is the strongest
evidence available that the instrument is doing work.

**A Fumola cell runs on the main thread, and the reasoning that said it could
not run at all was wrong.** Instrumenting the evaluator reported `runtime=absent
ctx=worker` and `runtime=present ctx=main-thread ms=0.7`. The first line was
read once as "so the program cannot run during evaluation." It does not say
that: the worker is a choice per cell, `EvalResult.calculate` takes an optional
worker queue, and a cell whose statics saw a Fumola term is routed through
`evaluate_sync` on the main thread instead. Two things travel by that route for
the same reason — the runtime is a property of `window`, and the typing context
is full of OCaml closures, so neither crosses `postMessage`.

**Elaboration and evaluation are observably different times, for the same
program.** A livelit expands during elaboration; a `fumola … end` form runs
during evaluation. In a pure setting the difference is invisible. With a store
it is plain, and it is what lets `hazel m end` carry the *value* `m` is bound to
rather than the variable.

**Hazel's cache means fewer effects than assumed, not more.** A re-evaluation
that reuses a cached cell does not re-run the program at all. The
elaboration-time route erred the other way, re-running on nearly every
keystroke. So the effect count per edit is bracketed by the two routes and equal
to neither — and nobody has written down which one the semantics intends.

**The display and the store can disagree.** The store is mutable state outside
Hazel's incremental model, so it can move on without the displayed value
following it. This is the notebook honesty problem exactly, reproduced inside a
system built specifically not to have it. It is now visible rather than
theoretical, which is the point.

**On one slide, the editor caused 33 of 34 events.** Fixing the filter that
separates the editor's own traffic from the program's took a slide from 34
events — of which the filter wrongly left 20 — down to exactly one surviving
event: the `get` from `myThunk` to `myCell`, the single thing on that slide the
*program* did. Everything else in the store was the editor being live.

That last number is a demonstration, not a measurement of Hazel. It is one small
slide, where a program has little to do and an editor has as much to do as ever,
so an extreme ratio is what should be expected and it says nothing yet about a
real program. Turning it into a measurement is N1.

## What the instrument still cannot attribute

The store can say *that* the editor caused an event rather than the program. It
cannot yet say *which pass* — elaboration, evaluation, a stepper step, the
panel's own peek.

The separation today is an inference about the store's shape: an edge is the
editor's if its source is the root, `(Here, Now, _)`. That works, and it took
two corrections to get right. The first judged an event by the edge it named,
which left every event that names a *node* unattributed on the reasoning that a
node is neither the editor's doing nor the program's — it simply exists. That
reasoning is right about the node and wrong about the event: a cell signals
because someone put into it, so a cell the editor put into signals on the
editor's account.

A heuristic that needed two corrections to arrive at one right answer on one
slide is not a foundation for a measurement. N2 is replacing it with a
declaration.

## The composition law

The open question, named rather than buried in a milestone.

Two incremental systems, one nested inside the other, with different notions of
"changed." Hazel decides whether to re-run; Fumola decides whether to repair;
neither knows about the other's decision. Three candidate disciplines, in
increasing order of how much I believe them:

1. **Change Hazel's cache** so a cell containing a Fumola form is never skipped.
   Honest, and it gives up the cache exactly where programs will be slowest.
2. **Make the effect count part of the semantics** — "at most once per edit that
   changes the program text" — and hold the evaluator to it. This is the version
   that reads best in a paper and is the most work to be true.
3. **Make the program idempotent and stop caring.** Adapton is already
   idempotent in the right way: forcing an aligned thunk is free, and a `put` of
   a value a cell already holds signals nobody. A program written in archivist
   style may already be safe under arbitrary re-running — in which case the
   composition law is a discipline on the *Fumola* program, and Hazel's
   evaluator needs no change at all.

I think the third is right. I do not think it is established, and the difference
between those two sentences is what N1 and N2 are for.

## Other things unresolved

- **Stepping and undo.** The stepper runs expressions under a different schedule
  again, and undo runs them backwards. Neither has been looked at.
- **Main thread only.** The runtime is a property of `window`, so a Fumola cell
  cannot use the worker, and `evaluate_sync` runs to completion rather than in
  5000-step slices with a client timeout. Measured costs on the shipped slides
  are small — about 0.4 ms median per run, and ~65 ms once per instance for
  `claim` and `ensureMode`. A Fumola program sharing a cell with a heavy Hazel
  computation blocks the UI for the whole of it.
- **A mode change discards the store.** `ensureMode` resets an instance when the
  mode differs from the one it holds. An instrument that silently loses its
  history is worse than no instrument, and this is the most likely way to lose
  one.

## Proposal

Two milestones. They are instrumentation, and they are also the empirical part
of the claim in the next section. The checks matter more than the deliverables —
every finding above began as a confident belief that measurement corrected, and
there is no reason to think that has stopped.

### N1 — The effect schedule, written down

A corpus whose only job is to count. One instance, one cell, one `put` per
visit, and a rendered count. Then a table: for each editor gesture — a keystroke
inside the cell, a keystroke elsewhere, a cell added, a reload, a step, an undo,
a focus change — how many effects, in what order.

*Why first.* It needs no new machinery, and everything downstream of it assumes
an answer that nobody currently has.

*How we would know it worked.* The table is produced by running the instrument
and reading the panel, never by reading `Evaluator.re` — and it is reproducible
by someone else from the document alone. A row that does not reproduce is a
finding about the instrument, which at this stage is worth as much as a finding
about Hazel.

*Deliverable.* `docs/hazel-effect-schedule.md`, explicitly measured rather than
derived, naming the build and the runtime version it was measured against.
`window.fumola.source()` reports the latter, and it matters: a developer with
stale local wasm artifacts and a developer with none are running different
runtimes at the same commit.

### N2 — Attribution by declaration, not by heuristic

The run carries which pass it came from, so an event can say *elaboration*,
*evaluation*, *stepper*, *panel peek*, rather than being sorted by whether its
source happens to be the root.

*Why second.* N1 can count effects but cannot fill in the column that says why
each one happened. Without that, the schedule table has a hole exactly where the
interesting part is.

*How we would know it worked.* On a slide whose schedule N1 established, every
event is attributed and the per-pass counts add up. Then the heuristic and the
declaration are run against each other on the existing slides — and the
interesting result is a disagreement, since the heuristic is currently what the
panel ships.

## What this would be, if it works

**An operational account of Hazel's cell semantics, derived by measuring the
implementation rather than by reading it.**

Live programming environments are specified by their front ends and
under-specified by their schedules. Notebooks are the well-known case, and the
usual response in the literature is to observe that notebooks are bad. Hazel is
a system built specifically not to have that problem, with a real semantics and
a real type system behind it — and the effect schedule of its cells was still,
until this month, an open empirical question that nobody had the instrument to
ask.

Having the instrument is the contribution. The findings are what it produces.

## What this does not propose

- **Changing Hazel's evaluator.** The instrument's job is to describe the
  schedule. If the schedule turns out to be wrong, that is a separate decision
  needing separate evidence, and N1 is what would supply it.
- **A verdict on the composition law.** Three candidates are written down and
  one is preferred. Preferring it is not establishing it.
- **Merging the branch chain.** `fumola-tiles-mvp` → `fumola-tiles-and-livelits`
  → `experimental-lang-integration` remains a demo track. What lands on `dev`,
  and when, is a separate conversation this document is trying to inform rather
  than pre-empt.

## Background

| Where | What |
|---|---|
| [`src/language/fumola/README.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/src/language/fumola/README.md) | running a program, where the runtime lives, and why it decides the rest |
| [`docs/fumola-tiles-design.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/docs/fumola-tiles-design.md) | why the instance is named in the syntax rather than derived from an `Id` |
| [`docs/fumola-runtime-changes.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/docs/fumola-runtime-changes.md) | the unpinned wasm dependency, and why to check `source()` before drawing a conclusion from a browser session |
| [`docs/programmable-probes.md`](programmable-probes.md) | the same instrument, pointed at a user's program |
