# Notebook semantics

> Hazel is Fumola's Strange Observer (and editor).
> Fumola is Hazel's Strange Outsider (and doer/archivist).
>
> They make a strange loop that's very exciting to me.
>
> — Matthew Hammer

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

**The count per edit is three, and was believed to be one.** This was the
belief this document was written to check, and checking it is
[N1](hazel-effect-schedule.md). An edit inside a `fumola … end` form runs the
program **three times**, within about five milliseconds, on this branch as
committed. Two of the three are passes that want only the *shape* of a rule --
`ReusePass`, which walks a term after the evaluation that already ran it, and
`StreamCollector`, which reassembles an evaluation that happened in the worker --
and pay for that shape by firing the rule. For every pure rule that costs time
and nothing else, which is why it has never mattered. For the one rule whose
step reaches a store outside Hazel, it costs a put.

The draft of this paragraph said the opposite: that Hazel's cache means *fewer*
effects than assumed, because a re-evaluation reusing a cached cell does not
re-run the program. That is true of one gesture -- undo, measured at zero -- and
it was the wrong generalisation. The instrument was built to settle this and it
settled it the other way.

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

## The channel is there; spending it is not free

An earlier draft of this document said the store can tell you *that* the editor
caused an event but not *which pass* caused it, and proposed threading a
declaration through the run. That was wrong, and the correction was to spend a
field the store already carries. That correction was also wrong, in a way only
measurement found. Both halves are worth keeping, because the observation
survives and the inference does not.

A node id is a triple, and both of its first two components are symbols:

```
node_id = (space, time, int)
space   = Here | Symbol(sym)
time    = Now  | Symbol(sym)
```

`is_editor` — the test that separates the editor's traffic from the program's —
destructures that triple as `[space, ..._]` and tests the space alone. **The
observation holds**: measured on both builds of
[N1](hazel-effect-schedule.md), every edge in every instance is sourced at
`Here/Now/0`. 16 of 16, 33 of 33, 2 of 2. The time and the counter on the
editor's root carry no information, across every pass, gesture and reload.

**The inference does not hold.** Hazel cannot spend that field, for two reasons
that were checked against the running runtime rather than reasoned about.

**The root node is a constant.** `root_node()` in the graphical engine answers
`(Space::Here, Time::Now, MetaTime(0))`. It takes no argument and reads no
state, so the source of every edge the editor causes is that triple and nothing
Hazel does can make it otherwise.

**The navigations exist, and they move the wrong end.** Fumola has
`do goto time <sym> { … }`, which sets the current time, and
`do within time <sym> { … }`, which extends it; both scope to their block. Run a
program inside one and the time does land — on the **target** node ids, the
cells the program touches. The source stays `Here/Now/0`:

```
put   Here/Now/0  ->  Symbol(b) / Symbol(p1)     under  do goto time `p1
put   Here/Now/0  ->  Symbol(c) / Symbol(p2)     under  do within time `p2
```

**And the time is part of a node's identity**, which is what makes this fatal
rather than merely inconvenient. The same program run under two times does not
write one cell twice; it writes two cells:

```
`k := 1  under `pA   ->   Symbol(k) / Symbol(pA) / 1
`k := 1  under `pB   ->   Symbol(k) / Symbol(pB) / 3
```

A time per pass would therefore give every pass its own private copy of the
store — destroying exactly the incrementality the store is there for. The
cheapest-looking version of N2 is the one that breaks the thing N2 is
instrumenting.

### What is left, and it is two different milestones

**A — change the runtime, and attribution becomes structural.** `root_node()`
stops being a constant and reads the current time. Then the editor's own root
carries the pass, attribution is read straight off the edge source exactly as
hoped, and no cell's identity moves, because the cells are targets and only the
source changed. The space-only `is_editor` test is what makes this safe, and
that half of the design was right and is already committed (`303fc7c018`). The
cost is a change in the Fumola repo, and
[`fumola-runtime-changes.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/docs/fumola-runtime-changes.md)
is the reason that is not a small decision: Hazel pins no version, so it reaches
every Hazel user without a commit here.

**B — a sentinel, from Hazel alone.** Each run begins by putting the pass's name
into one well-known cell. Attribution is then positional: every event between
marker *n* and marker *n+1* belongs to that pass. Verified in the runtime — the
work cell is not forked, order is preserved, and all nine edges of a three-run
test came back `aligned` with **no signal**, because nothing ever reads the
marker. It costs one put per run and adds the marker's own traffic to the store,
which is the editor's traffic and already dimmable.

B works today and is what N2 below now proposes. A is better and is not this
milestone.

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

I think the third is right. I do not think it is established, and N2 is what
turns the difference between those two sentences into a check rather than a
preference.

Candidate 3 makes a prediction readable directly off the store: **a re-run that
changes nothing should add no `edgeSignaled` event, and leave every alignment
`Aligned`.** An alignment is `Aligned` or `Signaled`, and `edgeSignaled` is
already one of the event kinds `FumolaEvents` names and the panel lists — so if
archivist-style operations really are idempotent under arbitrary re-running,
then Hazel re-running a pass over an unchanged program leaves everything
aligned and adds no signal. Either showing up after such a re-run falsifies the
candidate, and the event is the cheaper of the two to watch.

[N1](hazel-effect-schedule.md) gave it a first real test: an instance that ran
an identical program exactly twice, with no edit between, produced four edges,
all `aligned`, no signal. It passed -- with a correction it did not anticipate.
**The store still grew.** A re-run that signals nobody and realigns nothing
appends two edges and advances metaTime by two, so adapton is idempotent with
respect to *alignment* and not with respect to *history*. A discipline on the
Fumola program can make re-running harmless to what the program computes, and
cannot make it invisible to anything that reads the history -- which is the
panel, and would be any probe built on P1's series.

One test on one instance is not a result, and the older evidence should still be
labelled weak: across six instances the only align seen was `aligned`, which is
six demo slides, not a sample.

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

**Done, 2026-09-14: [Hazel's effect schedule](hazel-effect-schedule.md).** It
came out as an A/B between the branch as committed and the uncommitted
`~effects` work, which is a sharper instrument than the one proposed here: three
runs per edit becomes one, and the two that disappear are identified by what
removing them removes. It also confirmed N2's premise directly -- every edge in
every instance on both builds is sourced at `Here/Now/0` -- and gave the
idempotence prediction below its first test, which it passed, with a correction.
What it did not get is the stepper.

### N2 — A sentinel naming the pass

Each run puts the name of the pass that caused it into one well-known cell
before running the program. The panel groups events by the marker they fall
after, and the reader gets *elaboration*, *evaluation*, *stepper*, *panel peek*
against the counts N1 measured.

The pass is known where it needs to be known: `Transition.transition` already
takes `~effects`, and every call site already declares whether its steps are the
program happening. Widening that from a two-state flag to one that names the
pass is a change to an argument that exists, not new plumbing.

*Why second.* N1 can count effects but cannot fill in the column saying why each
one happened.

*How we would know it worked.* On a program whose schedule N1 established, every
event falls after a marker naming a pass, and the per-pass counts add up to N1's
totals. The marker must not perturb: the work cell keeps one identity, and a
re-run that changes nothing still adds no signal.

*What it does not get.* Ordering across passes is positional rather than carried
in the ids, so nothing in an edge says which pass it belongs to when read on its
own — only where it sits. That is what A would fix, and it is why A is worth
doing even after this lands.

**Done, 2026-09-14: [#2566](https://github.com/hazelgrove/hazel/pull/2566).** It
answered a question N1 left open on its first run. N1 measured *turning the
stepper on → 1 run* and could not say whose:

```
put  _hazelPass = `eval
put  w          = 5
get  w          = 5
put  _hazelPass = `decompose      <- turning the stepper on
put  w          = 5
get  w          = 5
```

It is `decompose` — the pass that performs a step only to find out where the
step is, which is the one `EvaluatorStep` already flags as needing its own fix.
Six edges, all aligned, no signal: the work cell keeps one identity and nothing
reads the marker.

The panel groups by it too: Events, Nodes and Edges each show a header
wherever the pass changes. A header is emitted only above a row that is
actually shown, so hiding the editor cannot leave a heading with nothing
under it, and the pass of a hidden row is still read, so a boundary falling
inside a hidden run reaches the next visible row. Rows older than the first
marker get no header — every row an instance recorded before this build, and
every row a program put there itself.

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
| [`docs/hazel-effect-schedule.md`](hazel-effect-schedule.md) | N1's result: the measured schedule, and the A/B that produced it |
| [`docs/programmable-probes.md`](programmable-probes.md) | the same instrument, pointed at a user's program |
