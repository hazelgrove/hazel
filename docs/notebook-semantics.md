# Notebook semantics

> Hazel is Fumola's Strange Observer (and editor).
> Fumola is Hazel's Strange Outsider (and doer/archivist).
>
> They make a strange loop that's very exciting to me.
>
> — Matthew Hammer

Two mottos, side by side:

> **Incomplete programs should still be meaningful programs.** — Hazel
>
> **Incomplete programs should still be _responsive_ programs.** — the outside
> archivist

They ask for different things, and the second is only sayable once the first is
answered.

*Meaningful* is about what a program says while parts of it are missing. It is a
claim about **semantics**: a hole has a type, an expression containing one still
evaluates, and the value you are shown is the value the program has right now
rather than an error standing in for one. Hazel answered that, and the answer is
what makes an editor a live one rather than a text box that occasionally
compiles.

*Responsive* is about what it costs to ask again. It is a claim about
**effort**: the edit you just made should cost work proportional to the edit and
not to the program. Nothing in the first motto gets you the second — a semantics
can be perfectly meaningful about holes and still recompute the world on every
keystroke, which is what [the effect schedule](hazel-effect-schedule.md)
measures Hazel doing.

The two also fail differently, which is the useful part. A system that is
meaningful but not responsive is correct and slow, and its incompleteness shows
up as waiting. A system that is responsive but not meaningful is fast and lying:
it answers quickly because it is answering an older question. Getting both is
what the archivist is for, and it is why the store has to be somewhere the
editor does not control.

What actually runs when you edit, and how we found out.

Status: a design note and a proposal. The instrument it describes is built and
running on `experimental-lang-integration`, and the observations it reports were
made there. What is proposed here is how to turn them into a result. Prose only
— no code.

Companion document: [Archivist reflection](archivist-reflection.md), which is
the same mirror pointed at a user's program rather than at the editor.

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

## Fumola's times, measured

This section began life arguing that a time per pass could not work. It was
wrong twice — first about the mechanism, then about the consequence — and what
replaced it is the most useful thing the instrument has produced, because it is
not about Hazel at all. It is Fumola's model of revisions, which is the other
point of view Hazel is being looked at from.

A node id is `(space, time, int)`. **A space is a cell; a time is a moment.**
Recording which pass ran a program by putting its name into a cell, as an
earlier draft of this did, gives the pass a *space* — and a pass is not a thing,
it is an occasion.

### The lookup rule

**A read at time T answers with the write at the greatest *comparable* T′ ≤ T.**

Everything follows from that sentence, and every line of it was checked against
a running runtime:

| after | at 0 | at 1 | at 2 | at 3 | at `Now` | `` `step(5) `` |
|---|---|---|---|---|---|---|
| `` `q := 7 `` at 1 | — | 7 | 7 | 7 | — | |
| `` `q := 70 `` at 2 | — | **7** | 70 | 70 | — | |
| `` `q := 700 `` at 1 | — | 700 | **70** | 70 | — | — |

A later moment sees what earlier ones did. A later write leaves the earlier
moments untouched — the history is immutable, not a mutable store being
overwritten. And a time in another family sees none of it.

`Now` is not a special base; it is simply the **bottom** of the order. That is
why every named moment can read what was written at `Now`, and why `Now` can
read nothing written at a named moment.

### Ordered and unordered, both on purpose

Fumola's `PartialOrd` is where the design lives:

```
`hazel(1) < `hazel(2)        Symbol::Nat arguments compare numerically
`hazel(1)  ?  `step(2)       different heads: incomparable, in both directions
Now < every named time       Now is the bottom
```

Name symbols are incomparable *deliberately*. The comment in
`adapton/mod.rs` says why: non-equal quoted symbols are left incomparable "to
express certain kinds of **independence/parallelism** in the time ordering."
So Fumola offers a mixture — an ordered sequence within a family, and
independence between families — and a system layered on top gets to say which
of its own moments are a history and which are parallel observations.

One spelling decides which you get, and it is easy to get wrong: the index must
be **bare**. `` `hazel(1) `` is a `Symbol::Nat` and is ordered; `` `hazel(`1) ``
is a `QuotedAst` and is incomparable, which would silently give every run its
own island with nothing visible between them. Two more: the navigation takes a
nullary expression, so the time needs its own parentheses; and the braces after
a navigation are already a block position, so the program inside needs no `do`
of its own.

### What that makes Hazel

Hazel's passes are now moments in one ordered family: every run happens at
`` `hazel(n) ``, with `n` counting runs. Pass *n* sees everything every earlier
pass did; its writes leave the earlier moments alone; and `Now` is left to the
meta level, unused.

The attraction is that this is not a labelling scheme bolted on. **Hazel's
sequence of edits is a revision sequence, and Fumola already has revisions.**
The store stops being one mutable thing the editor keeps overwriting and becomes
a history of what the editor did, addressable at any point in it. Attribution
comes free and structural — a node's time *is* the moment that made it — with
nothing written anywhere to record it.

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
panel, and would be any probe built on AR1's series.

One test on one instance is not a result, and the older evidence should still be
labelled weak: across six instances the only align seen was `aligned`, which is
six demo slides, not a sample.

## A documented limit is a cache too

A memo, from getting this wrong on 2026-09-14, while writing per-form
documentation for the cursor inspector.

The entry for `hazel … end` — the escape back into Hazel from inside a Fumola
program — said that it carries a value written in place and **not** a variable
bound in the surrounding scope, because the Fumola program is rendered to
source during *elaboration*, before anything has been substituted. Every clause
of that was true when it was written. None of it is true now: the program runs
during **evaluation**, so by the time it is printed its escapes have been
reduced, and a bound variable is a value like any other.

Nobody wrote the stale sentence twice. It was carried forward from the code it
replaced, and shipped without being checked, which is the ordinary way this
happens and the reason it is worth a memo rather than an apology.

It is worth a memo *here*, in this document, because it is this document's own
subject. The question the notebook asks is whether a displayed value still
corresponds to the program it claims to be about. Prose is a display, and a
documented limit is a cached answer to "what can this system not do" — cached
with no dependency edges at all. Nothing recomputes it. Nothing signals it. No
test fails when the thing it describes moves out from under it. It is the
purest case of the failure the composition law is about, and it happens at the
layer where neither system is watching.

The sharper half is that the sentence did not merely go out of date. When the
limit was lifted, it **reversed**: the escape now carries a bound variable
anywhere, while the livelit route — which still expands during elaboration —
is the one whose slot takes a literal only. A reader following the stale advice
would have been sent to the integration that had just become the worse choice
for exactly this. Stale documentation of a limit does not decay into silence;
it decays into confident misdirection, because a limit is the kind of claim
readers act on rather than verify.

So, for both of us, going forward:

> A limit is a claim in the present tense. Write it with the thing that would
> show it had been lifted.

In this repository that thing is usually a slide, and a slide is the good case:
`Fumola (Tiles) / Self-inspection` reads a Hazel boolean through an escape and
the graph Fumola records changes shape when the boolean flips. That is a
limitation's test. It runs on every build, it is visible to a reader who is not
looking for it, and when the limit moves the slide stops meaning what it said.
A sentence in a comment has none of those properties.

And note what would have caught it, since it is what this whole document is
proposing. A record with dependency edges — one that knows the prose was
derived from the elaboration order, and signals the prose when that order
changes — is not a thing prose has and is exactly what the archivist is for.
The gap this memo describes is the gap [Archivist reflection](archivist-reflection.md)
argues for closing, one layer up from where it argues for closing it.

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

### N2 — Every run at its own moment

Each run happens at `` `hazel(n) ``, `n` counting runs, and the panel groups by
the moment it reads off a node id. The pass a moment belonged to is Hazel's own
business and stays in Hazel; nothing is written to the store to record it.

The pass is known where it needs to be known: `Transition.transition` already
takes `~effects`, and every call site already declares whether its steps are the
program happening. Widening that from a two-state flag to one that names the
pass is a change to an argument that exists, not new plumbing.

*How we would know it worked.* On a program whose schedule N1 established, every
node's time names a moment and the per-moment counts add up to N1's totals. The
store gains no cell that exists only to be read by the panel. And reads have to
move with the writes — a reader left at `Now` sees nothing at all, since `Now`
is below every moment.

**Done, 2026-09-14: [#2566](https://github.com/hazelgrove/hazel/pull/2566).**

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
| [`docs/archivist-reflection.md`](archivist-reflection.md) | the same mirror, pointed at a user's program |
