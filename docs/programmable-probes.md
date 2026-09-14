# Programmable probes

Fumola instances as probes you can write, and that survive the edit.

Status: a design note and a proposal. The integration it builds on is running on
`experimental-lang-integration`; nothing in the proposal is implemented. Prose
only — no code.

Companion document: [Notebook semantics](notebook-semantics.md), which is the
same instrument pointed at Hazel's own evaluator rather than at a user's
program, and which explains why a Fumola instance can do any of this.

## The claim

Hazel has two programmable surfaces in the editor, and neither covers
observation.

**Livelits** are programmable and are about *input*: a model, an action, a view,
an expansion. A user writes one in Hazel and it becomes a GUI in the source.

**Rich probes** are about observation and are not programmable by a user. A
renderer is an OCaml module, plus a mandatory `.rei` to satisfy
`pack_renderer`'s first-class module constraint, plus a line in
`RichProbeRegistry.renderers`, plus a rebuild of Hazel. That is the right design
for a renderer that ships with the editor and the wrong one for a renderer
somebody wants this afternoon.

And both are transient in the dimension that matters most for observation. A
probe's samples belong to one evaluation; they are re-derived on the next
keystroke and gone. "What values has this expression taken *while I have been
editing it*" is not a question Hazel can currently answer at all.

A Fumola instance closes both gaps at once, because it is durable state that a
Hazel program can reach and Hazel's evaluator does not control. The proposal is
to use it as one:

> A probe whose logic is a program in the editor, and whose samples outlive the
> evaluation that produced them.

## Two halves of a spreadsheet

Hazel and Fumola are close relatives — both ML-family, both with variants and
records and thunks, both having made evaluation order a design question rather
than an accident. That closeness is why the integration is a sort in the tile
grammar and a value translation rather than a foreign function interface.

Both also have a spreadsheet in them. They are *different halves* of one, which
is why putting them together is interesting rather than redundant.

| | Hazel's half | Fumola's half |
|---|---|---|
| The good part | **liveness** — a cell shows a value while the formula is half-written; holes do not stop evaluation; the display is continuous in the edit | **dependency** — a cell knows who read it, a change reaches exactly those readers, and recomputation is proportional to the change |
| Built around | editing, typing, rendering, responding | forcing, signaling, realignment, repair |
| What it lacks | memory: it re-runs the world on every keystroke and forgets | a surface: it knows precisely what changed and has no way to show anyone |

Excel has a weak version of Fumola's half and none of Hazel's — a half-written
formula shows an error, not a value. Jupyter has neither, and names the gap
*Restart and Run All*.

Hazel re-runs and forgets. Fumola remembers and cannot show you. Neither is a
criticism: each language is specialized for its domain, and the specializations
turn out to be complementary rather than overlapping.

## The correspondence

"Programmable probe" is a description rather than a metaphor, because Hazel's
probe system and Fumola's store are already the same shape.

| Hazel probe system | Fumola | What the pairing buys |
|---|---|---|
| a probe placed on an expression | a `put` into a named cell | the sample survives the edit that produced it |
| a probe's sample list | the cell's revision history | a series across edits, not within one evaluation |
| a rich-probe renderer (`parse` / `init` / `update` / `render`) | a `thunk` reading cells | the renderer is a program in the editor, not a module plus a `.rei` plus a registry line plus a rebuild |
| re-rendering a probe | realignment | a derived view over a long history repairs rather than recomputes |
| the call-stack navigation in `ProbeFocus` | the DCG's edges | provenance: *why* this value, not only *what* |
| `AutoProbe.All` — one probe per source row | the DCG itself | a graph already records every force and every dependency, which is a probe on everything, placed by the computation, keeping the edges a per-row probe throws away |

The last row is the one to sit with. Auto-probing every row is an approximation
of what a demanded computation graph already is, minus the structure — and the
structure is the part a reader actually wants when the value is surprising.

## What a durable probe can observe that today's cannot

Four capabilities, in rough order of how cheap they are:

1. **A series across edits.** The last fifty values this expression took, as I
   edited it, each labelled with the program text that produced it. Nearly free:
   the store already survives, and the panel already renders its rows as typed
   Hazel values.
2. **A derived view that repairs rather than recomputes.** A fold over a long
   history is a thunk. A histogram over ten thousand samples does not get
   recomputed because one sample arrived. This is Fumola doing the thing it was
   built for, in service of the editor rather than of a benchmark.
3. **Observation logic a user writes.** The renderer is Fumola source in the
   editor, edited as tiles, with Hazel statics on its escapes. This is the
   livelit argument applied to observation instead of to input.
4. **Provenance as a first-class view.** The Nodes and Edges views exist. Point
   them at a user's own computation and "why is this value this value" becomes a
   question the editor answers with a path rather than a number.

## The design question: who names the series

This is the one decision the proposal turns on, and it has already been made
once, one level up.

A sample series needs a key. The obvious key is the probed expression's `Id.t`,
and it is wrong for a reason that is already written down: a name taken from a
Hazel id starts a new series whenever that id changes — which is to say, on
exactly the edits worth watching. `fumola … end` faced this and answered it by
putting the instance name in the syntax, where the programmer wrote it:

```
fumola $graphical as store in … end
```

Stable by construction, visible in the program, survivable across a reload, and
it makes two blocks sharing one instance expressible.

The answer here should be the same answer one level down: **the user names the
series, in the source.** A renamed series starts over, visibly and on purpose,
rather than silently. The structural echo is a point in its favour — a design
that has to answer the same question twice should prefer answering it the same
way.

## Proposal

Four milestones. Each says what lands and how we would know it worked.

They are numbered P1–P4 to keep them distinct from N1–N2 in
[Notebook semantics](notebook-semantics.md), which are not merely separate work:
N1 and N2 measure how many times a Hazel expression actually runs per edit, and
P1's whole premise is that a sample series is a faithful record of that. If the
effect schedule turns out to be surprising, P1 inherits the surprise. It is
worth doing N1 first for that reason alone.

### P1 — A probe that survives the edit

Samples accumulate in a named instance rather than in an evaluation. The series
is named in the source, per the section above.

*Why first.* It is the smallest thing that is genuinely new to a user, and it
exercises the naming decision while it is still cheap to change.

*How we would know it worked.* Edit the probed expression; the earlier samples
are still there, still labelled with the program text that produced each one.
Rename the series; the history starts over, visibly. Change the instance's mode;
it should *not* silently reset — and today it would, which is the first thing
this milestone will run into.

### P2 — A rich probe written in Fumola

`RichProbe` becomes an interface a Fumola thunk can implement, rather than an
OCaml module plus a `.rei` plus a registry line plus a rebuild. This is the
milestone that earns the word *programmable*.

*How we would know it worked.* A new renderer is added, and renders, without
rebuilding Hazel. `TableRenderer` is the control: the Fumola version should be
recognisably the same program and should handle the same values. Where it cannot
— the existing renderer can rewrite the surrounding syntax via
`parent(SetSyntax(seg))`, and a Fumola thunk has no obvious counterpart — that
limit gets written down rather than worked around.

### P3 — Provenance over a user's own computation

The Nodes and Edges views pointed at a user's instance rather than at a demo,
with navigation from a sample back to the edges that produced it.

*How we would know it worked.* On a computation whose dependency structure was
written down in advance, the path the panel shows is the path the program takes.
Prefer an example where the obvious answer is wrong — a memoized call whose
result came from a revision older than the edit that appears to have caused it.
An example that can only confirm what a reader already assumed is not a check.

### P4 — Scenes as Hazel values

DCG scenes rendered through Hazel's own view layer rather than through a bespoke
panel, starting with the List rung. This is where the two halves finally close:
the graph Fumola maintains incrementally is displayed by the system that is good
at displaying things, as an ordinary value of an ordinary type.

*How we would know it worked.* The scene is a Hazel value with a declared Hazel
type, and the panel is one renderer over it rather than the only way to see it.

## Risks

- **A mode change discards the store.** `ensureMode` resets an instance when the
  mode differs from the one it holds. A probe that silently loses its history is
  worse than no probe, and P1 will meet this immediately.
- **Instance names are global.** Two programs that both say `store` share one
  runtime. That is a feature for the panel and a hazard for anything a user
  writes without knowing the convention.
- **Main thread only, so a heavy probe blocks the UI.** The runtime is a
  property of `window`, so a cell containing a Fumola form cannot use the
  worker. Measured costs today are small; a probe whose derived view is
  expensive is exactly the case that stops being small.
- **Hazel's cache and Fumola's store can disagree**, so a displayed value may
  not correspond to the store behind it. This is the composition law, and it
  belongs to the companion document.

## What this does not propose

- **Retiring Hazel's probes.** They answer a different question — what the
  program computed — and they answer it better, with no store to name and
  nothing to survive. A durable probe is an addition, and most of the time the
  transient one is what a reader wants.
- **A type system for Fumola.** Examples first. The contracts that matter here
  belong in this document and in the round-trip checks against the real parser.
- **Merging the branch chain.** `fumola-tiles-mvp` → `fumola-tiles-and-livelits`
  → `experimental-lang-integration` remains a demo track. What lands on `dev`,
  and when, is a separate conversation.

## Background

| Where | What |
|---|---|
| [`docs/rich-probes.md`](rich-probes.md) | the renderer plug-in layer P2 would open to Fumola, and why the `.rei` is mandatory |
| [`docs/livelits.md`](livelits.md) | the programmable surface this one is modelled on |
| [`docs/notebook-semantics.md`](notebook-semantics.md) | why a Fumola instance can outlive an evaluation, and what that revealed |
| [`src/language/fumola/README.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/src/language/fumola/README.md) | the grammar, the printer contract, the event list, and the round trip |
| [`docs/fumola-tiles-design.md`](https://github.com/hazelgrove/hazel/blob/experimental-lang-integration/docs/fumola-tiles-design.md) | why the instance is named in the syntax — the decision P1 echoes |
