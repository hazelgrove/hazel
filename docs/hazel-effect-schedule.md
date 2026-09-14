# Hazel's effect schedule

How many times a Fumola program runs, per editor gesture. Measured, not derived.

This is N1 of [Notebook semantics](notebook-semantics.md). Everything below was
produced by running an instrument and reading what came back. Nothing in the
tables was obtained by reading `Evaluator.re`.

## The headline

On `experimental-lang-integration` as committed, **an edit inside a `fumola … end`
form runs the program three times.** Not once. Three, within about five
milliseconds of each other, for every keystroke.

[`65c0e889c3`](https://github.com/hazelgrove/hazel/commit/65c0e889c3) -- "A pass
that only asks what a step would do must not take it" -- takes it to one. That
commit was written from reading the evaluator; this document is the controlled
experiment that says what it is worth.

| Gesture | Clean `22d619cf4b` | With `65c0e889c3` | Trials |
|---|---|---|---|
| Keystroke editing inside the fumola body | **3** | **1** | 3 and 3 |
| Backspace inside the fumola body | **3** | **1** | 1 and 1 |
| Paste replacing the program, new instance | **3** | **1** | 1 and 4 |
| Keystroke editing **outside** the form, same cell | 0 | 0 | 2 and 2 |
| Caret move — `ArrowLeft`, `Home`, `End` | 0 | 0 | 1 and 3 |
| Undo, restoring a program that had already run | 0 | 0 | 1 and 1 |
| Keystroke making the form ungrammatical | — | 0 | 1 |
| Page load, program restored from local storage | — | 1 | 1 |
| Turning the stepper on | — | 1 | 1 |
| Opening the Fumola panel | — | 0 (one peek) | 1 |
| Taking a stepper step | — | *not measured* | — |

A "run" is the triple `claim` → `ensureMode` → `evalTop`. All three appeared
together every time on both builds measured here — though a later commit has
since changed that, which the caveats record. A dash means not measured on that
build, not zero.

**What the two extra runs are.** `65c0e889c3` withholds effects in exactly two
places — `ReusePass`, which walks a term after the evaluation that
already ran it, and `StreamCollector`, which reassembles the state of an
evaluation that happened in the worker. Withholding those two, and nothing else,
takes the count from three to one. So the two extra runs are those two passes,
identified by what removing them removes rather than by reading them.

That is the shape of the whole finding. Each of those passes wants only the
*shape* of a rule — is this a value, where is the redex, what would be reused —
and pays for it by firing the rule. For every pure rule that costs time and
nothing else, which is why it has never mattered. For the one rule whose step
reaches a store outside Hazel, it costs a put.

## What was measured, and against what

| | |
|---|---|
| Date | 2026-09-14 |
| Baseline | `experimental-lang-integration` at `22d619cf4b`, clean, built 12:07 |
| Fixed | the `~/hazel-experimental` working tree at 11:46, committed at 11:58 as `65c0e889c3` + `303fc7c018` |
| Profile | `dune build src --profile dev`, both |
| Fumola runtime | `fumola.org @ 65f7b1f1a088676d` on both, reported by `window.fumola.source()` |
| Machine | this laptop, Linux, Chromium via the in-app browser pane |

**Check `window.fumola.source()` before comparing any number here with your
own.** Hazel pins no version of the runtime, so a developer with stale local
wasm under `src/web/www/fumola/` and a developer with none are running different
runtimes at the same commit. There were no local artifacts on either side here,
so both fetched the same build from `fumola.org` — which is also what makes the
two columns comparable.

## The instruments

Two, independent of each other, which is the only reason to trust either.

**At the boundary.** Every entry point on `window.fumola` is wrapped with a
counter recording the call, its arguments and its duration. This counts what
Hazel *hands to the runtime*, which is the definition of a run. It installs from
the browser console and needs no build:

```js
(function () {
  const names = ["claim","ensureMode","evalSync","evalTop","evalFresh","reset"];
  const log = [], orig = {};
  names.forEach(n => {
    if (typeof window.fumola[n] !== "function") return;
    orig[n] = window.fumola[n].bind(window.fumola);
    window.fumola[n] = function (...a) {
      const t0 = performance.now();
      try { return orig[n](...a); }
      finally { log.push({ fn: n, args: a.map(String), ms: performance.now() - t0 }); }
    };
  });
  window.__probe = { log, reset() { log.length = 0; } };
})()
```

**In the store.** `prim "adaptonPeekHistory" ()`, the call the Fumola panel
makes, read through `window.fumola.evalTop(id, …)`. Counting `put` edges gives a
run count that does not depend on the wrapper being right.

The two agree everywhere both were applied. On the baseline instance `base1`,
twelve runs at the boundary and **twelve `put` edges** in the store. Across three
instances on the fixed build, 6 and 6, 2 and 2, 1 and 1.

### The peek is free

Worth establishing before using it to measure anything: reading the history does
not change the history. Three consecutive peeks, no editing in between:

```
edges   16  16  16      gets  10  10  10
events  26  26  26      puts   6   6   6
```

Zero delta. `adaptonPeekHistory` is genuinely read-only, so the instrument does
not perturb what it measures, and having the panel open costs the store nothing.

## What the store says

Every edge, in every instance, on both builds, is sourced at `Here/Now/0`. Not
mostly — all of them: 16 of 16, 33 of 33, 2 of 2. The space is `Here`, the time
is `Now`, the counter is `0`, on every edge the editor caused, across every pass,
gesture and reload.

This is N2's premise confirmed by measurement rather than by reading the type:
**the time and the counter on the editor's root carry no information at all.**

`metaTime` advances per *operation*, not per pass — pairs run `(1,1) (2,2) …
(12,12)`, one per edge. So a pass boundary is currently invisible in the store:
nothing says which of the three runs an edge belongs to. On the baseline that is
not an abstract complaint. Three runs per keystroke land in the history as nine
edges with nothing to distinguish them, and the panel shows all nine.

## Three things worth saying out loud

**An edit outside the form does not re-run the program — on either build.** The
cell was `let y = fumola … end in y + 0`; editing the `0` to `078` moved the
displayed result to `83`, correct, so the cell genuinely re-evaluated, while the
Fumola program did not run at all. That is incremental reuse working at the
granularity that matters, and `65c0e889c3` does not change it.

It also sits awkwardly beside
[#2564](https://github.com/hazelgrove/hazel/issues/2564), which reports that the
incremental evaluation cache is overwritten with empty between passes so
"nothing is ever reused." Both observations are real and I do not know how they
fit together. It is the first thing I would look at next, because one of the two
is scoped more narrowly than its wording suggests.

**Undo costs nothing, and that is not obviously good.** Undoing back to a program
that had already run produced zero runs on both builds, and the cell displayed
the right answer. The value came from Hazel; the store was not consulted and did
not move. That is the display and the store diverging on the most ordinary
gesture there is.

**Naming an instance is the expensive part, and it is not the run.** On a fresh
instance `claim` took 139.7 ms and `ensureMode` 132.1 ms (237.6 and 172.9 on the
baseline's first), while `evalTop` took 0.4–4.5 ms. On an instance that already
exists all three are under 2 ms. The cost of Fumola in Hazel is almost entirely
the first mention of a name — which also means the baseline's two redundant runs
were nearly free in time, and expensive only in the store.

## The idempotence prediction survives its first test

[Notebook semantics](notebook-semantics.md) states the archivist candidate as a
falsifiable prediction: *a re-run that changes nothing should add no
`edgeSignaled` event, and leave every alignment `aligned`.*

Instance `fresh4` ran an identical program exactly twice — once from a paste,
once from turning the stepper on — with no edit between:

```
put  5   aligned   metaTimes (1,1)
get  5   aligned   metaTimes (2,2)
put  5   aligned   metaTimes (3,3)
get  5   aligned   metaTimes (4,4)
```

All aligned, no signal. The prediction holds here.

It needs a correction the prediction did not anticipate: **the store still grew.**
A re-run that signals nobody and realigns nothing still appends two edges and
advances metaTime by two. Adapton is idempotent with respect to *alignment*; it
is not idempotent with respect to *history*. A discipline on the Fumola program
can make re-running harmless to what the program computes, and cannot make it
invisible to anything that reads the history — which includes the panel, and
would include any probe built on P1's sample series.

That is exactly what the baseline's three-runs-per-keystroke looks like from
inside the store: harmless to every value, and a history three times the size of
the one the programmer wrote.

One test on one instance is not a result. It is the right prediction, it has
survived once, and it has not yet been given a real chance to fail.

## What did not reproduce

The very first measurement of the session recorded **two** complete runs for a
single paste on the fixed build. That is the shape #2564 predicts, where statics
arrives in two rounds and the larger one re-elaborates.

It did not happen again. Four subsequent pastes, three into a cell emptied and
allowed to settle first, produced exactly one run each. The one condition I could
not reproduce is the one that held the first time: it was the first Fumola
program to exist after the page finished booting.

Recorded as an unreproduced observation, not a finding. If the two-round
behaviour is real it is rare or boot-conditioned, and a row that fires once in
five is a row this method cannot yet measure.

## What was not measured

**Taking a stepper step.** Turning the stepper on works and costs one run. I
could not get the stepper to advance — neither clicking the highlighted redex nor
the step-forward control moved it off step zero, with `Step Backwards` disabled
throughout. Whether that is the stepper stalling on the Fumola quote or my
driving it wrong, I could not tell from outside.

This matters, because the uncommitted work carries a claim about exactly this
case: that `Decompose` performs the step to find out where it is and `TakeStep`
then performs it again, so stepping a Fumola cell puts twice. **This document did
not reproduce that**, and it should not be cited to this document. It is the
highest-value row still missing.

**Page load, the panel, the stepper and the ungrammatical form on the baseline
build** — measured only on the fixed one. **Redo, and undo beyond one step.**
**The worker path**, which a cell containing a Fumola term never takes.

## Caveats

**Gestures were driven by dispatched `KeyboardEvent`s**, not by a human typing.
They go to `.code-container`, the element carrying the editor's `Key.listener`,
so they enter the same path a real keystroke does — `Key.listener` →
`Keyboard.handle_key_event` → `Update.Perform`. Mouse clicks (caret placement,
the stepper toggle, the panel tab) were real browser input. Two details of that
path are worth recording for anyone repeating this: a synthetic event with
`code: "KeyA"` set is silently ignored while the same event with `code` omitted
is honoured; and `#clipboard-shim`, which holds focus, is a child of `#page` and
not of the editor, so events dispatched at the focused element never reach the
editor's listener at all.

**Waits are six seconds.** Nothing arrived later than about 2.7 s after a
gesture, but a count of zero is only as good as the wait behind it.

**The branch moved during the measurement, and one later commit matters.**
`ce5c0e5870` -- "A declared mode means something when it changes, not on every
run" -- landed at 12:06, after the fixed build was taken, and makes `ensure_mode`
fire only when the declared mode is new or has changed rather than on every run.
So "a run is the triple `claim` → `ensureMode` → `evalTop`" describes the builds
measured here and is already out of date on the branch: after `ce5c0e5870` the
~132 ms `ensureMode` should be paid once per declaration rather than once per
run. The run counts are unaffected, since that commit does not touch `evalTop`.
`cc52eabb09` landed later still and touches only the panel.

**Both builds ran the same `bundled.js`.** A fresh worktree building only
`dune build src` does not produce it — it is an npm artifact — and without it the
Fumola bridge never loads and `window.fumola` stays undefined. The baseline's
copy was taken from the experimental worktree, which is sound here because the
`~effects` work touches only `.re` files, but it is the first thing to check if
these numbers do not reproduce.

## Reproducing this

```bash
cd ~/hazel-experimental && dune build src --profile dev
```

Serve `_build/default/src/web/www`, open it, switch to Scratch, and paste

```
fumola $graphical as probe in { let c = `n` := 1; @ c } end
```

Install the boundary counter above, then per gesture: `window.__probe.reset()`,
perform it, wait six seconds, read `window.__probe.log`. For the store-side
count, `window.fumola.evalTop(id, 'prim "adaptonPeekHistory" ()')` and count
edges whose action is `put`.

For the baseline column, do the same at `22d619cf4b` with the worktree clean,
remembering to copy `bundled.js` across.

A row that does not reproduce is worth more than a row that does; at this stage a
disagreement is a finding about the instrument, and the instrument is the part
that is least established.
