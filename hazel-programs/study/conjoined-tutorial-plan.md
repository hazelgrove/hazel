# Conjoined Tutorial — Design Notes (v2)

## Organizing principle
**Not** "Hazel basics, then a probes section." Instead: a single woven
progression that introduces Hazel language features incrementally and uses
**probes as the expository vehicle** for each one. Probes stay *on* throughout
(manual or auto) wherever they illuminate the semantics being taught.

The tutorial's job is singular: **prepare participants for the study tasks**
(program writing + debugging) so those tasks feel natural. That means by the
end they've met the Hazel features the tasks use and the probe features useful
for them — built up organically rather than as a feature checklist.

(The paper's spatial/temporal/granularity framing is a loose retrospective
lens, not crisp dimensions — this plan does **not** organize around it, and the
autoprobe=spatial / pin=temporal mapping is deliberately dropped.)

## The spine: language complexity drives probe-value multiplicity
The cleanest through-line — and one already latent in the existing probe
slides — is that each construct changes *how many values* an expression takes
at runtime, and each step motivates a new probe capability:

| Hazel feature | values per expr | probe capability it motivates |
|---|---|---|
| literals, arithmetic, `let` (straight-line) | exactly **1** | place a probe; read the value; environment on click |
| `if` / `case` (branching) | **0 or 1** (un-taken branch → none) | the `∅` "never evaluated" indicator |
| function literals, called > once | **0 / 1 / many** | navigate samples (←/→, single vs many mode); "one per call" |
| recursion / folds (many calls) | **many** — hard to find the one you want | **pin** a call to filter to its downstream; **step-into** one call |
| — (advanced, optional) | relating samples across the call stack | dynamic cursor, alignment (`⊖`), sample **colors** |
| writing / exploring | live values as you type | **autoprobe** |

This is already how `probes/01-fundamentals` (1 → `∅` via `case`) and
`probes/02-functions-and-cursor` (→ many + navigation, then dynamic cursor)
actually unfold. So most content exists; the work is **sequencing it against
the language lessons and trimming** — not writing from scratch.

It's a framing, not the only one. But it gives a reason for the *order* and
makes probes feel like a lens on the language rather than a bolted-on feature.

## Consequence: reorder so multiplicity builds monotonically
Alexander's order currently puts **Functions (08) before If (12) / Case (13)**,
so the probe-value count jumps to "many" before "0-or-1" is even established.
To get the clean **1 → 0/1 → many** build, conditionals should come *before*
functions. Proposed core stretch:

```
Holes → Parser/Backpack → arithmetic → Let
  → [basic probe: 1 value]
  → If → Case            (→ the ∅ "not evaluated" indicator)
  → Type Annotations
  → Functions            (→ many values + sample navigation)
  → Multi-arg / Partial
  → Lists → Map → Fold   (→ pin / step-into when there are many samples)
  → Pipelining
  → [autoprobe]
  → writing & debugging warmups
  → exploration arenas
```

## Merger vs. replacement of Alexander's lessons
**Keep his language lessons as the backbone; weave probes into them; replace
only the overlapping probe-intro.**

- His 22 are solid *language-feature* lessons. Keep them as the spine — but
  **add probes** (autoprobe on, or "place a probe on X to see…") so each
  feature is taught *through* its runtime values. **This probe-ification is the
  main new authoring work.**
- **Redundant probe intros — pick one.** `basics/06-probes` (add-probe +
  probe-in-function via `add_tax`/`map`) overlaps your `probes/01-fundamentals`
  (add-probe + environment + case-`∅`) and `probes/02-functions-and-cursor`
  (functions → many). Your 01/02 are better *staged* (they respect the build);
  his 06 jumps to "many" immediately. → **Retire `basics/06-probes`** as a
  standalone (optionally reuse its `add_tax`/`map` as a later writing warmup);
  use 01/02 as the probe introduction, placed at the right points (01 right
  after `let`; the "many" half of 02 right after Functions).
- **Cut for the study:** the 5 labeled-tuple lessons (`basics/18–22`) and table
  content — almost certainly not needed (confirm against task requirements).
- **Consolidate your variants** to one each: `04 - Pin`/`Pinning`/`Variant Map
  Only`/`Variant Two Exercises`; `05 - Step Into`/`Variant Fold`/`Variant Map
  Fold`; `10 - Colors And Alignment`/`Sample Colors`.

## Probe features: core vs. show-and-tell
- **Core** (tasks depend on these): add/remove probe, reading samples +
  environment, the `∅` indicator, navigating many samples (single/many,
  ←/→), **pinning**, **step-into**, **autoprobe**, and **print** (the study's
  debugging baseline).
- **Show-and-tell** (introduce only as far as it lands): dynamic cursor + `⊖`
  alignment, sample **colors**. `probes/02` currently front-loads the dynamic
  cursor very early and densely — consider **splitting it**: keep
  functions→many+navigation in the main line, defer dynamic-cursor / `⊖` /
  colors to a later optional "here's more you can do" slide.

## Building toward the tasks (and the qualitative half)
- The first ~hour is mostly **habituation and qualitative feel** for probes —
  so favor "turn probes on and *notice* …" over rigid gated drills (gating
  optional per slide; not every slide needs a passing-test gate).
- End the tutorial with a small **writing** warmup (you have `clamp`,
  `running-sum`), a small **debugging** warmup, then one or two **exploration
  arenas** — bigger programs (you have these in `probes/03-auto-probe`,
  `07-writing`) where participants poke around freely. These double as
  qualitative-feedback surfaces and on-ramps to the real tasks.
- Sequence so the *last tutorial slide and the first task feel continuous* —
  the tasks are garden-themed, so theming the warmups similarly helps.

## Open questions / to decide
1. **Task feature audit** (do this first): which Hazel features do the actual
   `study/debugging/*` and `study/writing/*` tasks require? That set defines
   what's *essential* vs. nice-to-have in the basics, and what can be cut.
2. Per lesson: **probe-ify Alexander's version** vs. author a fresh
   probe-centric one.
3. Where to introduce **autoprobe** — a labeled mode after manual probes are
   comfortable, or earlier as the default always-on lens.
4. **Gating**: which (if any) slides gate on a passing test vs. pure
   exploration — leans mostly toward exploration given the qualitative goal.

---
*Supersedes the v1 draft, which framed this as "Phase 1 Hazel → Phase 2 probes"
organized by the paper's dimensions. This v2 reflects the woven,
multiplicity-driven framing: language features as the spine, probes as the lens,
everything aimed at making the study tasks feel natural.*
