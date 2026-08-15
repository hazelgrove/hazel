# Conjoined Tutorial — Design

Distilled from `conjoined-tutorial-plan.md` (v3). The shipped tutorial —
29 slides, `00`–`39`, including the embedded study tasks — lives in
`hazel-programs/tutorial/*.hzt` and is the ground truth; this records the
design rationale behind it. (The slide sequence below is the design-time
numbering, which the shipped set renumbered and extended with the task
slides 26–34.)

## Organizing principle

One woven progression: introduce Hazel language features incrementally and use
**probes as the expository vehicle** for each. Probes stay on (manual or auto)
wherever they illuminate the semantics being taught. The tutorial's only job is
to prepare participants for the study tasks (writing + debugging) so those
tasks feel natural.

### The spine: language complexity drives probe-value multiplicity

| Hazel feature | values / expr | probe capability it motivates |
|---|---|---|
| literals, arithmetic, `let` | exactly **1** | place a probe; read the value; environment on click |
| `if` / `case` (branching) | **0 or 1** | the `∅` "never evaluated" indicator |
| functions called > once | **0 / 1 / many** | navigate samples (←/→), single vs many, one-per-call |
| recursion / folds | **many** | **pin** a call; **step into** one call; dynamic focus |
| writing / exploring | live values as you type | **auto-probe** |
| advanced / optional | relating samples across the stack | sample **colors** |

## Cuts (confirmed against the task feature audit)

Partial application and pipelining `|>` (zero tasks use either); advanced
labeled-tuple operations (extension/omission/list-conversions); standalone
float arithmetic. Recursion needs no dedicated lesson — `fold_left` covers all
task iteration, so step-into is taught on fold iterations, not recursion depth.

## Additions closing audit gaps

A tuples slide (grouping + destructuring + `_`); the MoonPhase ADT folded into
the case slide (closing the no-ADT-lesson gap); strings as secondary content
(literals + `++` seeded early, one `string_match` regex in a writing warmup)
rather than standalone string lessons — the string-function writing tasks are
precisely about discovering function behavior via live probe values.

## Continuity: one growing garden program

A single garden codebase threads from the first arithmetic (`250 * 7` weekly
water) through `MoonPhase`, `watering_amount`, `Plant = (name, icon, water)`
records, beds, and map/fold over beds, into the warmups and the garden-themed
tasks. Tone: a night garden on lunar time, a brass moon-dial, a caretaker's
ledger — strangeness kept subtle and dry. Not rigid: a disjoint example is used
where it covers technical content better.

## Progressive probe reference

Each slide's Task Reference sidebar hosts a probe quick-reference that grows:
a control/row appears the moment its feature is introduced and stays on every
later slide (strictly additive). Implemented per-slide, keyed by slide module
(`TutorialProbeStrip`), with per-slide initial probe settings
(`TutorialSlideInit`): auto-probe off until introduced, then All; samples
window deliberately reset per slide (Many only where several samples at once
is the lesson); the Hybrid color scheme + legend only on the colors slide.

## Gating

Some, not rigid: the Next button is never disabled; hidden tests flip a
cosmetic checkmark (🤔 → 🤩). Small language exercises and writing warmups are
gated on hidden tests; "notice this" probe beats lean exploration.

## Sequence (design-time; acts)

- **Act 0 — editor & one value:** arithmetic & holes; parser & backpack
  (intro `let`); first probe; variables & exploring.
- **Act 1 — branching & data:** tuples; records (`.field`); if (clamp +
  multi-probe); sum types, case & `∅`; variants with data (payload + Option).
- **Act 2 — functions (0/1/many):** many samples (←/→, Space); aligning
  samples (dynamic focus, ⊖).
- **Act 3 — auto-probe** (follows cursor; line-break granularity).
- **Act 4 — lists & iteration:** lists; map (+ `fun` lambdas, filter/mapi);
  fold; pin; step-into.
- **Act 5 — writing & debugging:** writing warmups (running-sum fold,
  strings/regex); print console; debugging warmup (mini-MVU with planted bug).
- **Act 6 — show-and-tell:** sample colors; free-exploration arena.

The shipped set follows this arc with the study tasks (26–34) as Act 5's
payload; the sample-colors slide (39) survives as the one optional extra.

## Naming

User-facing text says **dynamic focus** (bar title "probe focus", legend
"Sample Focus Legend"); code identifiers keep `SampleFocus*`.
