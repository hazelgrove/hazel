# Conjoined Tutorial — Plan (v3)

Supersedes v2. v2 framed the principle (language features as spine, probes as
lens, multiplicity-driven build). v3 keeps that and adds the concrete decisions,
the actual slide sequence, the build mechanics, and a done/todo tracker. The
per-slide walkthrough lives in `conjoined-tutorial-overview.md`.

## Organizing principle (unchanged from v2)
One woven progression. Introduce Hazel language features incrementally and use
**probes as the expository vehicle** for each. Probes stay on (manual or auto)
wherever they illuminate the semantics being taught. The tutorial's only job is
to **prepare participants for the study tasks** (writing + debugging) so those
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

## The one-track finding (build mechanics)
There are **not** two slide systems to merge. Alexander's basics and the probe
slides already compile from the same Tutorial track
(`hazel-programs/tutorial/**.hz` → `./hazel gen-tutorial` → one flat `lessons`
list). **Slide order is the alphabetical sort of filenames.** So reordering and
interleaving is renaming files plus regenerating. There is no hard gate either:
the Next button is always enabled; hidden tests only flip a cosmetic checkmark.

**Chosen layout.** Build the conjoined sequence as a flat numbered set under
`hazel-programs/tutorial/conjoined/NN-name.hz`, and point `gen-tutorial`'s
`input_dir` at that subdir. The original `basics/`, `probes/`, `archive/` dirs
stay in place as **reference sources** (no longer generated). Reversible: revert
one line in `src/CLI/GenTutorial.re`. Titles come out flat ("01 - Holes").

**Auto-probe is a single global boolean** (🔬 / Cmd+P), not per-slide. The draft
toggles it with in-prose instructions. A per-slide `autoprobe` flag is Phase-4
infra if we decide we need it.

## Variant decisions (one each)
- **Pin → `04-pinning`** (most complete: pin-from-test-site + pin-enclosing-call;
  fix its one `bed_name`/`_name` mismatch).
- **Step into → `05-variant-fold`** (teaches step-into on a growing `fold_left`
  accumulator, reuses the `Plant` domain; port the breadcrumb-bar prose from
  `05-step-into`).
- **Sample colors → `10-sample-colors`** (polished; explains why colors pair up;
  reuses MoonPhase/Plant).

## Cuts (confirmed against the task feature audit)
- **Partial application** and **pipelining `|>`** — zero tasks use either (the
  `, _)` occurrences in tasks are tuple patterns, not partial application).
- **Advanced labeled tuples** (extension / omission / list-conversions) and the
  **standalone float-arithmetic** slide — already archived; stay cut.
- Basics **17 (mean-of-string-integers)** capstone — replace with a garden
  map+fold capstone (keeps the theme; optional).

## Additions
- A **tuples** slide in Act 1 (grouping + destructuring + `_`). Tuples and tuple
  destructuring are used heavily by tasks but were never given their own lesson.
- The **MoonPhase ADT** folded into the case slide (closes the "no ADT lesson"
  gap; threads continuity).
- **Strings as secondary content**: string literals + `++` concatenation seeded
  in Act 0/1; one simple regex (`string_match`) in an Act-5 writing warmup. We
  add small, straightforward secondary elements alongside the primary spine
  rather than as standalone slides.

## Continuity: one growing garden program
Commit to a single garden codebase threaded from the first arithmetic
(`250 * 7` weekly water) through `MoonPhase`, `watering_amount`, `Plant` records,
beds, and map/fold over beds, into the warmups and the (garden-themed) tasks.
Standardize: `Plant = (name, icon, water)`, `MoonPhase` with leading `+`, one
bed-naming scheme, `weekly = daily * 7`. **Not rigid**: where a disjoint example
covers technical content better, use it. Avoid samey/redundant repetition.

**Tone.** A night garden on lunar time, watering set by a brass moon-dial, a
caretaker's ledger recording each pour. Keep the strangeness subtle and dry: an
occasional one-line aside, never lore-dumps.

## Probe reference sidebar (progressive disclosure)
Use each slide's `@reference` sidebar to host a **probe quick-reference that
grows**. It appears the moment we first suggest a probe shortcut and stays up on
every slide after, probe-focused or not, gaining one entry per feature
introduced. Draft: hand-maintained cumulative block. Phase-2 infra: a
`GenTutorial` helper holding one ordered list of `(key, markdown)` entries plus a
`@flags probe_ref=<key>` directive that injects the cumulative prefix, so we
maintain the list in one place. Keep probe controls reachable from the **top bar**
(auto-probe 🔬 already lives there) so the right sidebar is free for the tutorial
reference. Defer the legend and color-scheme toggle.

## Prose style
Terse and light. No em-dashes. No ornate punctuation. Bold/italic only. A little
dry humor is fine; tighten anything loose.

## Naming: dynamic focus
Rename user-facing text only: the bar title `probe focus`, the legend title
`Sample Focus Legend`, and all tutorial prose ("dynamic cursor" → "dynamic
focus"). Leave the `SampleFocus*` code identifiers, DOM id, and CSS as-is.

## Gating
Introduce some, not rigidly. Lean exploration for "notice this" probe beats; gate
the small language exercises (tuples, if/clamp) and the writing warmups on hidden
tests. Look for probe activities that naturally gate (e.g. "pin the failing call,
then fix it so the test passes").

## The sequence (as built: 23 slides, full run-through playable on 8744)
```
ACT 0  Editor & one value
 01 Arithmetic & holes   02 Parser & backpack (intro let)
 03 Probes               04 Variables & exploring
ACT 1  Branching & data
 05 Tuples   06 Tuple Records (.field)   07 If expressions (clamp + multi-probe)
 08 Sum types, case & ∅   09 Variants with data (payload + Option)
ACT 2  Functions (0/1/many)
 10 Functions & many samples (←/→, Space; pink/blue colors mentioned)
 11 Aligning samples (dynamic focus, ⊖; green mentioned)
ACT 3  Auto-probe
 12 Auto-probe (🔬 follows cursor; line-break granularity)
ACT 4  Lists & iteration
 13 Lists (+ nth/length)   14 Map (+ fun lambda, filter/mapi)   15 Fold
 16 Pin (records)   17 Step into (records)
ACT 5  Writing & debugging
 18 Writing: running-sum (fold + tuple acc + @/cons)
 19 Writing: strings (filter + string_match regex)
 20 Print   21 Debugging warmup (mini-MVU: Action+Model+fold, planted bug)
ACT 6  Show-and-tell
 22 Sample colors   23 Greenhouse arena (free exploration)
```
Notes: dropped standalone multi-arg-functions (C-style multi-param shown
implicitly) and "writing: clamp" (clamp is the intro `if` slide). `fun` lambdas
introduced at Map. Records taught (06) and used in pin/step-into. Coverage now
spans every Tier-1 feature + Option/payload-variants/strings/cons. Still light:
deep mapi-on-2D-grid practice (only referenced), compound booleans `&& || !`.

## Phasing
- **Phase 1 (this draft):** assemble the ordered sequence; fully author Acts 0/1
  to the garden thread with probe beats and progressive reference; slot Acts 2–6
  from the chosen variants (renamed, with the dynamic-focus prose rename); point
  `gen-tutorial` at `conjoined/`; generate + build. Playable end to end.
- **Phase 2:** probe-ify and re-theme Acts 2–6 onto the garden codebase; split
  `probes/02` into Functions-many + Dynamic-focus; trim `probes/03` into
  auto-probe-intro + exploration arena; build the progressive-reference helper.
- **Phase 3:** wire warmups to mirror the tasks; add the regex exposure; finalize
  gating; polish the colors show-and-tell.
- **Phase 4 (infra, optional):** per-slide auto-probe flag; retire the
  Study-track `StudyTutorial##` duplicates; the points/milestone layer; the
  in-code "dynamic focus" rename if wanted.

## Open / undecided
1. Auto-probe on by default for the study build vs in-prose toggling vs a
   per-slide flag (leaning: in-prose now, flag if needed).
2. How much float to teach (tasks use none; the watering multiplier uses some).
   Leaning: integer-only multipliers in the garden thread, a light float aside.
3. Whether to keep both clamp and running-sum as warmups or pick one (time).
4. Exact debugging-warmup program (trim a plotter vs the 07-writing boundary bug).
5. The points mapping over the existing M0–M5 ladder (separate workstream).

## Status (2026-05-31)
Built and served (vite, port 8744). The ordered sequence is **20 slides** in
`hazel-programs/tutorial/conjoined/` (those `.hz` files are the source of truth;
the per-slide prose in `conjoined-tutorial-overview.md` predates this revision
and is stale for Acts 0-1).

**Done**
- Acts 0-1 reworked into 7 intro slides (01-07): arithmetic+holes merged;
  `let` introduced in the backpack slide; probes motivated by "intermediate
  values"; a richer "explore with probes" slide; tuples carry pattern type
  annotations (standalone type-annotation slide dropped); `clamp` teaches
  if + C-style function syntax + manual multi-probe + line-break granularity;
  case/∅ now has a fill-to-solve gate.
- Function examples in Acts 0-1 use C-style `let f(x: T): T = ...` (verified to
  parse/eval); `fun` deferred to higher-order functions.
- Acts 2-6 (slides 08-20) are the chosen variants, renumbered but **not yet
  re-themed** (still use `fun`, original examples; seams expected past slide 07).
- Tutorial nav bugs fixed and committed (8bf32bb6ab): selector label + position
  based prev/next.

**Todo**
- Phase 2: theme Acts 2-6 onto the garden codebase + switch them to C-style
  functions; split `probes/02` into functions-many + dynamic-focus; trim
  `probes/03`; build the progressive-reference `GenTutorial` helper.
- Regex/string exposure; debugging-warmup + exploration-arena slides.
- Resolve open items above; ongoing user playthrough + reactions.
