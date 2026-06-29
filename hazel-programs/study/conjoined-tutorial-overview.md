# Conjoined Tutorial — Slide-by-slide Overview

Companion to `conjoined-tutorial-plan.md`. Acts 0 and 1 are described at full
detail (act intro, per-slide purpose, code, probe beat, gating, continuity).
Acts 2–6 are sketched; they fill in during Phase 2.

Reading key: **reuses** = lifted from an existing slide that already round-trips;
**new** = freshly authored; ★ = probe beat woven into a language slide.

The garden thread in one place: `daily_ml = 250`, `days = 7`,
`weekly = 250 * 7 = 1750`; `type MoonPhase = New + Waxing + Full + Waning`;
`Plant = (name, icon, water)` with moonleaf 🌿 250, starbloom 🌸 180,
thornveil 🍄 50, dewcup ☘️ 200; `watering_amount(base, phase)`; beds hold plants.
The conceit: a night garden on lunar time, the moon set by a brass dial, each
pour written to a ledger.

---

## ACT 0 — The editor, and your first values

**One value per expression.** Get comfortable editing Hazel (holes, the
backpack), do a little arithmetic and `let`, and place your **first manual
probe** to read a single runtime value and its environment. The garden is seeded
here: the first number you compute is a week of watering.

### 01 — Holes  ·  reuses basics/01
Holes and live evaluation. Fill the hole so the week's water comes out right.
```
let target = 1750 in
let partial = 250 * ¿ in
target == partial
```
Fill `7`. Gated (`test answer end`). Seeds `250 * 7 = 1750` from the first
keystroke, which is exactly the number the first probe will read on slide 04.
Prompt keeps Alexander's holes explanation; retheme the number only.

### 02 — Parser & backpack  ·  reuses basics/02
The Tylr parser and the backpack (the `in` obligation). Pure editing mechanics;
build `let x = 1 in x + 1` with Tab. Gated (`== 2`). Left almost verbatim, it is
about the editor, not the garden. One light themed aside at most.

### 03 — Integer arithmetic  ·  reuses basics/03
`+ - * /`. Worked example is the watering total `250 * 7`; the task completes
`250 * ¿` to `1750`.
```
250 * ¿
```
Gated (`== 1750`). This is the expression the next slide probes, so the value is
already familiar.

### 04 — Your first probe  ·  new (from probes/01 front half)  ★
The first probe. The expression is complete so there is nothing to fix, only
something to inspect.
```
let weekly_water = 250 * 7 in
weekly_water
```
Prompt: put the cursor on `*`, press **Cmd/Ctrl+E** (or right-click, Add probe),
read `1750` inline. Then toggle it off and on. Exploration (no gate). This is the
milestone moment, so it gets its own light slide rather than riding on the
arithmetic lesson. **The progressive probe reference starts here**: the sidebar
gains "Add / remove a probe: Cmd/Ctrl+E or right-click."

### 05 — Let & the environment  ·  reuses basics/05 + probes/01  ★
`let ... in` binding, plus the **environment**: click a sample to see the
variables that fed it. A small string aside seeds concatenation.
```
let daily_ml = 250 in
let days = 7 in
let weekly = ¿ in
weekly
```
Fill `daily_ml * days`. Gated (`weekly == 1750`). Then probe `weekly` (or the
variable name on the left of a `let`), click the sample, read the environment
dropdown (`daily_ml = 250`, `days = 7`). Reference shows a string label example
`name ++ " needs water"` so `++` shows up early without a dedicated slide.
Reference gains "Click a sample to see its environment."

---

## ACT 1 — Branching and data

**Zero or one value.** Branches mean some code does not run. Introduce the data
the garden is made of (tuples, types, the MoonPhase sum type), then meet the `∅`
indicator: a probe on an untaken branch has nothing to show. This is the payoff
of the act and the reason branching comes before functions in the spine.

### 06 — Tuples  ·  new
Group related values and take them apart. Destructuring and the `_` wildcard.
This is the lesson tuples never had, and everything downstream (multi-arg
functions, case patterns, the Model/Action records of the tasks) leans on it.
```
let plant = ("moonleaf", 250) in
let (name, water) = plant in
name ++ " needs " ++ string_of_int(water) ++ "ml"
```
Gated (result `== "moonleaf needs 250ml"`). Carries more string practice
(`++`, `string_of_int`) as secondary content. Reference shows `let (name, _) =
plant` for the wildcard. Continuity: `("moonleaf", 250)` is the first `Plant`,
one slide before we name the record type.

### 07 — Type annotations  ·  reuses basics/07
The `:` operator and type errors, including a tuple type. Fix a mismatch:
```
("250" : Int)
```
Replace the string with the integer `250`. Gated (`== 250`). Reference lists
`Int Float Bool String` and a tuple annotation `(String, Int)` (a plant), so the
type of the slide-06 tuple is now spelled out. Themed number, same lesson.

### 08 — If expressions  ·  reuses basics/12
`if / then / else`, nesting, comparisons. The function is `clamp`, which keeps a
water amount inside a safe range, and which returns as a real writing task in Act
5.
```
let clamp = fun x -> x in
clamp(50)
```
Complete `clamp` to the range 0..100. Gated (`clamp(-5)==0; clamp(50)==50;
clamp(150)==100`). Reference: comparison operators. Light reframe of the prompt
toward "clamp a watering amount"; the code is unchanged and already validated.

### 09 — Sum types, case & the ∅ indicator  ·  reuses basics/13 + probes/01  ★
The MoonPhase sum type and `case`, then the `∅` payoff. Define the type, match a
phase to a watering adjustment, then probe the branches.
```
type MoonPhase = New + Waxing + Full + Waning in
let base_water = 250 in
let current_phase : MoonPhase = Full in

case current_phase
| New  => base_water + 50
| Full => base_water - 30
| _    => base_water
end
```
Prompt: probe each branch. The matched branch shows its value; the others show
**∅**, meaning that path was never taken. Change `Full` to `New` and watch which
branch lights up. Exploration (notice, do not gate). Reference gains "∅ means a
branch was never evaluated" and recalls tuple patterns from slide 06 (`case` can
destructure: `| (0, _) => ...`). This is the verbatim probes/01 tail, so it
round-trips, and it introduces both the ADT and `case` the audit asked for.

---

## ACT 2 — Functions (0 / 1 / many)  · sketch

A function called more than once produces **many** samples, one per call.

- **10 — Functions** (reuses basics/08 + probes/02 functions half). The garden
  function is `watering_amount`. Probe a value inside it and meet **many samples**
  with ←/→ navigation and single-vs-many mode (Space). Introduce **multi-probe**
  (right-click a `let`, Add multi probe) to probe every line of a definition at
  once. ★
- **11 — Multi-argument functions** (reuses basics/09). `watering_amount(base,
  phase)`. Tuples-as-arguments, building on slide 06.

Cut from here: partial application, pipelining.

## ACT 3 — Auto-probe & dynamic focus  · sketch

- **12 — Auto-probe** (trim of probes/03). Turn on the 🔬 mode. Probes flock to
  the definition under the cursor and follow it. Add line breaks to give each
  sub-expression room. The transition from placing probes to writing with them on.
- **13 — Dynamic focus & ⊖ alignment** (split from probes/02 back half). Selecting
  a sample aligns the others to the same call. `⊖` means samples exist but are not
  aligned; click to align. Renamed to **dynamic focus** throughout.

## ACT 4 — Lists & iteration  · sketch

Many calls, hard to find the one you want.

- **14 — Lists** (reuses basics/14). Lists of plants.
- **15 — Map** (reuses basics/15). Map a watering over a bed.
- **16 — Fold** (reuses basics/16). `fold_left` totals the water. The workhorse
  for the tasks.
- **17 — Pin** (reuses probes/04-pinning). Many fold/map samples; pin one call to
  filter every probe to its run. ★
- **18 — Step into** (reuses probes/05-variant-fold). Step into one fold
  iteration; the breadcrumb bar shows your place in the stack; the matching sample
  highlights. ★

## ACT 5 — Writing & debugging warmups  · sketch

- **19 — Writing: clamp** (reuses probes/08). The first real writing task.
  **Auto-probe is ON and emphasized**, so values appear as they type. clamp came
  up in Act 1, so the shape is familiar.
- **20 — Writing: running-sum** (reuses probes/09). `fold_left` with a tuple
  accumulator. Also a real task. Carry the simple **regex** exposure here or in a
  sibling warmup (`string_match` on a plant label).
- **21 — Print** (reuses probes/06). The print panel, the study's debugging
  baseline.
- **22 — Debugging warmup** (new, small). A garden program with one planted bug;
  use pin and step-into to find it. Mirrors the real debugging tasks so the last
  tutorial slide and the first task feel continuous.

## ACT 6 — Show-and-tell & free exploration  · sketch

- **23 — Sample colors** (reuses probes/10-sample-colors). "Here is more; you can
  ignore the colors." Framed against dynamic focus.
- **24 — Exploration arena** (the full greenhouse program). Free poking. Doubles
  as a qualitative-feedback surface and the on-ramp to the tasks.

---

## Dependency check (taught before used)
`holes → backpack → arithmetic → probe → let/env → tuples → type annotations →
if → case/ADT → functions → multi-arg → auto-probe → dynamic focus → lists → map
→ fold → pin → step-into → warmups`. Every construct is introduced before it is
relied on. The two frictions in Alexander's original order are resolved: `if`
now precedes the partial-application use site (which is cut anyway), and `map`
is introduced before the fold/pin/step-into slides that lean on it. Tuples
precede their use in multi-arg functions and case patterns.
