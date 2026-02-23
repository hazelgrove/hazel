<!-- NOTE FOR ANDREW:
     Feature idea to consider: When hovering over an ⊖ (not aligned) icon,
     show the first value that is hidden by it. This would help users understand
     what they're missing without having to change the dynamic cursor. Put this
     in a GitHub issue.
-->

# Introduction to Probes

<!-- META: Covers basics through advanced debugging features.
     Theme: Greenhouse Planner / plant care calculator.
     Verified against codebase 2026-02-22: keyboard shortcuts, icon meanings,
     pin/step-into behavior all confirmed.
-->

Welcome! This tutorial introduces **probes**, Hazel's system for seeing the values of expressions as your program runs. Probes show you what's happening inside your code without leaving the editor.

We'll learn probes by building a small plant care calculator, then later integrate it into a larger greenhouse planner.

**Reading this tutorial:** When an expression has a probe, you'll see it in the editor with a colored underline. In this text-based tutorial, we represent probed expressions with brackets: `⟦expression⟧`. The value appears after `≡`, like: `⟦1 + 2⟧ ≡ 3`.

---

## Quick Reference

<!-- This will also appear as a terse cheat sheet in the sidebar during the study. -->

| Action | How |
|--------|-----|
| **Add/remove probe** | Right-click → "Add probe", or `Ctrl+E` (`Cmd+E` on Mac) |
| **See environment** | Click a sample → dropdown shows variable values |
| **Pin this call** | Click sample → dropdown → **Pin this call** (or press `P`) |
| **Pin enclosing call** | Same, on a non-function-call probe — pins the nearest enclosing call |
| **Step into** | Click sample on a function call → dropdown → **Step Into** (or press `Enter`) |
| **Single/Many mode** | Double-click a sample, or press `Space` |
| **Navigate samples** | `←` / `→` arrow keys |
| **Auto-probe toggle** | **A/M** button at top-right of screen |
| **Resize sample** | `Shift + drag` horizontally on a sample |
| **Reset probe state** | `Shift + Escape` (clears pins, resets cursor) |
| **Clear all probes** | Button in the dynamic cursor bar (top of editor) |

**Status icons:**

| Icon | Meaning |
|------|---------|
| `∅` | Expression was never evaluated (branch not taken, function not called) |
| `⍟` | Sample exists but is hidden by a pin — click to unpin |
| `⊖` | Not aligned — this probe has no sample for the current dynamic cursor position |

---

## Part 1: Seeing Values

### Your First Probe

Let's start simple. Here's a calculation:

```hazel
let weekly_water = 250 * 7 in
weekly_water
```

This multiplies 250 (milliliters per day) by 7 (days) to get a weekly amount. But what if we want to *see* that value right here, without scrolling to the bottom of our program?

**Try this:** Put your cursor on the expression `250 * 7` and either:
- Press `Ctrl+E` (or `Cmd+E` on Mac), or
- Right-click and select "Add probe" from the menu

<!-- META: ASCII illustration of context menu. We could replace with screenshot. -->

```
┌─────────────────┐
│ Add probe       │  ← Select this
│ Go to definition│
│ Find references │
└─────────────────┘
```

Now you should see:

```hazel
let weekly_water = ⟦250 * 7⟧     ≡ 1750
in weekly_water
```

The `≡ 1750` appearing to the right is the **sample** — the value that expression took during execution.

<!-- META: Should we show the underline in ASCII? Maybe with ~~~~ under the expression? -->

**Key insight:** Probes show values *inline*, right next to your code. No print statements, no console, no context switching.

---

### Probing Expressions with Variables

Now let's name our values:

```hazel
let plant_name = "Moonleaf Fern" in
let daily_ml = 250 in
let days = 7 in
let weekly_water = ⟦daily_ml * days⟧     ≡ 1750
in weekly_water
```

The probe still shows `1750`, but now it involves variables. What if you want to know what `daily_ml` and `days` were when this was calculated?

**Try this:** Hover over (or click on) the sample `1750`. A dropdown appears:

```
┌──────────────────────┐
│   daily_ml  ≡  250   │
│   days      ≡  7     │
└──────────────────────┘
```

This is the **environment** — all the variables that were in scope when this expression was evaluated, along with their values.

<!-- META: The actual dropdown also has Pin/Step buttons above the environment
     when it's a function application sample. We'll introduce those later.
     For now I'm just showing the environment part. -->

**Key insight:** Probes capture not just the value, but the context. Hover to see what the variables were.

---

### Probing the Variable Itself

You can also probe patterns, like variable names in `let` bindings:

```hazel
let ⟦daily_ml⟧ = 250     ≡ 250
in daily_ml * 7
```

This shows the value being bound to `daily_ml`. Pattern probes appear with a blue decoration (versus green for expressions).

Both approaches — probing the expression or probing the pattern — show you values. Use whichever feels natural for what you're investigating.

---

## Part 2: Branches and Control Flow

### Not Every Expression Runs

Consider this moon phase adjustment for our plant watering:

```hazel
type MoonPhase = + New + Waxing + Full + Waning in

let current_phase = Full in
let base_water = 250 in

let adjusted = case current_phase
  | New => ⟦base_water + 50⟧       ≡ ???
  | Full => ⟦base_water - 30⟧      ≡ 220
  | _ => ⟦base_water⟧              ≡ ???
  end
in adjusted
```

<!-- META: The ??? are placeholders to show "no sample". In actual rendering,
     these would show the ∅ icon. Let me revise to make that clearer. -->

What do you see? Only **one** probe has a sample — the `Full` branch showing `220`. The others show a special icon: `∅`

```hazel
let adjusted = case current_phase
  | New => ⟦base_water + 50⟧       ∅  ← never evaluated
  | Full => ⟦base_water - 30⟧      ≡ 220
  | _ => ⟦base_water⟧              ∅  ← never evaluated
  end
```

**Key insight:** Probes only have samples when the expression is actually evaluated. The `∅` icon means "this code path wasn't taken."

**Try this:** Change `current_phase` to `New` and watch the samples shift — now the `New` branch lights up and `Full` goes dark.

---

### The Same with If-Expressions

The principle applies to any branching construct:

```hazel
let needs_extra = true in
let amount = if needs_extra
  then ⟦300⟧     ≡ 300
  else ⟦200⟧     ∅
in amount
```

Only the branch that executes has a sample.

---

## Part 3: Functions and Multiple Samples

### From Constant to Function

So far, our calculations have been for one plant. Let's generalize. Instead of a fixed `daily_ml`, let's make a function that works for any plant:

```hazel
type MoonPhase = + New + Waxing + Full + Waning in

let watering_amount: (Int, MoonPhase) -> Int =
  fun base_ml, phase ->
    let adjusted = case phase
      | New => ⟦base_ml + 50⟧
      | Full => ⟦base_ml - 30⟧
      | _ => ⟦base_ml⟧
      end
    in adjusted
in
watering_amount
```

Right now, if you look at the probes inside the function... they all show `∅`. Why?

**The function hasn't been called yet.** The code inside a function only runs when you call it.

---

### One Call, One Sample

Let's call our function:

```hazel
let watering_amount: (Int, MoonPhase) -> Int =
  fun base_ml, phase ->
    case phase
      | New => ⟦base_ml + 50⟧      ∅
      | Full => ⟦base_ml - 30⟧     ≡ 220
      | _ => ⟦base_ml⟧             ∅
      end
in

watering_amount(250, Full)
```

Now the `Full` branch has a sample. We called the function with `(250, Full)`, so `base_ml` was 250 and `phase` was `Full`, leading to `250 - 30 = 220`.

---

### Multiple Calls, Multiple Samples

Here's where probes get interesting. Let's call the function multiple times:

```hazel
let watering_amount: (Int, MoonPhase) -> Int =
  fun base_ml, phase ->
    let adjusted = ⟦case phase
      | New => base_ml + 50
      | Full => base_ml - 30
      | _ => base_ml
      end⟧
    in adjusted
in

let fern_water = watering_amount(250, Full) in
let cactus_water = watering_amount(50, New) in
let orchid_water = watering_amount(180, Waning) in

(fern_water, cactus_water, orchid_water)
```

<!-- META: I moved the probe to wrap the whole case expression for simplicity.
     This way we see one probe with multiple samples rather than
     three probes each with some samples. Easier to introduce the concept. -->

Now the probe shows **three samples** — one for each call:

```hazel
    let adjusted = ⟦case phase ...⟧     ≡ 220  100  180
```

<!-- META: In "many mode" these appear side by side. Need to show this. -->

The three values are `220` (fern), `100` (cactus: 50+50), and `180` (orchid: unchanged).

**Key insight:** A probe inside a function body collects a sample *each time* that function is called. Multiple calls = multiple samples.

---

### Navigating Between Samples

With three samples showing, how do you know which is which?

**Click on a sample** to select it. When selected:
- The sample gets a highlight (green outline)
- Hovering shows the environment for *that specific call*

```
Clicking on "100":
┌──────────────────────┐
│   base_ml  ≡  50     │
│   phase    ≡  New    │
└──────────────────────┘
```

**Use arrow keys** to move between samples:
- `←` / `→` moves to previous/next sample

**Try this:** Click on the first sample (220), then press `→` twice to step through each function call.

---

### Single Mode vs Many Mode

You've been looking at **many mode**, where all samples appear side by side:

```
≡ 220  100  180
```

Sometimes you want to focus on one call at a time. **Double-click** on a sample (or press `Space`) to toggle to **single mode**:

```
≡ 220
```

Now only one sample shows. Use `←` / `→` to step through them one at a time.

**Double-click again** (or press `Space`) to return to many mode.

---

## Part 4: Following the Thread (Dynamic Cursor)

### Multiple Probes, Same Function

Let's add more probes to see intermediate steps:

```hazel
let watering_amount: (Int, MoonPhase) -> Int =
  fun
    ⟦base_ml⟧,
    ⟦phase⟧
  ->
    let multiplier = case phase
      | New => ⟦1.2⟧
      | Full => ⟦0.88⟧
      | Waxing => ⟦1.1⟧
      | Waning => ⟦0.95⟧
      end
    in
    let result = ⟦float_to_int(int_to_float(base_ml) *. multiplier)⟧
    in result
in

let fern = watering_amount(250, Full) in
let cactus = watering_amount(50, New) in
let orchid = watering_amount(180, Waning) in
(fern, cactus, orchid)
```

<!-- META: Changed to float multiplier for more interesting values.
     Using Hazel's float_to_int and int_to_float for conversion. -->

Now we have several probes, each with three samples:

```
    fun
      ⟦base_ml⟧,     ≡ 250  50  180
      ⟦phase⟧     ≡ Full  New  Waning
    ->
        ...
        | Full => ⟦0.88⟧                 ≡ 0.88
        ...
        let result = ⟦...⟧               ≡ 220  60  171
```

But which `base_ml` goes with which `result`? When there are multiple probes with multiple samples, how do you keep track?

---

### The Dynamic Cursor

**Click on a sample** and watch what happens to the *other* probes.

When you click on `50` (the cactus's `base_ml`):

```
    fun ⟦base_ml⟧, ⟦phase⟧ ->     ≡ 250  [50]  180     ≡ Full  [New]  Waning
                                         ^^^^                  ^^^^^
        ...                              highlighted           highlighted
        | New => ⟦1.2⟧                   ≡ [1.2]
                                           ^^^^^
        ...                                highlighted
        let result = ⟦...⟧               ≡ 220  [60]  171
                                                ^^^^
                                                highlighted
```

<!-- META: Using [brackets] to indicate highlighting. In the real UI this would be
     colored (green for same function execution). We might want a different ASCII convention.
     Maybe: *50* or «50» or ⟦50⟧ -->

The samples that belong to the **same function call** are highlighted together. Click on `50` and you see the whole story of the cactus calculation: base=50, phase=New, multiplier=1.2, result=60.

**This is the dynamic cursor** — it tracks which execution context you're focused on, and shows you the corresponding values across all probes.

---

### Arrow Keys Maintain Alignment

Here's something powerful: in **single mode**, when you press `←` / `→` to move between samples on one probe, the *other probes move too*.

**Try this:**
1. Switch to single mode (double-click or Space)
2. All probes now show just one sample each — all from the same call
3. Press `→` to advance
4. All probes advance together, showing the next call's values

```
Before pressing →:
    fun ⟦base_ml⟧, ⟦phase⟧ ->     ≡ 250     ≡ Full
    ...
    let result = ⟦...⟧                   ≡ 220

After pressing →:
    fun ⟦base_ml⟧, ⟦phase⟧ ->     ≡ 50      ≡ New
    ...
    let result = ⟦...⟧                   ≡ 60
```

You're stepping through executions of the function, seeing how all the values relate.

**Key insight:** The dynamic cursor keeps probes aligned. In single mode, arrow keys step through complete executions. In many mode, clicking a sample highlights its siblings.

---

## Part 5: Adding Tests

Let's solidify our function with some tests:

```hazel
type MoonPhase = + New + Waxing + Full + Waning in

let watering_amount: (Int, MoonPhase) -> Int =
  fun base_ml, phase ->
    let multiplier = case phase
      | New => 1.2
      | Full => 0.88
      | Waxing => 1.1
      | Waning => 0.95
      end
    in
    float_to_int(int_to_float(base_ml) *. multiplier)
in

test watering_amount(250, Full) == 220 end;
test watering_amount(50, New) == 60 end;
test watering_amount(180, Waning) == 171 end;
test watering_amount(100, Waxing) == 110 end
```

Tests drive evaluation, which means they generate samples for any probes inside the function. This is useful: write tests first, and probes inside your function show you what's happening as you implement.

---

## Recap: What We've Learned

| Concept | What It Does |
|---------|--------------|
| **Adding a probe** | `Ctrl+E` or context menu on any expression or pattern |
| **Probed expression** | Shown with colored underline (represented as `⟦expr⟧` in this tutorial) |
| **Sample** | The value shown to the right (`≡ 1750`) |
| **Environment hover** | Click/hover on sample to see variable values |
| **`∅` icon** | Expression was never evaluated (branch not taken, function not called) |
| **Multiple samples** | One per function call, shown side-by-side in many mode |
| **Single/Many mode** | Double-click or Space to toggle |
| **Arrow keys** | Navigate between samples |
| **Dynamic cursor** | Click a sample to highlight related samples from the same call |

See the **Quick Reference** at the top for a complete summary of interactions.

---

## Part 6: The Greenhouse Planner

Now let's see how probes work in a larger program. Here's a Greenhouse Planner that uses our `watering_amount` logic to manage multiple plants:

<!-- META: This is the "larger program" that our earlier work integrates into.
     I'm presenting it as something the user is exploring, not writing from scratch. -->

```hazel
# ═══════════════════════════════════════════════════════ #
#                  GREENHOUSE PLANNER                     #
# ═══════════════════════════════════════════════════════ #

type MoonPhase = + New + Waxing + Full + Waning in

type Plant = (
  name = String,
  icon = String,
  base_water = Int
) in

type Bed = (
  name = String,
  plants = [Plant],
  shade_level = Int    # 0 = full sun, 3 = deep shade #
) in

# ─── Plants ─────────────────────────────────────────── #

let moonleaf: Plant = (
  name = "Moonleaf Fern",
  icon = "🌿",
  base_water = 250
) in

let starbloom: Plant = (
  name = "Starbloom Orchid",
  icon = "🌸",
  base_water = 180
) in

let thornveil: Plant = (
  name = "Thornveil Cactus",
  icon = "🌵",
  base_water = 50
) in

let dewcup: Plant = (
  name = "Dewcup Lily",
  icon = "🪷",
  base_water = 200
) in

# ─── Beds ───────────────────────────────────────────── #

let shade_garden: Bed = (
  name = "Shade Garden",
  plants = [moonleaf, dewcup],
  shade_level = 2
) in

let sun_terrace: Bed = (
  name = "Sun Terrace",
  plants = [starbloom, thornveil],
  shade_level = 0
) in

# ─── Watering Calculations ──────────────────────────── #

let phase_multiplier: MoonPhase -> Float =
  fun phase -> case phase
    | New => 1.2
    | Waxing => 1.1
    | Full => 0.88
    | Waning => 0.95
  end
in

let shade_multiplier: Int -> Float =
  fun shade -> case shade
    | 0 => 1.0
    | 1 => 0.9
    | 2 => 0.75
    | _ => 0.6
  end
in

let daily_water: (Plant, Bed, MoonPhase) -> Int =
  fun plant, bed, phase ->
    let base = int_to_float(plant.base_water) in
    let phase_adj = base *. phase_multiplier(phase) in
    let shade_adj = phase_adj *. shade_multiplier(bed.shade_level) in
    float_to_int(shade_adj)
in

let weekly_water: (Plant, Bed, MoonPhase) -> Int =
  fun plant, bed, phase ->
    daily_water(plant, bed, phase) * 7
in

# ─── Schedule Generation ────────────────────────────── #

let plant_schedule: (Plant, Bed, MoonPhase) -> String =
  fun plant, bed, phase ->
    let daily = daily_water(plant, bed, phase) in
    let weekly = weekly_water(plant, bed, phase) in
    plant.icon ++ " " ++ plant.name ++ ": "
      ++ int_to_string(daily) ++ "ml/day, "
      ++ int_to_string(weekly) ++ "ml/week"
in

let bed_schedule: (Bed, MoonPhase) -> [String] =
  fun bed, phase ->
    map(bed.plants, fun plant -> plant_schedule(plant, bed, phase))
in

# ─── Tests ──────────────────────────────────────────── #

test daily_water(moonleaf, shade_garden, Full) == 165 end;
test daily_water(thornveil, sun_terrace, New) == 60 end;
test weekly_water(moonleaf, shade_garden, Full) == 1155 end;
test weekly_water(starbloom, sun_terrace, Waning) == 1197 end
```

This is a lot of code! When you're working with a program this size, manually placing probes everywhere would be tedious. That's where **auto-probe** comes in.

---

## Part 7: Auto-Probe Mode

### Seeing Everything at Once

Instead of adding probes one by one, you can turn on **auto-probe mode**. This automatically places probes on each line of the current definition, giving you a live view of the whole function.

**Try this:**
1. Put your cursor inside the `daily_water` function
2. Toggle auto-probe mode using the **A/M** button at the top-right of the screen (it switches between **A**uto and **M**anual mode)

Now every line in `daily_water` has a probe:

```hazel
let daily_water: (Plant, Bed, MoonPhase) -> Int =
  fun plant, bed, phase ->
    let base = ⟦int_to_float(plant.base_water)⟧     ≡ 250.  180.  50.  200.
    in
    let phase_adj = ⟦base *. phase_multiplier(phase)⟧     ≡ 220.  158.4  60.  190.
    in
    let shade_adj = ⟦phase_adj *. shade_multiplier(bed.shade_level)⟧     ≡ 165.  158.4  60.  142.5
    in
    ⟦float_to_int(shade_adj)⟧     ≡ 165  158  60  142
in
```

<!-- META: The actual samples depend on which tests/calls drive evaluation.
     With the 4 tests, we'd see 4 samples per probe. I'm showing approximate values. -->

**Key insight:** Auto-probe gives you a bird's-eye view of data flowing through a function. You can see every intermediate step without manually instrumenting anything.

---

### Auto-Probe Follows Your Cursor

Auto-probe is *local* to the current definition. As you move your cursor to different functions, the probes follow:

- Move to `phase_multiplier` → see probes there
- Move to `weekly_water` → see probes there
- Move back to `daily_water` → probes return

This keeps the display focused and avoids overwhelming you with data from the entire program.

**Try this:** Click into different functions and watch the probes shift.

---

### Writing with Auto-Probe

Auto-probe isn't just for reading existing code — it's powerful when *writing* new code too.

Imagine you're adding a new function to calculate total weekly water for a bed:

```hazel
let bed_total: (Bed, MoonPhase) -> Int =
  fun bed, phase ->
    let amounts = map(bed.plants, fun p -> weekly_water(p, bed, phase)) in
    fold_left(amounts, fun (acc, x) -> acc + x, 0)
in
```

With auto-probe on, as soon as you write each line:

```hazel
let bed_total: (Bed, MoonPhase) -> Int =
  fun bed, phase ->
    let amounts = ⟦map(bed.plants, fun p -> weekly_water(p, bed, phase))⟧     ≡ [1155, 980]  [420, 1197]
    in
    ⟦fold_left(amounts, fun (acc, x) -> acc + x, 0)⟧     ≡ 2135  1617
in
```

You see results *immediately* — no need to wait until your function is complete to test it. The tests at the bottom of the program drive the evaluation, and you see live feedback as you type.

<!-- META: This is a key selling point for auto-probe during writing.
     The values depend on having tests that exercise this function. -->

---

## Part 8: Debugging with Probes

### A Bug Appears

Let's say someone reports that the watering schedule is wrong. You add a test for a specific case you know should work:

```hazel
test weekly_water(dewcup, shade_garden, Waxing) == 1155 end
```

But the test **fails**. The expected value is `1155`, but the actual result is something else.

<!-- META: The actual bug and values need to be worked out carefully.
     Let me set up a plausible scenario. With:
     - dewcup.base_water = 200
     - shade_garden.shade_level = 2 (multiplier 0.75)
     - Waxing phase (multiplier 1.1)
     - daily = 200 * 1.1 * 0.75 = 165
     - weekly = 165 * 7 = 1155

     So if someone made the bug be using daily instead of weekly somewhere,
     or forgot the * 7, the test would fail. Let me introduce a subtle bug. -->

How do you find the problem? With probes, you can trace the calculation step by step.

---

### Narrowing Down with Pin

The program has many function calls — four tests means four executions of `daily_water` and `weekly_water`. With auto-probe showing all of them, you see a wall of samples:

```hazel
let daily_water: (Plant, Bed, MoonPhase) -> Int =
  fun plant, bed, phase ->
    let base = ⟦int_to_float(plant.base_water)⟧     ≡ 250.  180.  50.  200.  200.
    ...
```

Five samples now (four original tests plus your new one). Which one is the failing case?

**Pin** lets you focus on a specific call. Here's how:

1. Find the call site you care about — in this case, the test that fails
2. Add a probe to the function call: `⟦weekly_water(dewcup, shade_garden, Waxing)⟧`
3. Click on that sample to select it
4. In the **sample dropdown** that appears, click **"Pin this call"** (or press `P`)

```
┌──────────────────────────────────────────┐
│  [Pin this call]   [Step Into]           │
├──────────────────────────────────────────┤
│  plant   ≡  (name="Dewcup Lily", ...)    │
│  bed     ≡  (name="Shade Garden", ...)   │
│  phase   ≡  Waxing                       │
└──────────────────────────────────────────┘
```

Notice the dropdown shows the argument values with their names — `plant = (name="Dewcup Lily", ...)` — so you can confirm you're looking at the right call.

Now **only** samples from this specific call are shown. The other four executions are filtered out:

```hazel
let daily_water: (Plant, Bed, MoonPhase) -> Int =
  fun plant, bed, phase ->
    let base = ⟦int_to_float(plant.base_water)⟧     ≡ 200.
    in
    let phase_adj = ⟦base *. phase_multiplier(phase)⟧     ≡ 220.
    in
    let shade_adj = ⟦phase_adj *. shade_multiplier(bed.shade_level)⟧     ≡ 165.
    in
    ⟦float_to_int(shade_adj)⟧     ≡ 165
```

Now you can see exactly what happened for the dewcup in the shade garden during a waxing moon.

**Key insight:** Pin filters samples to a specific call. Use it when there are many executions and you need to focus on one.

---

### The Pin Icon

When you have a pin active, some probes might show no samples (because they weren't reached in the pinned call). These show a special icon: `⍟`

```hazel
| New => ⟦1.2⟧     ⍟  ← not this branch (pinned call used Waxing)
| Waxing => ⟦1.1⟧     ≡ 1.1
```

If you see `⍟` and are confused why there's no sample, **click on it** — this clears the pin and shows all samples again.

---

### The Not-Aligned Icon

There's one more status icon to know about: `⊖`. This appears in **single mode** when a probe has samples, but none of them match the current dynamic cursor position.

For example, if you're in single mode looking at the fern call (base_ml=250), but a probe only has samples from a different call, it shows `⊖` instead of a value. This just means "I have data, but not for the execution you're currently focused on."

If you see `⊖` and want to understand it: switch to **many mode** (double-click or Space) to see all available samples, or use the arrow keys to navigate to a different execution.

---

### Pinning from Non-Application Probes

So far, we've pinned from probes on function *calls* like `weekly_water(...)`. But you can also pin from probes on regular expressions inside a function body.

When you click on a sample of a non-application probe (like a `let` binding or a `case` branch), the dropdown shows **"Pin enclosing call"** instead of "Pin this call". This pins the nearest function call that contains the probed expression — filtering all probes to show only samples from that same execution.

This is useful when you're already looking at values inside a function and want to lock your view to that particular call without navigating to the call site.

---

### Step Into: Following the Call Stack

You've pinned the failing call and you can see the values in `daily_water`. But what if the bug is deeper — inside `phase_multiplier` or `shade_multiplier`?

**Step Into** lets you jump from a function call into that function's body, keeping your pinned context.

1. Find a function call inside `daily_water`, like `phase_multiplier(phase)`
2. Add a probe to it: `⟦phase_multiplier(phase)⟧`
3. Click on the sample to open the **sample dropdown**
4. Click **Step Into** (or press `Enter`)

```
┌──────────────────────────────────┐
│  [Pin this call]  [Step Into]    │  ← Click "Step Into"
├──────────────────────────────────┤
│  phase  ≡  Waxing                │
└──────────────────────────────────┘
```

Your cursor jumps to `phase_multiplier`, and auto-probe shows that function's internals — still pinned to the context of your failing test.

```hazel
let phase_multiplier: MoonPhase -> Float =
  fun phase -> case ⟦phase⟧     ≡ Waxing
    | New => ⟦1.2⟧     ⍟
    | Waxing => ⟦1.1⟧     ≡ 1.1
    | Full => ⟦0.88⟧     ⍟
    | Waning => ⟦0.95⟧     ⍟
  end
```

You can see: the phase is `Waxing`, and the multiplier is `1.1`. That looks correct.

Step into the next suspect — `shade_multiplier` — and continue tracing until you find the discrepancy.

**Key insight:** Step Into + Pin together let you trace backwards from a failing result to find where things went wrong, without losing context.

---

### Clearing the Pin

When you're done debugging and want to see all samples again:

- Click on the `⍟` icon on any probe that shows it, or
- Press `Shift+Escape` to reset all probe state

---

### The Dynamic Cursor Bar

When you step into a function, you might wonder: where am I in the call stack? The **dynamic cursor bar** at the top of the editor shows you.

It displays a breadcrumb trail of function calls leading to your current position:

```
  ○ ❯ run ❯ fold_left ❯ update
```

Each entry is a function name from the call stack. The `○` at the left represents the top-level program. As you step into deeper calls, entries appear to the right.

**Clicking entries navigates the call stack:**
- Click a **function name** to jump to that function's definition
- Click a **❯ chevron** to jump to the call site (the application expression)

This makes it easy to move up and down the call stack during debugging — click a parent function name to see where the call was made, or click back into a deeper function to continue tracing.

The bar also has a **clear-all button** to remove all probes at once.

---

## Part 9: Print Statements

Hazel also has traditional print-statement debugging. You can call `print` on any expression:

```hazel
let x = 42 in
print(x);
x + 1
```

`print(expr)` outputs the value of `expr` to the **print panel** in the sidebar, then returns unit (`()`). Because it returns unit, you need to sequence it with `;` before the next expression.

The print panel shows each printed value with a sequence number and source line number, making it easy to match output to code.

**When to use print vs. probes:** Print statements work well for quick checks and for seeing values in order of execution. Probes work well when you want to see multiple values at once, compare across function calls, or trace through the call stack without manually adding and removing output statements.

---

## Part 10: Seeing Richer Values

### Records and Algebraic Types

Throughout this tutorial, probes have shown simple values: numbers, floats, strings. But Hazel's probes handle complex values too.

When you probe an expression that returns a Plant:

```hazel
⟦moonleaf⟧     ≡ (name="Moonleaf Fern", icon="🌿", base_water=250)
```

Or a list of strings from `bed_schedule`:

```hazel
⟦bed_schedule(shade_garden, Full)⟧     ≡ ["🌿 Moonleaf Fern: 165ml/day...", "🪷 Dewcup Lily: ..."]
```

The sample shows the structure of the data — field names, list brackets, constructor tags for sum types.

---

### Resizing Samples

Sometimes values are long and get truncated. You can **resize** a sample to see more:

**Hold Shift and drag** horizontally on a sample to expand or contract it.

```
Before:  ≡ (name="Moonleaf F...
After:   ≡ (name="Moonleaf Fern", icon="🌿", base_water=250)
```

The abbreviation algorithm keeps the structure visible even when shortened — it collapses branches rather than just cutting off text.

---

## Recap: Advanced Features

| Feature | What It Does |
|---------|--------------|
| **Auto-probe** | Automatically probes each line of the current definition (A/M toggle, top-right) |
| **Pin this call** | Filters samples to a specific function call; in sample dropdown or press `P` |
| **Pin enclosing call** | Same, from a non-application probe — pins the nearest enclosing call |
| **`⍟` icon** | "No sample due to pin" — click to clear pin |
| **`⊖` icon** | "Not aligned" — no sample for current dynamic cursor position |
| **Step Into** | Jump into a function call while keeping pin context; in dropdown or press `Enter` |
| **Dynamic cursor bar** | Breadcrumb trail at top of editor; click to navigate call stack |
| **Print statements** | `print(expr);` outputs to sidebar print panel |
| **Shift+Escape** | Reset all probe state, clear pins |
| **Shift+drag** | Resize sample display |

---

## What We've Built

You now have tools to:

1. **See values** inline as your code runs (basic probes)
2. **Navigate** between multiple samples from function calls (dynamic cursor)
3. **Get an overview** of an entire function (auto-probe)
4. **Focus** on a specific execution (pin)
5. **Trace** through the call stack (step into, dynamic cursor bar)
6. **Print** values to the sidebar for sequential inspection

Whether you're writing new code and want live feedback, or debugging a failing test and need to trace values, probes give you visibility into your program's runtime behavior without leaving the editor.

Happy gardening!

---

<!-- META: Remaining TODOs:
1. The bug scenario in Part 8 is still somewhat vague. Should make it concrete with
   an actual bug (e.g., wrong multiplier, off-by-one in shade level mapping).
2. The sample values throughout should be verified against actual calculations.
3. Consider whether Part 9 (print) should be earlier or later — currently placed
   after debugging features since it's the baseline comparison tool.
-->
