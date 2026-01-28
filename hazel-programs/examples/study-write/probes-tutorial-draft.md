# Introduction to Probes

<!-- META: This is Part 1, covering basics through closure cursor/sample navigation.
     Part 2 will introduce the larger program context and debugging features (auto, pin, step).

     The "larger program" I'm thinking ahead to: a Greenhouse Planner that:
     - Has multiple plants with care schedules
     - Uses moon phases for watering adjustments (the celestial tinge)
     - Has beds/zones with different conditions
     - Has tests that exercise the calculations
     - Will have a bug to debug (e.g., confusing daily vs weekly, or wrong phase multiplier)

     The function we build here (watering_amount) will be called by that larger system.
-->

Welcome! This tutorial introduces **probes**, Hazel's system for seeing the values of expressions as your program runs. Probes show you what's happening inside your code without leaving the editor.

We'll learn probes by building a small plant care calculator, then later integrate it into a larger greenhouse planner.

**Reading this tutorial:** When an expression has a probe, you'll see it in the editor with a colored underline. In this text-based tutorial, we represent probed expressions with brackets: `⟦expression⟧`. The value appears after `≡`, like: `⟦1 + 2⟧ ≡ 3`.

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

<!-- META: I believe Space toggles the mode. Need to verify this is the right key. -->

---

## Part 4: Following the Thread (Closure Cursor)

### Multiple Probes, Same Function

Let's add more probes to see intermediate steps:

```hazel
let watering_amount: (Int, MoonPhase) -> Int =
  fun ⟦base_ml⟧, ⟦phase⟧ ->
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
    fun ⟦base_ml⟧, ⟦phase⟧ ->     ≡ 250  50  180     ≡ Full  New  Waning
        ...
        | Full => ⟦0.88⟧                 ≡ 0.88
        ...
        let result = ⟦...⟧               ≡ 220  60  171
```

But which `base_ml` goes with which `result`? When there are multiple probes with multiple samples, how do you keep track?

---

### The Closure Cursor

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
     colored (green for same closure). We might want a different ASCII convention.
     Maybe: *50* or «50» or ⟦50⟧ -->

The samples that belong to the **same function call** are highlighted together. Click on `50` and you see the whole story of the cactus calculation: base=50, phase=New, multiplier=1.2, result=60.

**This is the closure cursor** — it tracks which execution context you're focused on, and shows you the corresponding values across all probes.

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

**Key insight:** The closure cursor keeps probes aligned. In single mode, arrow keys step through complete executions. In many mode, clicking a sample highlights its siblings.

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
| **Closure cursor** | Click a sample to highlight related samples from the same call |

---

## Coming Up: The Greenhouse Planner

<!-- META: This sets up Part 2. The larger program will have:
     - A Plant type with name, base_water, emoji
     - Multiple plants in a list
     - A Bed type with plants and a light_level
     - A schedule function that calculates weekly needs
     - A failing test that we'll debug using auto-probe, pin, and step-into
-->

Our `watering_amount` function is ready. In the next part, we'll integrate it into a larger **Greenhouse Planner** that manages multiple plants across different beds. We'll encounter a bug in the planner and use probes' advanced features — auto-probing, pinning, and step-into — to track it down.

You'll see values like these:

```hazel
type Plant = (
  name = String,
  icon = String,
  base_water = Int,
  sun_hours = Int
) in

let moonleaf: Plant = (
  name = "Moonleaf Fern",
  icon = "🌿",
  base_water = 250,
  sun_hours = 4
) in

let starbloom: Plant = (
  name = "Starbloom Orchid",
  icon = "🌸",
  base_water = 180,
  sun_hours = 6
) in
...
```

When probes show these richer values, you can see the whole structure — not just numbers, but the records and types that make up your program's world.

See you in Part 2!

---

<!-- META: END OF PART 1

Things I'm uncertain about:
1. Is Space the right key for toggling single/many mode? The probes-guide.md says Space.
2. The exact highlighting colors (green for same closure, pink for caller, cyan for callee)
   — I kept it simple and just said "highlighted" without specifying colors yet.
3. Whether we want to show the Pin/Step buttons in the sample dropdown this early,
   even if just to acknowledge they exist. Currently I'm hiding them.
4. The float_to_int / int_to_float functions — need to verify these exist in Hazel stdlib.

For Part 2 (the larger program), I'm envisioning:
- A Greenhouse with multiple Beds
- Each Bed has a list of Plants and a light condition
- A schedule function that calculates weekly water for each plant, adjusted by moon phase
- A display function that formats the schedule
- A BUG: somewhere the daily vs weekly calculation is wrong, or the moon phase
  is being looked up incorrectly, causing one test to fail
- We'll use auto-probe to see the whole flow, pin to focus on the failing case,
  and step-into to trace back to the source of the wrong value

Celestial tinge achieved through:
- MoonPhase type affecting watering calculations
- Plant names like "Moonleaf Fern", "Starbloom Orchid"
- Could add a CelestialCalendar or DayInfo type in Part 2

-->
