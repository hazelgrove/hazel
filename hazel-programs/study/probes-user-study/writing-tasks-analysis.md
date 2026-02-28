# Writing Tasks Analysis

Analysis of existing program-writing tasks against research questions.

---

## Relevant RQs for Program Writing

**RQ3a**: Does auto-probe help catch errors *earlier* during writing (before completing implementation)?

**RQ3b**: How do live intermediate values affect API exploration / understanding unfamiliar functions?

**RQ3c**: Do programmers use probes to guide implementation differently than write-then-test?

---

## Current Writing Tasks Analysis

| Task | Complexity | Pipeline? | Accumulator? | API Discovery? | RQ Fit |
|------|-----------|-----------|--------------|----------------|--------|
| **Mentions Extractor** | Medium | Yes (split→filter→map) | No | Yes (string_split order) | **RQ3a, RQ3b** ✓ |
| **Base Route** | Medium | Yes (split→nth) | No | Yes (string_split) | **RQ3b** ✓ |
| **Running Sum** | Medium-High | No | Yes (fold) | No | **RQ3a, RQ3c** ✓ |
| **Clamp** | Very Low | No | No | No | Weak |
| **Safe Head** | Very Low | No | No | No | Weak |
| **Last Element** | Low | No | Yes (simple fold) | No | Marginal |
| **EmojiPaint Extend** | Medium | Implicit | No | Some (mapi patterns) | **RQ3c** ✓ |

### Verdict

**Strong tasks** (exercise probe benefits well):
- **Mentions Extractor** - Pipeline visibility, API discovery
- **Running Sum** - Accumulator evolution visible across fold iterations
- **EmojiPaint Extend** - See grid transformations as you build

**Weak tasks** (probes don't add much):
- **Clamp** - Just conditionals, too simple
- **Safe Head** - Just pattern match, too simple
- **Last Element** - Slightly better (fold), but simple

**Base Route** is decent but similar to Mentions.

---

## Theming Opportunities

### Easy to Theme (Night Garden):

| Task | Current | Night Garden Version |
|------|---------|---------------------|
| **Clamp** | `clamp(x, lo, hi)` | `clampMoisture(level, minSafe, maxSafe)` - soil moisture 0-100% |
| **Safe Head** | `safe_head(xs, default)` | `firstPlant(bed, emptyEmoji)` - first plant in garden bed |
| **Last Element** | `last(xs, default)` | `lastHarvest(harvests, defaultCrop)` - last crop harvested |
| **Running Sum** | `running_sum([1,2,3])` | `cumulativeMoonlight(exposures)` - total light received each night |
| **Mentions** | `@alice @bob` | Keep as-is (generic social feature) or `#moonflower #starfern` tags |
| **Base Route** | `/api/v1/actions` | `/greenhouse/bed1/crop` - garden plot paths |

### EmojiPaint Extend
Already close - just swap emojis to plant ones: 🌱 🌻 🥕 🌽 🍅

---

## Recommendations

### 1. Keep & Enhance (Strong probe fit)
- **Mentions** - Good API exploration task, maybe theme lightly
- **Running Sum** - Great accumulator visibility, theme as "cumulative moonlight" or "harvest totals"
- **EmojiPaint Extend** - Good extension task, swap to plant emojis

### 2. Theme but Keep Simple (Educational warmups)
- **Clamp** → `clampMoisture` - Simple, teaches syntax
- **Safe Head** → `firstPlant` - Teaches pattern matching

### 3. Consider Dropping or Merging
- **Last Element** - Similar to Safe Head, not much probe benefit
- **Base Route** - Similar to Mentions

### 4. Potential New Tasks to Add

**A) "Harvest Calculator"** (exercises accumulator + ADTs)
```hazel
# Calculate total value of harvests with quality bonuses
type Quality = Bronze | Silver | Gold in
type Harvest = (crop = String, quality = Quality, qty = Int) in

let totalValue: [Harvest] -> Int = fun harvests ->
  fold_left(harvests, fun (acc, h) -> acc + harvestValue(h), 0)
in
```
- Shows accumulator evolution
- ADT pattern matching visible in probes
- Fits Night Garden theme perfectly

**B) "Growth Schedule"** (exercises pipeline + filtering)
```hazel
# Filter plants that need watering based on moon phase
let needsWatering: (MoonPhase, [Plant]) -> [Plant] = fun (phase, plants) ->
  filter(plants, fun p -> isCompatible(p.affinity, phase))
in
```
- Pipeline (filter → map)
- Ties into other Night Garden tasks

---

## Summary

**Current tasks that align well with RQs:** Mentions, Running Sum, EmojiPaint Extend

**Tasks that could use theming:** Clamp, Safe Head (simple warmups)

**Tasks that are redundant:** Last Element (merge with Safe Head?), Base Route (similar to Mentions)

**Gap:** No task currently exercises ADT transformations in a writing context (unlike the debugging tasks). A "Harvest Calculator" type task would fill this.

---

## File Locations

Writing tasks are in: `hazel-programs/study/writing/`

- `mentions/` - Mention extractor (split → filter → map)
- `basepoint/` - URL base route extraction
- `clamp/` - Simple numeric clamping
- `running-sum/` - Cumulative sums with fold
- `last-element/` - Get last list element
- `safe-head/` - Safe list head with default
- `emojipaint-extend/` - Add PaintCol to existing app
