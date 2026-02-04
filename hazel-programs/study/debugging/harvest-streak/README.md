# Harvest Streak Tracker

A harvest ledger system for a night garden themed farming game. Tracks quality streaks where consecutive harvests of the same quality build up bonus multipliers.

## Program Overview

**Lines:** ~260
**Tests:** 16
**Concepts:** State management, streak/combo detection, temporal ordering bugs, comparing with stale vs fresh values

### Domain

A harvest festival under the stars. Farmers record harvests of magical night crops (Moonmelon, Starfruit, Nightberry, Duskwheat) at different quality tiers (Bronze, Silver, Gold, Starlight). Consecutive harvests of the same quality build a streak bonus.

### Data Model

```
Quality = Bronze | Silver | Gold | Starlight
Crop = Moonmelon | Starfruit | Nightberry | Duskwheat

Harvest = { crop: Crop, quality: Quality, quantity: Int }

Ledger = {
  harvests: [Harvest],     # All recorded harvests
  totalValue: Int,         # Cumulative value
  streakBonus: Int,        # Current unclaimed bonus
  lastQuality: Quality     # For streak comparison
}

Action = RecordHarvest(Harvest) | ClaimBonus | CloseDay
```

### Value Calculation

```
harvestValue = cropValue * qualityMultiplier * quantity

Crop values: Duskwheat=10, Moonmelon=15, Starfruit=20, Nightberry=25
Quality multipliers: Bronze=1, Silver=2, Gold=3, Starlight=5
```

### Streak Mechanics

When recording a harvest:
1. Compare current harvest's quality with `ledger.lastQuality`
2. If they match (and not the first harvest), streak continues: `streakBonus += 5`
3. If they differ, streak resets: `streakBonus = 0`
4. Update `lastQuality` to current harvest's quality

### Key Functions

- `harvestValue` - Calculates value of a single harvest
- `processHarvest` - Records harvest, handles streak logic
- `claimBonus` - Collects accumulated streak bonus
- `closeDay` - Resets streak tracking for new day

---

## Bug: Premature Value Update

**Difficulty:** Medium

### What's Wrong

The bug is in `processHarvest`. The code updates `lastQuality` BEFORE checking if the streak continues, instead of after. This means `h.quality == newLast` always compares the current quality with itself (which is always true for non-first harvests).

```hazel
# BUGGY version:
let processHarvest = fun (ledger, h) ->
  let value = harvestValue(h) in
  let isFirst = length(ledger.harvests) == 0 in
  let newLast = h.quality in                         # Updated FIRST
  let continues = !isFirst && h.quality == newLast in # Compares with new value!
  ...

# CORRECT version:
let processHarvest = fun (ledger, h) ->
  let value = harvestValue(h) in
  let isFirst = length(ledger.harvests) == 0 in
  let continues = !isFirst && h.quality == ledger.lastQuality in  # Compare with OLD
  let newLast = h.quality in                                       # Update AFTER
  ...
```

### The Effect

Since `h.quality == newLast` is always `h.quality == h.quality` = `true`, the streak never resets when quality changes. Every non-first harvest continues the streak.

### Failing Tests

1. **"different quality resets streak"** - Record Bronze, then Gold. Expected streakBonus=0, but bug gives 5 (streak continued).

2. **"streak resets then can build again"** - Record Bronze, Gold, Gold. Expected streakBonus=5 (reset, then one continuation). Bug gives 10 (never reset, two continuations).

### Debugging with Probes

Place a probe on the comparison:

```hazel
let continues = !isFirst && ^^probe(h.quality == newLast) in
```

With `./hazel probe --many`, you'll see:
- Bug: `true` for every non-first harvest
- Expected: `false` when quality changes

Or probe both values being compared:

```hazel
let continues = !isFirst && ^^probe(h.quality) == ^^probe(newLast) in
```

You'll see they're always identical, revealing the bug.

### Why This Bug Is Realistic

This is a common "order of operations" bug pattern:
- You need to compare with the old value before updating
- It's tempting to prepare all the new values first, then use them
- The bug compiles and typechecks perfectly
- Many tests pass (basic value calculation, streak building with same quality)
- Only tests involving quality CHANGES fail

### Fix

Move the streak comparison BEFORE updating `newLast`:

```hazel
let continues = !isFirst && h.quality == ledger.lastQuality in
let newLast = h.quality in
```

Or equivalently, compare with `ledger.lastQuality` directly instead of `newLast`.

---

## Files

- `harvest-streak.hz` - Working version, all tests pass
- `harvest-streak-bug.hz` - Bug planted, minimal comments
- `harvest-streak-bug-scaffold.hz` - Bug planted, detailed comments explaining each function

---

## Development Notes

### Hazel-Specific Considerations

1. **First harvest edge case:** The initial ledger has `lastQuality = Bronze`. Without special handling, a first Bronze harvest would incorrectly continue a "streak". We use `length(ledger.harvests) == 0` to detect the first harvest.

2. **Boolean operators:** Hazel uses `!` for negation, `&&` for AND, `||` for OR.

3. **Record updates:** Hazel doesn't have record update syntax. We must construct a complete new record each time.

### Test Coverage

- Basic value calculations (crop values, quality multipliers, quantities)
- Streak building (same quality continues)
- Streak breaking (different quality resets)
- Streak rebuilding (after reset, can build again)
- Bonus claiming and day closing
- Edge cases (first harvest, complex scenarios)
