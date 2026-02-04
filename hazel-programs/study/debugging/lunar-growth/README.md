# Lunar Growth Tracker

A mystical greenhouse simulation where plants grow based on lunar energy. Each plant kind responds differently to moon phases.

## Concept

You run a magical night garden where plants absorb lunar energy to grow. Different plants have different affinities:

- **Moonbloom**: Thrives during New moon (darkness), struggles during Full moon
- **Starfern**: Thrives during Full moon (brightness), struggles during New moon
- **Duskrose**: Loves transition phases (Waxing and Waning)
- **Evergreen**: Steady grower, unaffected by moon phases

### Growth Rules

- **Perfect match** (plant affinity equals current phase): +20 growth
- **Opposite phase** (New/Full or Waxing/Waning are opposites): +5 growth (penalty)
- **Neutral phase**: +10 growth
- **Evergreen**: Always +10 growth regardless of phase

Plants are ready to harvest at 100 growth.

## The Bug

### Location
In the `growthBonus` function, line ~113 in the bug files.

### What's Wrong
The bug is in the `isOpposite` check:
```
else if isOpposite(currentPhase, currentPhase)
```

This should be:
```
else if isOpposite(plant.affinity, currentPhase)
```

### Why It's Wrong
The buggy code compares the current moon phase against itself (`isOpposite(currentPhase, currentPhase)`), which will always return `false` because no phase is opposite to itself.

The correct code should compare the plant's affinity against the current phase (`isOpposite(plant.affinity, currentPhase)`) to determine if the plant is in unfavorable conditions.

### Effect
- Plants never receive the -5 penalty for being in their opposite phase
- A Moonbloom during Full moon grows at +10 (neutral) instead of +5 (penalty)
- A Starfern during New moon grows at +10 instead of +5

### Failing Test
The test "Moonbloom in opposite phase grows slowly over multiple nights" plants a Moonbloom and runs 4 Full moon nights:
- **Expected**: 4 × 5 = 20 growth (with opposite penalty)
- **Actual (buggy)**: 4 × 10 = 40 growth (no penalty applied)

## Files

- `lunar-growth.hz` - Working version with all tests passing
- `lunar-growth-bug.hz` - Buggy version with minimal comments (for unscaffolded debugging)
- `lunar-growth-bug-scaffold.hz` - Buggy version with explanatory comments (for scaffolded debugging)

## Why Probes Help

This bug is subtle because:
1. The code type-checks correctly
2. The function names and structure look reasonable
3. The bug only affects a specific branch (opposite phase penalty)

Using probes, a debugger can:
1. Add `^^probe(isOpposite(currentPhase, currentPhase))` to see it always returns `false`
2. Add `^^probe(plant.affinity)` and `^^probe(currentPhase)` to see they differ
3. Trace the growth values through `^^probe(bonus)` to see unexpected values

The probe output would reveal that `isOpposite` is being called with the same argument twice, making the opposite-phase penalty branch unreachable.

## Running Tests

```bash
# Check for type errors
./hazel analyze lunar-growth-bug.hz

# Run and see failing tests
./hazel test lunar-growth-bug.hz

# Debug with probes
./hazel probe lunar-growth-bug.hz
./hazel probe --many lunar-growth-bug.hz
```
