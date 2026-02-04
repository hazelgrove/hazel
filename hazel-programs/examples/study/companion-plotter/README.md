# Companion Planting Grid

A night garden simulation where plants sense their neighbors under the moonlight. Some crops are companions that help each other thrive; others are rivals that struggle side by side.

## Concept

In this moon garden, you plant crops on a grid. Each crop has a health value (0-100) that's affected by its orthogonal neighbors (above, below, left, right). When you calculate health, each plant checks its neighbors and applies companion effects.

### Crop Interactions

| Crop 1 | Crop 2 | Effect | Modifier |
|--------|--------|--------|----------|
| Tomato | Basil | Beneficial | +10 |
| Basil | Tomato | Beneficial | +10 |
| Carrot | Corn | Beneficial | +10 |
| Corn | Carrot | Beneficial | +10 |
| Tomato | Carrot | Harmful | -10 |
| Carrot | Tomato | Harmful | -10 |
| Any | Same | Neutral | 0 |
| Any | Other | Neutral | 0 |

### Health Calculation

- Base health: 50
- Each neighbor applies its companion effect modifier
- Multiple neighbors stack (e.g., tomato with 2 basil neighbors: 50 + 10 + 10 = 70)
- Health is clamped to 0-100

## The Bug

### Location
In the `neighborModifier` function, around line 125 in the bug files.

### What's Wrong
```hazel
let effects = map(neighbors, fun neighbor ->
  companionEffect(cell.crop, cell.crop)    # BUG: compares crop with itself
) in
```

Should be:
```hazel
let effects = map(neighbors, fun neighbor ->
  companionEffect(cell.crop, neighbor.crop)  # CORRECT: compares with neighbor
) in
```

### Why It's Wrong
The buggy code compares `cell.crop` with `cell.crop` (itself) instead of comparing `cell.crop` with `neighbor.crop`. Since a crop is always `Neutral` with itself (by the companion rules), no companion effects ever apply.

### Effect
- Plants never gain health from companions (Beneficial effect never triggers)
- Plants never lose health from rivals (Harmful effect never triggers)
- All plants stay at base health (50) regardless of their neighbors
- The `neighborModifier` function always returns 0

### Failing Tests
4 tests fail because they expect companion effects to change health:

1. **"tomato next to basil gains health"** - Expected: >50, Actual: 50
2. **"basil next to tomato gains health"** - Expected: >50, Actual: 50
3. **"tomato next to carrot loses health"** - Expected: <50, Actual: 50
4. **"multiple companions stack benefits"** - Expected: 70, Actual: 50

## Files

- `companion-plotter.hz` - Working version with all tests passing
- `companion-plotter-bug.hz` - Buggy version with minimal comments (unscaffolded)
- `companion-plotter-bug-scaffold.hz` - Buggy version with explanatory comments (scaffolded)

## Debugging with Probes

This bug is subtle because:
1. The code type-checks correctly
2. The function structure looks reasonable
3. The `neighbor` variable is in scope but unused
4. Tests for basic planting, harvesting, and companion rules pass

### Probe Strategy

**Step 1: Check the effect calculation**
Add a probe in `neighborModifier` to see what effect is computed:
```hazel
let effects = map(neighbors, fun neighbor ->
  ^^probe(companionEffect(cell.crop, cell.crop))
) in
```
This will always show `Neutral` - suspicious!

**Step 2: Inspect the arguments**
Add probes to see what's being compared:
```hazel
let effects = map(neighbors, fun neighbor ->
  let crop1 = ^^probe(cell.crop) in
  let crop2 = ^^probe(cell.crop) in  # Wait, both are cell.crop!
  companionEffect(crop1, crop2)
) in
```
The probes reveal that `crop2` should be `neighbor.crop`.

**Step 3: Verify the neighbor data**
Check that neighbors are being retrieved correctly:
```hazel
let neighbors = ^^probe(getNeighborCells(field, row, col)) in
```
This shows the neighbors have different crops - confirming the issue is in the comparison, not the retrieval.

### Expected Probe Output
With a tomato at (0,0) and basil at (0,1), using `./hazel probe --many`:

**Buggy version:**
```
companionEffect(cell.crop, cell.crop) => Neutral
```

**Fixed version:**
```
companionEffect(cell.crop, neighbor.crop) => Beneficial
```

## Running Tests

```bash
# Check for type errors (all files pass)
./hazel analyze companion-plotter-bug.hz

# Run tests (4 will fail in bug versions)
./hazel test companion-plotter-bug.hz

# Debug with probes
./hazel probe companion-plotter-bug.hz
./hazel probe --many companion-plotter-bug.hz
```

## Why This Bug is Interesting

This bug demonstrates a common programming mistake: having the right variable in scope but using the wrong one. The code:

1. Correctly retrieves neighbors
2. Correctly iterates over neighbors
3. Has `neighbor` in scope inside the map callback
4. Uses `cell.crop` (twice) instead of `cell.crop, neighbor.crop`

The fact that `companionEffect(crop, crop)` returns `Neutral` (self has no effect) means the code "works" - it just never applies any effects. This makes the bug particularly insidious: the code doesn't crash, it just silently does nothing.
