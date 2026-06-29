# Crop Plotter with Soil Types

A field planner where each cell has BOTH a crop AND a soil type. This adds richer structured data for probes to display.

## Concept

You manage a small farm where each cell in the field tracks two properties:
- **crop**: What's planted (emoji like "🌱", "🌻", "🥕", or "" for empty)
- **soil**: The soil type (Loamy, Sandy, Clay, or Rich)

Operations preserve the property they don't modify:
- Planting a crop keeps the existing soil type
- Tilling soil keeps the existing crop

### Data Model

```hazel
type Cell = (
  crop = Crop,     # Plant emoji or "" #
  soil = SoilType  # Loamy | Sandy | Clay | Rich #
) in

type Field = [[Cell]] in
```

## The Bug

### Location
In the `plantCrop` function, line ~68 in the bug files.

### What's Wrong
The bug is in the column check inside the inner `mapi`:
```hazel
if i == col
then (crop = seed, soil = cell.soil)
```

This should be:
```hazel
if j == col
then (crop = seed, soil = cell.soil)
```

### Why It's Wrong
The buggy code uses `i` (the row index from the outer mapi) instead of `j` (the column index from the inner mapi) to check the column position.

This means the crop is planted when `row == col` (on the diagonal), regardless of the actual target column.

### Effect
- Planting at (0, 0), (1, 1), or (2, 2) works correctly (diagonal positions where row == col)
- Planting at off-diagonal positions like (0, 1), (0, 2), (1, 0), etc. fails - the crop is planted in the wrong place or not at all

### Failing Tests
Two tests fail:
1. "plant with different seed" - plants at (0, 1), which is off-diagonal
2. "plant off-diagonal" - plants at (0, 2), explicitly testing off-diagonal

### Why Other Tests Pass
Many tests use diagonal positions like (0, 0), (1, 1), (2, 2) where `i == col` happens to be true, so they pass coincidentally.

## Files

- `soil-plotter.hz` - Working version with all tests passing
- `soil-plotter-bug.hz` - Buggy version with minimal comments (for unscaffolded debugging)
- `soil-plotter-bug-scaffold.hz` - Buggy version with explanatory comments (for scaffolded debugging)

## Why Probes Help

This bug is subtle because:
1. The code type-checks correctly
2. Many tests pass (diagonal positions work)
3. The variable names `i` and `j` look similar

Using probes, a debugger can:
1. Add `^^probe(i)` and `^^probe(j)` inside the inner mapi to see their values
2. Add `^^probe(i == col)` to see when the condition is true
3. Add `^^probe(cell)` to see which cell is being modified
4. The rich record output `(crop = "🌱", soil = Loamy)` makes it clear which cells are being updated

The probe output shows records with both crop and soil fields, making it easier to understand the state compared to simpler grid types.

## Debugging Strategy

1. Run `./hazel test soil-plotter-bug.hz` to see which tests fail
2. Notice the pattern: diagonal positions pass, off-diagonal fail
3. Add probes in `plantCrop`:
   ```hazel
   let plantCrop: (Field, Row, Col, Crop) -> Field =
     fun field, row, col, seed ->
       mapi(field, fun (i, r) ->
         if i == row
         then mapi(r, fun (j, cell) ->
           if ^^probe(i == col)  # Should this use j? #
           then (crop = seed, soil = cell.soil)
           else cell)
         else r)
   in
   ```
4. Run `./hazel probe --many soil-plotter-bug.hz` to see that `i == col` is only true on the diagonal

## Running Tests

```bash
# Check for type errors
./hazel analyze soil-plotter-bug.hz

# Run and see failing tests
./hazel test soil-plotter-bug.hz

# Debug with probes
./hazel probe soil-plotter-bug.hz
./hazel probe --many soil-plotter-bug.hz
```
