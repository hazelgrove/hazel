# Crop Plotter

A garden planning tool built as a Model-View-Update (MVU) application. Users can plant crops on a grid, select different seeds, and manage their virtual garden plot.

## Concept

The Crop Plotter simulates a simple garden grid where you can:
- Plant crops at specific positions
- Select different seeds from your inventory
- Fill entire rows with crops
- Harvest (clear) individual cells or the whole field

## Files

- `crop-plotter.hz` - Working version (all tests pass)
- `crop-plotter-bug.hz` - Bug version, minimal comments
- `crop-plotter-bug-scaffold.hz` - Bug version with helpful comments

## The Bug

The bug is in the `setCell` function. It uses the row index `i` when checking the column position, instead of the column index `j`:

```hazel
# Buggy code #
let setCell: (Field, Row, Col, Crop) -> Field =
  fun field, row, col, crop ->
    mapi(field, fun (i, r) ->
      if i == row
      then mapi(r, fun (j, c) ->
        if i == col    # BUG: should be j == col
        then crop
        else c)
      else r)
in
```

This causes crops to be planted in the wrong column position. When row == col (like positions (0,0), (1,1), (2,2)), the bug happens to produce the correct result. But when row != col, the crop appears in the wrong position or not at all.

## Why Probes Help

Without probes, the symptom is that some tests fail with incorrect grid states. With probes, you can observe:

1. **The outer mapi indices** - see that `i` correctly identifies rows
2. **The inner mapi behavior** - see that `j` should identify columns but the condition uses `i`
3. **When the bug manifests** - the condition `i == col` only matches when row == col

Try adding a probe to see the issue:
```hazel
let setCell: (Field, Row, Col, Crop) -> Field =
  fun field, row, col, crop ->
    mapi(field, fun (i, r) ->
      if i == row
      then mapi(r, fun (j, c) ->
        if ^^probe(i == col)   # Probe shows this is checking the wrong variable
        then crop
        else c)
      else r)
in
```

Running `./hazel probe --many crop-plotter-bug.hz` will show that the inner condition only evaluates true when the row index happens to equal the target column, revealing the variable confusion.
