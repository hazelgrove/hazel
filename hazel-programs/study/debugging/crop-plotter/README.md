# Grove Plotter

A garden planning tool built as a Model-View-Update (MVU) application. Users can plant seeds on a grid, select different plants, and manage their moonlit grove.

## Concept

The Grove Plotter simulates a simple garden grid where you can:
- Plant seeds at specific positions
- Select different plants from your inventory
- Fill entire rows with plants
- Uproot (clear) individual cells or the whole grove

## Files

- `grove-plotter.hz` - Working version (all tests pass)
- `grove-plotter-bug.hz` - Bug version, minimal comments
- `grove-plotter-bug-scaffold.hz` - Bug version with helpful comments

## The Bug

The bug is in the `setCell` function. It uses the row index `i` when checking the column position, instead of the column index `j`:

```hazel
# Buggy code #
let setCell: (Grove, Row, Col, Plant) -> Grove =
  fun grove, row, col, plant ->
    mapi(grove, fun (i, r) ->
      if i == row
      then mapi(r, fun (j, c) ->
        if i == col    # BUG: should be j == col
        then plant
        else c)
      else r)
in
```

This causes plants to be placed in the wrong column position. When row == col (like positions (0,0), (1,1), (2,2)), the bug happens to produce the correct result. But when row != col, the plant appears in the wrong position or not at all.

## Why Probes Help

Without probes, the symptom is that some tests fail with incorrect grid states. With probes, you can observe:

1. **The outer mapi indices** - see that `i` correctly identifies rows
2. **The inner mapi behavior** - see that `j` should identify columns but the condition uses `i`
3. **When the bug manifests** - the condition `i == col` only matches when row == col

Try adding a probe to see the issue:
```hazel
let setCell: (Grove, Row, Col, Plant) -> Grove =
  fun grove, row, col, plant ->
    mapi(grove, fun (i, r) ->
      if i == row
      then mapi(r, fun (j, c) ->
        if ^^probe(i == col)   # Probe shows this is checking the wrong variable
        then plant
        else c)
      else r)
in
```

Running `./hazel probe --many grove-plotter-bug.hz` will show that the inner condition only evaluates true when the row index happens to equal the target column, revealing the variable confusion.
