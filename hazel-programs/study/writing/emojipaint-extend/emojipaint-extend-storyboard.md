# EmojiPaint Extension Storyboard

## Task Overview
- **Category**: Modification (extend existing program)
- **Domain**: Creative app / pixel art
- **Base program**: ~75 lines of working MVU code
- **Changes needed**: ~10 lines (type, helper, case)
- **Error patterns**: Row/column confusion, copy-paste errors

## Setup
The user receives a working emojipaint app with:
- `PaintRow(Row)` action that fills a row
- `setRow` helper function

They need to add:
- `PaintCol(Col)` action
- `setCol` helper function
- Handler in `update`

## Probe Benefits for Modification Tasks

1. **Understanding existing code**: Probes show how `setRow` transforms the canvas
2. **Verifying new code**: See `setCol` output immediately
3. **Catching row/column bugs**: Wrong iteration shows visibly wrong grid

## CLI Development Session

### Step 1: Read and understand setRow

**Existing code:**
```hazel
let setRow: (Canvas, Row, Emoji) -> Canvas =
  fun canvas, targetRow, emoji ->
    mapi(canvas, fun (i, row) ->
      if i == targetRow
      then map(row, fun _ -> emoji)
      else row)
```

**Probe output (when PaintRow(1) is called):**
```
let setRow: ... =
  fun ⟦canvas, targetRow, emoji⟧ ->     ≡ ([["", "", ""], ...], 1, "🎨")
    ⟦mapi(canvas, fun (⟦i, row⟧) ->     ≡ (0, ["", "", ""])
      if ⟦i == targetRow⟧     ≡ false
      then ⟦map(row, fun _ -> emoji)⟧     ≡ ["🎨", "🎨", "🎨"]
      else row)⟧     ≡ [["", "", ""], ["🎨", "🎨", "🎨"], ...]
```

**Insight**:
- `i` is the row index (0, 1, 2)
- When `i == targetRow`, replace the whole row
- Otherwise keep the row unchanged

### Step 2: Write setCol (first attempt - WRONG)

**User copies setRow and modifies:**
```hazel
let setCol: (Canvas, Col, Emoji) -> Canvas =
  fun canvas, targetCol, emoji ->
    mapi(canvas, fun (i, row) ->
      if i == targetCol        # BUG: i is row index, not column!
      then map(row, fun _ -> emoji)
      else row)
```

**Probe output:**
```
  ⟦setCol(init, 0, "X")⟧     ≡ [["X", "X", "X"], ["", "", ""], ["", "", ""]]
```

**Bug revealed**: Filled **row** 0, not **column** 0. Expected `[["X","",""], ["X","",""], ["X","",""]]`.

### Step 3: Fix setCol

**Correct implementation:**
```hazel
let setCol: (Canvas, Col, Emoji) -> Canvas =
  fun canvas, targetCol, emoji ->
    map(canvas, fun row ->           # Iterate over each row
      mapi(row, fun (j, cell) ->     # Within row, check column index
        if j == targetCol then emoji else cell))
```

**Probe output:**
```
  ⟦map(canvas, fun ⟦row⟧ ->     ≡ ["", "", ""]
    mapi(row, fun (⟦j, cell⟧) ->     ≡ (0, "")
      if j == targetCol then emoji else cell))⟧     ≡ [["🎨", "", ""], ["🎨", "", ""], ...]
```

**Insight**: Now iterating correctly - `j` is column index within each row.

### Step 4: Add to update function

**User adds case:**
```hazel
| PaintCol(col) =>
    updateGrid(m, fun c -> setCol(c, col, m.brush))
```

**Probe shows the case being matched:**
```
    | ⟦PaintCol(col)⟧ =>     ≡ PaintCol(1)
        ⟦updateGrid(m, fun c -> setCol(c, col, m.brush))⟧     ≡ (canvas=[["", "💜", ""], ...
```

## Common Mistake Paths

### Mistake A: Copy-paste setRow without changing iteration

**User writes:**
```hazel
let setCol = fun canvas, targetCol, emoji ->
    mapi(canvas, fun (i, row) ->
      if i == targetCol  # Wrong! i is row index
      then map(row, fun _ -> emoji)
      else row)
```

**Probe shows:** Fills a row instead of column. `setCol(canvas, 0, "X")` produces `[["X","X","X"], ...]`.

**How probe helps:** Immediately see wrong output shape - horizontal line instead of vertical.

### Mistake B: Wrong nesting of map/mapi

**User writes:**
```hazel
let setCol = fun canvas, targetCol, emoji ->
    mapi(canvas, fun (i, row) ->
      if i == targetCol then emoji else row)  # Wrong: replaces row with single emoji
```

**Probe shows:** Type error or wrong structure - row becomes a string instead of list.

### Mistake C: Forgetting to add case to update

**User adds type and helper but forgets update case:**

**Result:** Pattern match error when PaintCol action is dispatched.

### Mistake D: Using wrong variable in setCol

**User writes:**
```hazel
mapi(row, fun (j, cell) ->
  if i == targetCol then emoji else cell)  # Using i instead of j
```

**Probe shows:** Depends on outer `i` - inconsistent behavior across rows.

## Key Probe Benefits for Modification

1. **Learning from existing code**: See how setRow works before writing setCol
2. **Visual grid debugging**: Canvas output shows exactly what changed
3. **Row vs column confusion**: Horizontal vs vertical fill is immediately visible
4. **Incremental verification**: Test setCol in isolation before integrating

## Domain Appeal

- **Fun theme**: Emoji pixel art is playful
- **Real pattern**: MVU/Elm architecture is industry-relevant
- **Visual feedback**: Grid transformations are easy to verify visually
- **Building on existing code**: Realistic modification task
