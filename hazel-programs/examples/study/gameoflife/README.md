# Conway's Game of Life

Cellular automaton simulation with birth/death rules based on neighbor counts.

## Program Overview

**Lines:** ~230
**Tests:** 20
**Concepts:** Grid manipulation, neighbor counting, simultaneous update, classic patterns

### Data Model

```
Cell = Dead | Alive
Grid = (cells: [Cell], width: Int, height: Int)
```

The grid is stored as a flat list with width/height metadata. Coordinates are (x, y) where x is column and y is row.

### Rules

1. **Underpopulation:** Alive cell with < 2 neighbors dies
2. **Survival:** Alive cell with 2-3 neighbors lives
3. **Overpopulation:** Alive cell with > 3 neighbors dies
4. **Birth:** Dead cell with exactly 3 neighbors becomes alive

### Key Functions

- `countNeighbors(grid, x, y)` - Counts the 8 surrounding cells
- `nextCellState(current, neighbors)` - Applies the rules
- `step(grid)` - Updates entire grid simultaneously

### Classic Patterns Tested

- **Blinker:** 3 horizontal cells oscillate to vertical and back
- **Block:** 2x2 square is stable (still life)
- **Lone cell:** Dies from underpopulation

---

## Bug Variants

### gameoflife-bug-neighbor.hz

**Bug:** Only counts 4 orthogonal neighbors instead of all 8

**Difficulty:** Easy

**What's wrong:**
```hazel
# Buggy - missing diagonal neighbors:
let neighbors = [
  getCell(g, x,     y - 1),  # up
  getCell(g, x - 1, y),      # left
  getCell(g, x + 1, y),      # right
  getCell(g, x,     y + 1)   # down
] in

# Correct - all 8 neighbors:
let neighbors = [
  getCell(g, x - 1, y - 1), getCell(g, x, y - 1), getCell(g, x + 1, y - 1),
  getCell(g, x - 1, y),                           getCell(g, x + 1, y),
  getCell(g, x - 1, y + 1), getCell(g, x, y + 1), getCell(g, x + 1, y + 1)
] in
```

**Failing test:** "cell with 8 neighbors" - Should count 8, but only counts 4

**Probe strategy:**
- Probe `neighbors` list to see only 4 elements
- Probe `countNeighbors(g, 1, 1)` for a surrounded cell

**Why this bug is realistic:** When implementing neighbor counting, it's easy to start with the obvious 4 directions and forget the diagonals.

---

### gameoflife-bug-survival.hz

**Bug:** Survival threshold is 2-4 instead of 2-3 (cell survives with 4 neighbors)

**Difficulty:** Medium

**What's wrong:**
```hazel
# Buggy:
if neighbors >= 2 && neighbors <= 4 then Alive else Dead

# Correct:
if neighbors == 2 || neighbors == 3 then Alive else Dead
```

**Failing test:** "alive cell with 4 neighbors dies (overpopulation)" - Cell survives when it should die

**Probe strategy:**
- Probe `nextCellState(Alive, 4)` to see it returns `Alive`
- Probe the condition evaluation

**Why this bug is realistic:** The rule "2 or 3 neighbors" could be misremembered as "2 to 4 neighbors" or implemented with wrong comparison operators.

---

### gameoflife-bug-sequential.hz

**Bug:** Updates cells sequentially instead of simultaneously

**Difficulty:** Hard

**What's wrong:**
```hazel
# Buggy - sequential update (each cell sees previous updates):
let step = fun g ->
  fold_left(indices, fun (currentGrid, idx) ->
    let neighbors = countNeighbors(currentGrid, x, y) in  # Uses currentGrid!
    ...
  , g)

# Correct - simultaneous update (all cells see original state):
let step = fun g ->
  let newCells = mapi(g.cells, fun (idx, _) ->
    let neighbors = countNeighbors(g, x, y) in  # Uses original g!
    ...
  ) in
  ...
```

**Failing test:** "updates are simultaneous not sequential" - Blinker pattern evolves incorrectly

**Probe strategy:**
- This is harder to debug because the bug is architectural
- Probe the grid state at different points during `step`
- Compare intermediate states vs expected simultaneous behavior
- Need to understand that cells should all "see" the same original grid

**Why this bug is realistic:**
- The sequential version seems natural if you think of "going through each cell and updating it"
- The correct simultaneous version requires understanding that the new state depends only on the old state
- This is a conceptual bug, not a typo

**Multi-location aspect:** To truly understand this bug, you need to:
1. Understand the `step` function structure
2. Recognize that `currentGrid` vs `g` makes a difference
3. Know that Game of Life requires simultaneous updates

---

## Development Notes

### Smoothest Development

This was the easiest of the three programs to develop. The logic is well-defined, the tests are straightforward, and there were no surprises during implementation.

### Test Coverage

The working version tests:
- Grid operations (create, get, set, bounds checking)
- Neighbor counting (isolated cell, partial neighbors, full 8 neighbors, corners)
- All four rules (underpopulation, survival with 2, survival with 3, overpopulation, birth)
- Classic patterns (blinker oscillation, block stability)
- Edge behavior
- Simultaneous update verification

### Coordinate System

Using (x, y) where x is column and y is row. Index formula: `y * width + x`

The reverse (getting x, y from index) uses:
```hazel
let x = idx - (idx / g.width) * g.width in  # idx % width
let y = idx / g.width in
```
