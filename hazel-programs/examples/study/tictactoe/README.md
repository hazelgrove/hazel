# Tic-Tac-Toe

Classic 3x3 game with win detection for rows, columns, and diagonals.

## Program Overview

**Lines:** ~265
**Tests:** 18
**Concepts:** ADTs, pattern matching, list operations, game state management

### Data Model

```
Cell = Empty | X | O
Board = [Cell]  (9 cells, row-major order)
Player = PlayerX | PlayerO
GameStatus = InProgress | Won(Player) | Draw
Model = (board, turn, status)
```

### Key Functions

- `checkWinner` - Checks all 8 winning lines (3 rows, 3 cols, 2 diagonals)
- `move` - Validates and applies a move, updates game status
- `play` - Applies a sequence of moves

### Board Layout

```
0 | 1 | 2
---------
3 | 4 | 5
---------
6 | 7 | 8
```

Diagonals:
- Main: 0, 4, 8
- Anti: 2, 4, 6

---

## Bug Variants

### tictactoe-bug-diagonal.hz

**Bug:** Anti-diagonal check uses wrong indices (2, 5, 6) instead of (2, 4, 6)

**Difficulty:** Easy

**What's wrong:**
```hazel
# Buggy:
let diag2 = threeInRow(getCells3(board, 2, 5, 6)) in

# Correct:
let diag2 = threeInRow(getCells3(board, 2, 4, 6)) in
```

**Failing test:** "X wins with anti-diagonal" - X plays positions 2, 4, 6 but win isn't detected

**Probe strategy:**
- Probe `getCells3(board, 2, 4, 6)` vs `getCells3(board, 2, 5, 6)` to see which cells are actually being checked
- Probe `diag2` to see it returns `None` when it should return `Some(X)`

**Why this bug is realistic:** Off-by-one errors in grid indices are common. The anti-diagonal (2, 4, 6) is less intuitive than the main diagonal (0, 4, 8).

---

### tictactoe-bug-turn.hz

**Bug:** `nextPlayer` returns the same player instead of switching

**Difficulty:** Easy

**What's wrong:**
```hazel
# Buggy:
let nextPlayer = fun p ->
  case p
  | PlayerX => PlayerX  # Should be PlayerO
  | PlayerO => PlayerO  # Should be PlayerX
  end

# Correct:
let nextPlayer = fun p ->
  case p
  | PlayerX => PlayerO
  | PlayerO => PlayerX
  end
```

**Failing test:** "O places O marker" - After X plays, O's move still places X

**Probe strategy:**
- Probe `m.turn` after each move to see it never changes
- Probe `playerCell(m.turn)` to see it's always X

**Why this bug is realistic:** Copy-paste error when writing the two cases, forgetting to change the return values.

---

## Development Notes

### Indeterminate Test Issue

During development, some tests returned "indeterminate" instead of pass/fail when using direct ADT equality:

```hazel
# This was indeterminate:
m.status == Draw

# This worked:
case m.status | Draw => true | _ => false end
```

Also, comparing full models (`m == init`) was indeterminate. Changed to comparing specific fields.

**Takeaway for study:** If participants write tests with ADT equality and get indeterminate results, this could be confusing. The bug variants use simpler comparisons.

### Test Coverage

The working version tests:
- Basic operations (move, turn switching)
- All win conditions (3 rows, 3 columns, 2 diagonals)
- Draw detection
- Invalid move rejection (occupied cell, out of bounds, game over)
- Both players winning
