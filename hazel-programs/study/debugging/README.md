# Study Programs

Programs for user studies on Hazel's probe debugging mechanism.

Each program has:
- `program.hz` - Working version with comprehensive tests
- `program-bug-*.hz` - Buggy versions with different bugs

## Programs

| Program | Lines | Description |
|---------|-------|-------------|
| emojipaint | ~150 | MVU paint app with emoji brush, grid operations |
| tamagotchi/ | ~505 | Haunted toaster virtual pet with stats, decay, evolution |
| tictactoe/ | ~265 | Classic 3x3 game with win/draw detection |
| gameoflife/ | ~230 | Conway's cellular automaton with birth/death rules |
| calculator/ | ~270 | Expression parser with operator precedence |

## Tamagotchi Bug Variants

| File | Bug | Difficulty | Probe Strategy |
|------|-----|------------|----------------|
| tamagotchi-bug-decay.hz | Wrong stat checked in decay | Easy | Probe happinessDecay calculation |
| tamagotchi-bug-priority.hz | `neglectScore > 0` instead of `> careScore` | Medium | Probe evolution scores at boundary |
| tamagotchi-bug-bonus.hz | Sleep uses `- bonus` instead of `+ bonus` | Medium | Compare energy across personalities |

## Tic-Tac-Toe Bug Variants

| File | Bug | Difficulty | Probe Strategy |
|------|-----|------------|----------------|
| tictactoe-bug-diagonal.hz | Wrong anti-diagonal indices (2,5,6 instead of 2,4,6) | Easy | Probe getCells3 for diagonal check |
| tictactoe-bug-turn.hz | nextPlayer returns same player | Easy | Probe turn field after each move |

## Game of Life Bug Variants

| File | Bug | Difficulty | Probe Strategy |
|------|-----|------------|----------------|
| gameoflife-bug-neighbor.hz | Only counts orthogonal neighbors (4 not 8) | Easy | Probe countNeighbors for center cell |
| gameoflife-bug-survival.hz | Survives with 2-4 neighbors instead of 2-3 | Medium | Probe nextCellState with 4 neighbors |
| gameoflife-bug-sequential.hz | Sequential update instead of simultaneous | Hard | Compare before/after state during step |

## Calculator Bug Variants

| File | Bug | Difficulty | Probe Strategy |
|------|-----|------------|----------------|
| calculator-bug-precedence.hz | All operators at same precedence level | Medium | Probe parse tree for "2+3*4" |
| calculator-bug-associativity.hz | Right-associative instead of left | Medium | Probe parse tree for "10-5-2" |

## Bug Difficulty Scale

- **Easy**: Single character or token fix, obvious once located
- **Medium**: 1-2 lines in one location, requires tracing execution
- **Hard**: Multiple locations or subtle logic interactions

## Debugging with Probes

1. Run `./hazel test program-bug.hz` to see failing test
2. Add `^^probe(expr)` to inspect values
3. Run `./hazel probe program-bug.hz` to see probe output
4. Use `--many` flag to see all samples: `./hazel probe -m program-bug.hz`

See `plans/study-programs.md` for full documentation.
