# Study Programs for Probe Debugging Research

This document defines the requirements and process for creating Hazel programs for user studies on the probe debugging mechanism.

## Purpose

Create a corpus of programs with intentional bugs that users can debug using Hazel's probe mechanism. Each program has:
- A **working version** (`program.hz`) with comprehensive tests
- A **buggy version** (`program-bug.hz`) with minimal failing test(s)

## Probe Mechanism Overview

Probes (`^^probe(expr)`) capture runtime values with full context:
- **Value**: The evaluated expression
- **Environment**: Variables in scope (filtered to referenced ones)
- **Call stack**: Which function calls led here
- **Step range**: When in evaluation this occurred

**Key capabilities for debugging:**
- **Single/Many modes**: See one sample or all iterations
- **Call stack navigation**: Arrow keys to move between samples at different call depths
- **Step-into**: Jump into a function to see its body's samples for a specific call
- **Pinning**: Lock display to samples from a specific call context

**Bugs well-suited for probe debugging:**
- Wrong variable used (probe shows unexpected indices/values)
- Off-by-one errors (probe shows boundary conditions)
- Wrong accumulator/base case (probe shows evolution over iterations)
- State transformation errors (probe shows before/after)
- Condition logic bugs (probe shows which branch taken)

## Program Requirements

### Structure
- **Length**: 150-500 lines including tests
- **Architecture**: Prefer MVU-style (Model, Actions, Update) for stateful programs
- **Tests**: Working version has comprehensive test suite; buggy version has 1-3 tests (usually just 1 failing)

### Bug Design
- **Difficulty range**: Single-character fix to 2-3 locations with 1-2 lines each
- **Probe-debuggable**: Bug should be discoverable by placing probes and observing values
- **Realistic**: Bugs should feel like natural mistakes, not contrived puzzles
- **Information balance**: Tests reveal *that* something is wrong, not *what* - users must explore

### Themes
- Add personality even to conventional concepts (e.g., "EmojiPaint" not just "PaintGrid")
- Quirky framings make programs memorable and engaging

### Code Style
- **Use proper indentation** - 2 spaces per level is standard
- Keep lines reasonable length (~80-100 chars)
- Use comments sparingly but meaningfully

## Development Process

### 1. Start with MVP
Build minimal working version with core functionality and basic tests.

### 2. Add Features Incrementally
Expand naturally - this creates organic code structure with realistic complexity.

### 3. Write Comprehensive Tests
Cover all features, edge cases, and interactions. Tests define correctness.

### 4. Create Buggy Version
- Copy working version
- Introduce bug(s) that break specific behavior
- Reduce tests to minimal set that exposes the bug
- Verify bug is probe-debuggable

### 5. Validate
- Run `./hazel test program.hz` - all pass
- Run `./hazel test program-bug.hz` - expected failures
- Manually verify bug can be found with probes

## CLI Tools Reference

```bash
./hazel run program.hz       # Execute, print result
./hazel analyze program.hz   # Type check with error locations
./hazel test program.hz      # Run tests, show failures
./hazel test -v program.hz   # Show all tests
./hazel probe program.hz     # Show probe values inline
./hazel probe -m program.hz  # Show multiple samples per probe
```

## Documentation Reference

- `plans/hazel-lsp-cli/hazel-primer.md` - Complete Hazel syntax guide
- `plans/hazel-lsp-cli/cli-api.md` - CLI documentation
- `plans/hazel-lsp-cli/hazel-builtins.md` - Built-in functions
- `hazel-programs/` - Example programs

**Key Hazel notes:**
- No `let rec` - use arrow type annotation for recursion: `let f : A -> B = fun x -> ...`
- Use `?` for type holes, not `'a`
- Tests: `test expr end` or `hint "name" test expr end`
- Tests must be sequenced: `let _ = test ... end in ...`

## Program Concepts

### Tier 1: MVU / Interactive (ready to implement)

#### Tamagotchi
Virtual pet with stats (hunger, happiness, energy, health). Actions affect stats based on current state. Stats decay over time and interact (hungry → grumpy). Evolution based on care patterns. Memory of events affects personality.
- **Target**: ~300-400 lines
- **Bug ideas**: Stat decay calculation, evolution threshold, interaction effects
- **Theme**: Choose a quirky pet type (alien blob, haunted toaster, etc.)

#### Tic-Tac-Toe
3x3 board, alternating turns, win detection (rows, cols, diagonals), draw detection.
- **Target**: ~120-150 lines
- **Bug ideas**: Diagonal win check, turn logic, draw detection
- **Theme**: Maybe emoji-based (⭕❌) or thematic (cats vs dogs)

#### Vending Machine
Coins, balance tracking, product selection, dispensing, change calculation.
- **Target**: ~150-180 lines
- **Bug ideas**: Change calculation (greedy algorithm), insufficient funds, inventory
- **Theme**: Cursed vending machine with weird items

#### Shopping Cart
Products with prices, quantities. Add/remove items, apply discounts, calculate totals.
- **Target**: ~150-180 lines
- **Bug ideas**: Discount stacking, percentage vs flat, quantity edge cases
- **Theme**: Interdimensional marketplace

### Tier 2: Simulations (ready to implement)

#### Conway's Game of Life
Grid with birth/death rules based on neighbor counts. Step function, boundary handling.
- **Target**: ~150-200 lines
- **Bug ideas**: Neighbor counting at edges, simultaneous vs sequential update
- **Theme**: Standard or with emoji cells

#### Cellular Automaton Zoo
Multiple rule sets (Life, Brian's Brain, Wireworld). Switch rules, paint states, step/run.
- **Target**: ~350-450 lines
- **Bug ideas**: Rule-specific neighbor logic, state representation per automaton
- **Theme**: "Museum of Artificial Life"

#### Gravity Sandbox
2D grid with falling materials: sand (piles), water (flows), steam (rises). Material interactions.
- **Target**: ~350-450 lines
- **Bug ideas**: Update order (top-down vs bottom-up), material interaction table
- **Theme**: Pocket universe creator

#### Ecosystem Simulation
Grass grows, rabbits eat grass, foxes eat rabbits. Energy/hunger, reproduction thresholds.
- **Target**: ~300-400 lines
- **Bug ideas**: Reproduction threshold, energy transfer, simultaneous update
- **Theme**: "Tiny Planet"

### Tier 3: Algorithms & Data (ready to implement)

#### Expression Parser & Evaluator
Tokenize and parse arithmetic with precedence, evaluate AST.
- **Target**: ~250-350 lines
- **Bug ideas**: Operator precedence, associativity, parenthesis handling
- **Theme**: "Arcane Calculator"

#### Binary Search Tree
Insert, lookup, delete, traversal. Balance checking optional.
- **Target**: ~150-200 lines
- **Bug ideas**: Delete with two children, comparison direction
- **Theme**: "Family Tree Manager"

#### LRU Cache
Fixed-size cache with least-recently-used eviction. Get updates recency.
- **Target**: ~120-150 lines
- **Bug ideas**: Recency tracking, eviction selection, capacity edge cases
- **Theme**: "Memory Palace"

#### Merge Sort Visualizer
Divide-and-conquer sort tracking intermediate states.
- **Target**: ~100-130 lines
- **Bug ideas**: Merge logic, base case size, index calculations
- **Theme**: Standard

### Tier 4: Quirky / Experimental (ready to implement)

#### Potion Brewing
Combine ingredients with properties (hot/cold, stable/volatile). Order matters. Temperature management.
- **Target**: ~250-350 lines
- **Bug ideas**: Ingredient interaction matrix, temperature state machine, effect stacking
- **Theme**: "Witch's Workshop"

#### Rube Goldberg Simulator
Chain of cause-effect objects (dominoes, balls, ramps). Simulate reaction, validate goal reached.
- **Target**: ~300-400 lines
- **Bug ideas**: Trigger conditions, chain propagation, collision detection
- **Theme**: "Contraption Constructor"

#### Code Golf Scorer
Expression language, count strokes by token weights, apply optimizations, score result.
- **Target**: ~250-350 lines
- **Bug ideas**: Transformation correctness, scoring rules, optimization ordering
- **Theme**: "Competitive Shrinking"

#### Horoscope Calculator
Birth date → zodiac signs, elements, compatibility scores. Chinese zodiac optional.
- **Target**: ~200-300 lines
- **Bug ideas**: Date boundary (cusp dates), compatibility matrix, element interactions
- **Theme**: "Cosmic Compatibility Engine"

### Tier 5: Longer / More Complex (stretch goals)

#### Text Adventure Engine
Rooms, objects, NPCs with dialogue trees, puzzle state, inventory combinations.
- **Target**: ~400-500 lines
- **Bug ideas**: State flag logic, inventory interactions, dialogue conditions

#### Spreadsheet Evaluator
Grid with formulas referencing cells. Topological evaluation order, cycle detection.
- **Target**: ~300-400 lines
- **Bug ideas**: Coordinate parsing, evaluation order, circular references

#### Stack Language Interpreter
Forth-like: push, pop, arithmetic, conditionals, loops.
- **Target**: ~150-200 lines
- **Bug ideas**: Stack underflow, loop counter, conditional branching

#### Music Sequencer
Grid of beats × instruments, patterns, tempo, swing timing.
- **Target**: ~250-350 lines
- **Bug ideas**: Timing calculations, pattern boundaries, swing offset

## File Organization

```
hazel-programs/study/debugging/
├── emojipaint/
│   ├── emojipaint.hz       # Working version
│   └── emojipaint-bug.hz   # Buggy version
├── tamagotchi/
│   ├── tamagotchi.hz
│   └── tamagotchi-bug.hz
├── ...
└── README.md               # Index of programs with difficulty ratings
```

## Difficulty Rating

Rate each buggy program on:
- **Lines to change**: 1 (single char) / 2-3 (one location) / 4-6 (multiple locations)
- **Conceptual difficulty**: Easy (obvious once seen) / Medium (requires tracing) / Hard (subtle interaction)
- **Probe skill needed**: Basic (one probe) / Intermediate (multiple/navigation) / Advanced (pinning/step-into)

## Checklist for Each Program

- [ ] Working version compiles: `./hazel analyze program.hz`
- [ ] Working version tests pass: `./hazel test program.hz`
- [ ] Buggy version has failing test(s): `./hazel test program-bug.hz`
- [ ] Bug is discoverable via probes (manually verified)
- [ ] Both versions documented in study folder README
- [ ] Difficulty rated
