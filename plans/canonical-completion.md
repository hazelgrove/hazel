# Canonical Completion Plan

## Context and Motivation

### The Segment/Term Duality

Hazel has two representations of syntax:
- **Segments**: A "half-parsed" representation where delimiters are matched but full precedence parsing hasn't occurred. Good for text-like editing (character insertion/deletion).
- **Terms**: Full AST representation. Good for semantics (type checking, evaluation), collaboration/versioning (Grove/CRDT, patchwork), and refactoring.

### The Problem

Segments can represent *incomplete* syntax - forms where not all delimiters have been typed:
- `let x =` (missing `in`)
- `(1, 2` (missing `)`)
- `fun x` (missing `->`)

Currently, converting segment → term requires "completing" these incomplete forms. This is done by `Dump.re`, which:
1. Operates at the zipper level (cursor-dependent)
2. Only handles trailing delimiters
3. Requires a second `MakeTerm` pass after completion

### The Vision

We want:
1. **Terms as canonical representation** - for collaboration, versioning, refactoring
2. **Round-trippability** - segment → term → segment preserves structure
3. **Canonical completion integrated into MakeTerm** - no separate pass, no cursor dependency
4. **Shard annotations** - terms record which delimiters were originally typed

This enables:
- Doing term-level transformations oblivious to edit state
- Materializing segments on-demand for text editing
- Treating incomplete syntax deep in a tree as "just along for the ride" during refactoring

### What Round-Tripping Means

We accept that round-tripping is **structural equality modulo grout IDs**:
- Tile IDs are preserved (important for refractors, term_data, etc.)
- Grout IDs are ephemeral (completion may create/remove grout, new grout gets fresh IDs)
- The "shape" of incompleteness is preserved via shard annotations

---

## Current Implementation Analysis

### Dump.re Heuristics

Current algorithm (lines 12-60):
```
1. Set caret to Outer (allows put_down operations)
2. Loop:
   a. move_until_can_put_down - find where backpack can drop
   b. move_until_cant_put_down - go as far as possible while:
      - can_put_down is true
      - no linebreak to right of caret
   c. put_down_as_much_as_possible - drop all available shards
   d. repeat until backpack empty
```

Key heuristics:
- **Linebreak sensitivity**: Stops before linebreaks (treats them as semantic boundaries)
- **Right-biased**: Moves right, completing trailing delimiters
- **Greedy**: Goes as far as possible before dropping

### Indentation.re Heuristics

Different approach (lines 41-109):
- **Blank line sensitivity**: Uses two consecutive linebreaks as intent signal
- **Shallow completion**: Just fills in shard indices without restructuring
- **Non-cursor-dependent**: Can run during passive operations

### Key Functions

From `Tile.re`:
- `is_complete(t)` = `length(t.label) == length(t.shards)`
- `right_missing_shards(t)` - trailing shards not yet typed
- `left_missing_shards(t)` - leading shards not yet typed
- `missing_shards(t)` - all missing shards

From `Segment.re`:
- `incomplete_tiles(seg)` - tiles with missing shards
- `regrout((l_shape, r_shape), seg)` - adds grout to resolve shape conflicts

---

## Multi-Delimiter Forms in Hazel

### Three-Delimiter Forms
| Form | Label | Example |
|------|-------|---------|
| Let | `["let", "=", "in"]` | `let x = 1 in x` |
| TypeAlias | `["type", "=", "in"]` | `type t = Int in 1` |
| If | `["if", "then", "else"]` | `if true then 1 else 2` |

### Two-Delimiter Forms
| Form | Label | Example |
|------|-------|---------|
| Fun | `["fun", "->"]` | `fun x -> x` |
| Fix | `["fix", "->"]` | `fix f -> f` |
| Parens | `["(", ")"]` | `(1 + 2)` |
| ListLit | `["[", "]"]` | `[1, 2, 3]` |
| Case | `["case", "end"]` | `case x \| A => 1 end` |
| Filter* | `["hide/eval/pause/debug", "in"]` | `hide e in body` |

### Incomplete Scenarios

**Trailing missing (most common)**:
- `let x =` → missing `in` (shards [0, 1], missing [2])
- `let x` → missing `=` and `in` (shards [0], missing [1, 2])
- `fun x` → missing `->` (shards [0], missing [1])
- `(1, 2` → missing `)` (shards [0], missing [1])
- `if true then 1` → missing `else` (shards [0, 1], missing [2])

**Leading missing (rarer)**:
- `1, 2)` → missing `(` (shards [1], missing [0])
- `end` → ambiguous - could be case, test, proof_of, etc.

**Middle missing (rare)**:
- `let x in` → missing `=` (shards [0, 2], missing [1])
- `if true else 2` → missing `then` (shards [0, 2], missing [1])

---

## Implementation Plan

### Current Status (January 2025)

**DONE:**
- Phase 1: Trailing delimiter completion (single segment)
- Phase 2: Unit tests (35 tests passing)
- Phase 3: Recursive wrapper (descends into children with correct sorts)
- Phase 7: Indentation.re now uses CanonicalCompletion
- User-managed indentation: spaces auto-inserted on Enter, Format action (Cmd+S)

**NOT DONE:**
- Zero-indent partitioning heuristic (see section below)
- Phase 4: MakeTerm integration (partial - calls completion but doesn't use shard_records)
- Phase 5: Leading and middle delimiter completion
- Phase 6: ExpToSegment integration (round-tripping)
- Cached backpack optimization (skip completion if no incomplete tiles in syntax cache)

**Performance optimizations implemented:**
- Single-pass partition_at_blank_lines collects incomplete tiles during scan
- Regrout/reassemble happen once at end, not per-subsegment

---

## Indentation-Based Partitioning Heuristic

### Motivation

With user-managed indentation (spaces auto-inserted on Enter, but deletable), the presence/absence of spaces after a linebreak encodes user intent. When a user:
1. Types `let f = fun x`
2. Presses Enter → auto-inserts 2 spaces (function body indent)
3. Deletes those spaces and types `f(1)` at column 0

The deletion signals that `f(1)` is meant to be separate, not the function body.

### Current Behavior (blank-line only)

```
let f = fun x
f(1)
```
→ Completed as one chunk: `let f = fun x\nf(1)->?in?` (f(1) sucked into function)

### Desired Behavior (zero-indent heuristic)

```
let f = fun x
f(1)
```
→ Partitioned: `let f = fun x->?in?\nf(1)` (f(1) is separate)

### Examples

| Input | Expected | Reasoning |
|-------|----------|-----------|
| `let f = fun x`<br>`body` | `let f = fun x->?in?`<br>`body` | `body` at column 0 → separate |
| `let f = fun x`<br>`  body` | `let f = fun x`<br>`  body->?in?` | `body` indented → part of function |
| `let f = fun x`<br>`  a`<br>`b` | `let f = fun x`<br>`  a->?in?`<br>`b` | `a` indented (in fun), `b` at column 0 (separate) |
| `let x = 1`<br><br>`y` | `let x = 1`<br>`in`<br>`y` | Blank line → partition (existing heuristic) |

### Implementation Plan

**Zero-indent heuristic (Option 1 - simple):**
- In `partition_at_blank_lines`, add condition: if we see a linebreak followed by zero spaces (next piece is NOT a space Secondary), and there are incomplete tiles before, partition there
- This is IN ADDITION to the existing blank-line heuristic
- Handles the common case of "new top-level definition"

**Algorithm sketch:**
```
| [Secondary(linebreak), next_piece, ...rest]
    when !is_space(next_piece) && incomplete_before =>
  // Partition here - zero indent after incomplete tile
```

**Helper needed:**
```reason
let is_space_piece = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(s) => Secondary.is_space(s)
  | _ => false
  };
```

### Future Refinements

**Blank-line refinement (TODO):**
Currently blank-line = two consecutive linebreaks. Should generalize to: linebreak, followed by any combination of whitespace/grout (no tiles), followed by linebreak. This handles cases where grout insertion leaves a "blank" line with concave grout on it.

**Decreasing indent (more sophisticated, if needed):**
Instead of just zero-indent, partition at any *decrease* in indentation. Would handle nested cases more precisely but adds complexity.

---

### Phase 1: Non-Recursive Segment Completion Function

Create a standalone function that completes a single segment (no recursion into children):

```reason
(* In CanonicalCompletion.re *)

type shard_record = {
  tile_id: Id.t,
  original_shards: list(int),
};

type completion_result = {
  completed_seg: Segment.t,
  shard_records: list(shard_record),
};

(* Complete trailing delimiters for incomplete tiles in a flat segment.
 *
 * ~insert_separators: If true, add spaces where tokens would jam together.
 *   - true: For editor affordances (click-to-complete), readable output
 *   - false: For semantics (MakeTerm), minimal output
 *)
let complete_segment: (~insert_separators: bool=?, Segment.t) => completion_result;
```

**Two Modes:**
- `insert_separators=false` (default): Minimal completion for semantics. Tokens may jam together (e.g., `1in?`), but structure is correct.
- `insert_separators=true`: Readable completion for editor use. Adds spaces where needed (e.g., `1 in ?`).

**Key Insight from Existing Code**:

The zipper-level backpack logic (in `Siblings.re:61-66`) shows the pattern:
- For tiles to the LEFT of cursor: their `right_missing_shards` need to be dropped
- For tiles to the RIGHT of cursor: their `left_missing_shards` need to be dropped

For segment-level (no cursor), we process left-to-right:
- Each incomplete tile's `right_missing_shards` get dropped after the tile
- We don't handle `left_missing_shards` in Phase 1 (that's Phase 5)

**Algorithm Sketch**:
```
complete_segment(seg):
  result = []
  shard_records = []
  for each piece in seg:
    if piece is incomplete tile t:
      record (t.id, t.shards) in shard_records
      missing = right_missing_shards(t)
      drop_point = find_drop_position(rest_of_seg, missing)
      insert missing shards at drop_point
      regrout if shape conflict
    result.append(piece)
  return (result, shard_records)
```

**Subtasks**:
1. [ ] Create `CanonicalCompletion.re` with basic structure
2. [ ] Implement `find_incomplete_tiles` - identify tiles needing completion
3. [ ] Implement `find_drop_position` - where to insert missing trailing shards
       - Heuristic: go as far right as possible, stop at linebreak
4. [ ] Implement `complete_tile` - create completed tile with all shards
5. [ ] Implement `insert_trailing_shards` - insert at drop point, regrout
6. [ ] Handle shape conflicts via `Segment.regrout`
7. [ ] Return shard records for annotation

### Phase 2: Unit Tests for Segment Completion

Test file: `test/Test_CanonicalCompletion.re`

**Test patterns** (using textual syntax where possible):

```reason
(* Trailing delimiter tests *)
completion_test("let x = 1", "let x = 1 in ?");
completion_test("let x", "let x = ? in ?");
completion_test("fun x", "fun x -> ?");
completion_test("(1, 2", "(1, 2)");
completion_test("if true then 1", "if true then 1 else ?");

(* With linebreak sensitivity *)
completion_test("let x = 1\ny", "let x = 1 in ?\ny");

(* Nested incomplete - flat version doesn't recurse *)
completion_test("let x = let y", "let x = let y in ?");  (* only outer completed *)
```

**Subtasks**:
1. [ ] Create test file with infrastructure
2. [ ] Add simple trailing delimiter tests
3. [ ] Add linebreak sensitivity tests
4. [ ] Add tests showing grout insertion (shape conflicts)
5. [ ] Add tests with multiple incomplete tiles

### Phase 3: Recursive Wrapper

Create a recursive version that descends into tile children:

```reason
(* Complete all incomplete tiles in a segment and its children *)
let complete_segment_deep: Segment.t => completion_result;
```

This uses `complete_segment` as the workhorse, applying it at each level.

**Subtasks**:
1. [ ] Implement recursive traversal
2. [ ] Aggregate shard_info from all levels
3. [ ] Add tests for nested completion

### Phase 4: Integration with MakeTerm

Integrate completion into the MakeTerm process:

```reason
(* In MakeTerm.re, before processing a segment *)
let (seg, shard_info) = CanonicalCompletion.complete_segment_deep(seg);
(* ... proceed with normal MakeTerm logic ... *)
(* Store shard_info in term annotations *)
```

**Subtasks**:
1. [ ] Add `original_shards` field to `IdTagged.IdTag.t`
2. [ ] Thread shard_info through MakeTerm
3. [ ] Verify existing tests still pass
4. [ ] Remove/simplify Dump.re usage

### Phase 5: Leading and Middle Delimiters

Extend completion to handle non-trailing cases:

**Leading delimiters**:
- Option A: Disregard unmatched trailing delimiters (treat as errors)
- Option B: Insert at segment start
- Needs design discussion

**Middle delimiters**:
- Insert immediately before the next present shard
- E.g., `let x in` → insert `=` before `in`

**Subtasks**:
1. [ ] Design heuristics for leading delimiters
2. [ ] Implement middle delimiter completion
3. [ ] Add tests for edge cases

### Phase 6: ExpToSegment Integration

Extend ExpToSegment to use shard annotations:

```reason
(* When emitting a term with original_shards annotation *)
| Let(p, def, body) when has_incomplete_shards(exp) =>
  (* Emit only the shards that were originally present *)
  emit_partial_let(original_shards, p, def, body)
```

**Subtasks**:
1. [ ] Add `original_shards` handling to ExpToSegment
2. [ ] Handle regrout removal when emitting incomplete
3. [ ] Add round-trip tests

### Phase 7: Consider Indentation Integration

Evaluate whether the same completion logic can replace `Indentation.re`'s completion:

- Compare heuristics (linebreak vs blank-line)
- May want different behavior for indentation calculation
- Lower priority than MakeTerm integration

---

## Test Examples

### Simple Trailing Completion

| Input | Expected Output | Notes |
|-------|-----------------|-------|
| `let x = 1` | `let x = 1 in ?` | Missing `in`, add hole |
| `let x` | `let x = ? in ?` | Missing `=` and `in` |
| `fun x` | `fun x -> ?` | Missing `->` |
| `(1` | `(1)` | Missing `)`, no hole needed |
| `[1, 2` | `[1, 2]` | Missing `]` |
| `if true then 1` | `if true then 1 else ?` | Missing `else` |
| `case x \| A => 1` | `case x \| A => 1 end` | Missing `end` |

### With Linebreak Heuristic

| Input | Expected Output | Notes |
|-------|-----------------|-------|
| `let x = 1↵y` | `let x = 1 in ?↵y` | Linebreak stops completion |
| `let x = 1↵↵y` | `let x = 1 in ?↵↵y` | Blank line also stops |
| `let f = fun x↵1` | `let f = fun x -> ?↵1` or `let f = fun x -> 1 in ?` | Needs design decision |

### Nested Completion

| Input | Expected Output | Notes |
|-------|-----------------|-------|
| `let x = let y = 1` | `let x = let y = 1 in ? in ?` | Both lets completed |
| `fun x -> let y` | `fun x -> let y = ? in ?` | Fun complete, let incomplete |
| `(let x = 1` | `(let x = 1 in ?)` | Paren and let both completed |

### Shape Conflicts (Grout Insertion)

| Input | Expected | Notes |
|-------|----------|-------|
| `let x = 1` | `let x = 1 in ?` | `in` is infix, needs hole after |
| `fun x` | `fun x -> ?` | `->` expects term, needs hole |

### Leading/Middle (Future)

| Input | Expected | Notes |
|-------|----------|-------|
| `1, 2)` | `(1, 2)` or `? 1, 2)` | Design TBD |
| `let x in 1` | `let x = ? in 1` | Insert `=` before `in` |
| `if true else 2` | `if true then ? else 2` | Insert `then` |

---

## Open Questions

1. **Linebreak vs blank-line heuristic**: Dump uses single linebreak, Indentation uses blank line. Which is better? Should they differ?

2. **Leading delimiter strategy**: Disregard, insert at start, or something else?

3. **Where to store shard info**:
   - New field in `IdTagged.IdTag.t`?
   - Separate map in MakeTerm?
   - In `term_data`?

4. **Interaction with Dump**: Replace Dump entirely, or keep it for specific use cases?

5. **Performance**: Is segment-level completion fast enough, or do we need caching like Indentation?

6. **Unit testing building blocks**: It might be valuable to write focused unit tests for `Segment.reassemble` and `Segment.regrout` (and possibly other functions they use). Current tests are more exploratory/diagnostic. Clean unit tests would help document expected behavior and catch regressions.

---

## Files to Modify/Create

| File | Change |
|------|--------|
| `src/haz3lcore/derived/CanonicalCompletion.re` | NEW: Core completion logic |
| `test/Test_CanonicalCompletion.re` | NEW: Unit tests |
| `src/haz3lcore/lang/MakeTerm.re` | Integration point |
| `src/haz3lcore/IdTagged.re` | Add `original_shards` field |
| `src/haz3lcore/statics/ExpToSegment.re` | Emit incomplete forms |
| `src/haz3lcore/zipper/Dump.re` | Simplify/remove |
| `src/haz3lcore/derived/Indentation.re` | Potentially use shared completion |

---

## Indentation Performance Optimization Options

When auto-inserting indentation on linebreak, we currently compute the full `Indentation.level_map` for the entire program, then look up just the one linebreak we care about. If this becomes a performance issue, here are options:

| Option | Performance | Implementation Difficulty | Maintenance Burden |
|--------|-------------|---------------------------|-------------------|
| **Current approach** | O(program size) | Already done | Low |
| **Early termination via exception** | O(position in program), short-circuits | Low - add exception to `go'`, throw when target ID found | Low - minimal change to existing structure |
| **Local computation from zipper** | O(nesting depth) | Medium - new function, need to handle "effective prev/next" correctly | Medium - separate code path to maintain |
| **Incremental/differential** | O(changed region) | High - track changes, propagate updates | High - complex invariants |

**Recommendation:** If optimization needed, try early termination first (option 2). It's a small diff and provides meaningful speedup for linebreaks early in the program. Only pursue zipper-based computation (option 3) if profiling shows option 2 is insufficient.

---

## Success Criteria

1. All existing tests pass
2. New unit tests for completion scenarios
3. `Dump.re` no longer needed (or greatly simplified)
4. Round-trip tests: segment → term → segment preserves structure
5. Incomplete syntax deep in a tree doesn't break refactoring operations
