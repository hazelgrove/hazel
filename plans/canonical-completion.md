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
- Phase 2: Unit tests (35+ tests passing)
- Phase 3: Recursive wrapper (descends into children with correct sorts)
- Phase 7: Indentation.re now uses CanonicalCompletion
- User-managed indentation: spaces auto-inserted on Enter, Format action (Cmd+S)
- Relative indent partitioning heuristic (partition when content is same-or-lesser indented than incomplete tile)
- Parser fix: `~auto_indent=false` prevents Parser from adding indentation spaces

**NOT DONE:**
- Phase 4: MakeTerm integration (partial - calls completion but doesn't use shard_records)
- Phase 5: Leading and middle delimiter completion
- Phase 6: ExpToSegment integration (round-tripping)
- Cached backpack optimization (skip completion if no incomplete tiles in syntax cache)

**FUTURE CONSIDERATION:**
The `indentation-improvements-II` branch has a state-based single-pass indentation algorithm that may be worth bringing in later if we can get it working properly. It has some nice properties (explicit state tracking, continuation detection) but had issues when initially integrated. The current fold_left2-based algorithm is simpler and self-contained.

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

### Current Implementation: Relative Indent Heuristic

**Partition heuristics (when incomplete_before is true):**
1. **BLANK LINE**: Two consecutive linebreaks always partition
2. **RELATIVE INDENT**: After a linebreak, if content's indent ≤ incomplete tile's indent, partition

The relative indent heuristic interprets same-or-lesser indented content after incomplete syntax as user intent to start something new. This subsumes the simpler "zero indent" case: incomplete at col 0, content at col 0 means 0 ≤ 0 → partition.

**Key insight:** We track the column position (indent level) of the first incomplete tile encountered. When we see a linebreak followed by content, we compare:
- Content MORE indented than incomplete → absorb (part of incomplete form)
- Content SAME or LESS indented than incomplete → partition (separate)

### Examples

| Input | Expected | Reasoning |
|-------|----------|-----------|
| `let f = fun x`<br>`body` | `let f = fun x->`<br>`body` | `body` at column 0, fun at ~col 9 → 0 ≤ 9 → partition |
| `let f = fun x`<br>`  body` | `let f = fun x`<br>`  body->?` | `body` at 2, fun at ~col 9 → 2 ≤ 9 → partition |
| `fun x ->`<br>`    let`<br>`    y` | `fun x ->`<br>`    let?=?in`<br>`    y` | `let` at col 4, `y` at col 4 → 4 ≤ 4 → partition |
| `fun x ->`<br>`    let`<br>`      y` | `fun x ->`<br>`    let`<br>`      y=?in?` | `let` at col 4, `y` at col 6 → 6 > 4 → absorb |
| `let x = 1`<br><br>`y` | `let x = 1`<br>`in`<br>`y` | Blank line → partition (heuristic 1) |

### Implementation Details

**Tracking state in partition_segment:**
- `line_indent`: spaces since last linebreak (reset on linebreak, increment on space before content)
- `past_indent`: whether we've seen non-space content on this line
- `incomplete_indent`: option(int), the column of the first incomplete tile encountered

**Helper functions:**
- `count_leading_spaces(seg)`: count space pieces at start of segment
- `is_space_piece(p)`: check if piece is horizontal whitespace

### Known Limitations

**Delimiter stealing:** When an incomplete form (like `let`) is followed by content containing its missing delimiter, the parser may match the delimiter to the incomplete form. For example:
```
let f = fun x ->
  let
  body
in f(1)
```
Here the `in` at column 0 gets matched to the INNER incomplete `let`, not the outer one. To test the relative indent heuristic in nested contexts, use forms that can't steal delimiters (like `fun` which uses `->`).

### Future Refinements

**Blank-line refinement (TODO):**
Currently blank-line = two consecutive linebreaks. Should generalize to: linebreak, followed by any combination of whitespace/grout (no tiles), followed by linebreak. This handles cases where grout insertion leaves a "blank" line with concave grout on it.

### Known Indentation Bugs

**Operator continuation doesn't auto-indent (TODO):**
When typing `let x = 1` then Enter then `+ 2`, the `+ 2` should ideally get auto-indented (it's a continuation of the expression). Currently, the indentation logic doesn't handle this case - the user must manually indent. This is acceptable for now but should be fixed in `Indentation.re`'s `is_incrementor` or related logic.

**Case expression auto-indent (FIXED):**
Previously, pressing Enter in a complete case expression gave unwanted 2-space indentation. This was fixed by:
- Adding `effective_prev_pieces` and `effective_next_pieces` to skip over grout/linebreaks
- Adding `is_complete_case_rule_with_body` to detect complete rules with content
- Adding indentation rules that reset to base after complete case rules

Tests in `Test_Editing.re` under `case_indent_tests` and `nested_case_tests` verify this behavior.

### Case Rule Completion Skip (Load-Bearing)

`Indentation.re`'s `shallow_complete_segment` skips completing case rules (tiles with label `["|", "=>"]`). This is **load-bearing** - removing it causes 3 tests to fail (incomplete `|` swallows siblings as body).

Rationale: case rules are sibling-oriented (they don't nest into each other), so swallowing subsequent content as children is wrong.

### Editor Improvements

**Trailing whitespace cleanup:**
Lines may accumulate trailing whitespace (spaces before linebreaks). This can happen when:
- User deletes content but leaves spaces
- Auto-indent inserts spaces, then user immediately presses Enter again
- Cursor moves away leaving trailing spaces

Implementation:
- **On Format (Cmd+S)**: Strip spaces immediately before each linebreak
- Implemented in `Indentation.fix_indentation_in_segment` or as a pre-pass
- Future consideration: also clean on Enter (previous line only)

**Indent-level backspace:**
When the cursor is in leading whitespace (only spaces between linebreak and cursor), Backspace deletes 2 spaces at a time (one indent level) instead of 1. This is standard behavior in most editors:

- **VSCode**: `editor.useTabStops` setting
- **Sublime Text**: `use_tab_stops` setting
- **JetBrains IDEs**: Similar behavior for indentation-only lines

Implementation:
- Detect "leading whitespace context" in `Destruct.go`: cursor is Outer, left neighbors are only space pieces back to a linebreak
- If in leading whitespace: delete min(2, num_spaces) spaces
- Otherwise: fall through to normal single-character Destruct
- Edge cases: 1 space → delete 1; 3 spaces → delete 2 leaving 1

**Token-delete with hungry delete for whitespace:**
A modifier+backspace shortcut that operates at token granularity, with special "hungry delete" behavior for whitespace runs. Uses the existing `Destruct` action with a `chunkiness` parameter (like `Move`).

Keyboard shortcuts (matching platform standards):
- **Mac**: Option+Backspace → `Destruct(Left, ByToken)`
- **PC**: Ctrl+Backspace → `Destruct(Left, ByToken)`

Behavior depends on context:
1. **After contentful token**: Delete the entire token
   ```
   let foo| = 1  →  let | = 1   (deleted "foo")
   ```
2. **In whitespace run** (spaces/linebreaks, NOT comments): Hungry delete
   - Delete leading spaces on current line
   - Delete the preceding linebreak
   - Delete trailing spaces on previous line
   - Stop at previous line's content
   ```
   let x = 1
       |body  →  let x = 1|body   (joined lines)
   ```
   - Multiple blank lines require multiple presses (one linebreak per press)

Implementation:
- Add `chunkiness` parameter to `Destruct`: `Destruct(Direction.t, chunkiness)`
- Default `ByChar` preserves existing behavior
- `ByToken` mode: check if in whitespace run, if so do hungry delete, else delete token
- Only delete Secondary pieces where `is_space` or `is_linebreak` is true (preserve comments)

References:
- [Emacs CC Mode Hungry Delete](https://www.gnu.org/software/emacs/manual/html_node/ccmode/Hungry-WS-Deletion.html)
- [VSCode Hungry Delete extension](https://marketplace.visualstudio.com/items?itemName=jasonlhy.hungry-delete)

### Ideas for Indentation Refinement

**Prev-only logic (potential simplification):**
With user-managed indentation, we might not need to look at "next content" at all. The algorithm could work purely from what's before the linebreak:
- Inside incomplete container → `base + 2`
- After incrementor → `level + 2`
- After complete expression → maintain `level`

Edge cases (commas, case rules, `in`/`then`/`else`) would rely on auto-format to fix after the user types. This would eliminate the need for completion heuristics entirely.

**Two systems at play:**
1. **On-Enter cursor placement** - where caret goes when pressing Enter (uses current indent algorithm)
2. **Auto-format** - recomputes with full context (Cmd+S / Format action)

The on-Enter placement can be "good enough" with auto-format as fallback. Users already expect to sometimes adjust indentation.

**Tuple context issue:**
After `fun x -> x` inside a tuple, pressing Enter puts cursor at paren level (2 spaces) instead of tuple content level (4 spaces). Auto-format fixes it once comma is typed. This is because `(_, None) => base` fires when there's no next content yet.

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

---

## Technical Debt & Cleanup Tasks

### auto_indent Parameter

**What it does:** Controls whether `Insert.re` auto-inserts indentation spaces after linebreaks.

**Where it's set to false:** Only in `Parser.to_zipper` (Parser.re:7)

**Why it's necessary:** The Parser needs to faithfully reproduce input strings. If `auto_indent=true` during parsing:
1. Parser inserts characters including `\n`
2. On `\n`, auto_indent adds 2 spaces
3. Parser continues inserting the spaces that were already in the input
4. Result: double indentation

For strings WITHOUT indentation (like old doc slides), parsing with `auto_indent=true` would ADD indentation. This could potentially be used for migration (see below).

### Test Harness: parse_with_caret

Added `parse_with_caret()` in Test_Editing.re which uses `Parser.to_zipper` (auto_indent=false) to create zippers from strings. This is necessary because `mk()` uses auto_indent=true, causing double-indentation in tests with multiline indented code.

Use `test_from_parse()` for tests that need exact initial state without added indentation.

---

## Doc Slides Migration (Important)

### The Problem

The `src/b2t2/slides/` directory contains **39 ML files** with serialized segments in the old format:
- Segments have `Whitespace\"\\n\"` (linebreaks) followed directly by tiles
- No indentation spaces after linebreaks
- `backup_text` also has no indentation

These slides will render without indentation in the editor.

### Example (from B2T2ExampleTables.ml backup_text):
```
let students : [Student] = [
("Bob", 12, "blue"),     <- no leading spaces
("Alice", 17, "green"),
```

### Migration Options

**Option 1: Regex-based (fragile)**
- Find `Whitespace\"\\n\"` followed by non-space and insert spaces
- Problem: Doesn't know the correct indent level - would need to parse structure

**Option 2: ReasonML script (recommended)**
- Write a script that:
  1. Loads each ML file
  2. Extracts `backup_text`
  3. Parses with `Parser.to_zipper` (gets structure without indentation)
  4. Applies `Format` action (adds correct indentation)
  5. Re-serializes to new ML file
- Uses actual Hazel logic, guarantees correct indentation

**Option 3: On-load migration**
- Detect old format (no spaces after linebreaks) when loading
- Auto-apply Format before displaying
- Problem: Modifies in-memory only, doesn't update the files

**Option 4: Parser with auto_indent=true**
- Parse backup_text with a modified Parser that has auto_indent=true
- Would add indentation during parsing
- Problem: Only works for files where backup_text has no indentation; doubles indentation if any exists

### Recommended Approach

Create a migration script in ReasonML that:
```reason
(* pseudocode *)
let migrate_slide(path: string) = {
  let content = read_file(path);
  let (title, persisted) = parse_ml_structure(content);

  /* Parse backup_text to get zipper */
  switch (Parser.to_zipper(persisted.backup_text)) {
  | None => error("Failed to parse")
  | Some(z) =>
    /* Apply Format to add indentation */
    let z' = Perform.go(Format, z);

    /* Re-persist */
    let persisted' = PersistentSegment.persist(z');
    write_ml_file(path, title, persisted');
  };
};
```

This could be a standalone executable or integrated into the build/test process.

---

## Success Criteria

1. All existing tests pass
2. New unit tests for completion scenarios
3. `Dump.re` no longer needed (or greatly simplified)
4. Round-trip tests: segment → term → segment preserves structure
5. Incomplete syntax deep in a tree doesn't break refactoring operations
6. Doc slides migrated to new indentation format
