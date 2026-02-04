# Completion Visualization

This document plans the UI for visualizing canonical completion - showing users what delimiters will be inserted to complete their incomplete syntax.

## Design Goals

1. **Lightweight inline markers**: Use middle dots (`·`) to show WHERE completions will be inserted, without actually taking up space in the editor
2. **Offside display**: Show WHAT will be inserted in comments/annotations at the end of each line
3. **Non-intrusive**: Don't interfere with caret movement or editing flow

## Text Mockup Format

For testing and development, we use a text-based mockup format:

```
let x = 1·    // in ?
```

- `·` (middle dot, U+00B7) marks the insertion point
- `// ...` shows the completion text, 4 spaces after end of code
- One dot per insertion point (even if multiple completions go there)

### Examples

**Simple incomplete let:**
```
let x = 1·    // in ?
```

**Nested incomplete tiles (both complete at same position):**
```
let f = fun x·    // -> ? in ?
```

**Multi-line with column-0 partitioning:**
```
let x = 1·    // in
y
```

**Blank line partition (Option B - dot on blank line):**
```
let x = 1
·    // in
y
```

**Multiple lines with completions:**
```
let x = 1·    // in
let y = 2·    // in ?
```

## Visualization Options (for future reference)

### Blank line partition placement

**Option A: Dot at end of preceding line**
```
let x = 1·    // in

y
```
- Pro: Shows semantic association with the incomplete expression
- Pro: Keeps blank lines visually blank
- Con: Doesn't match where text actually appears

**Option B: Dot on the blank line** (current choice)
```
let x = 1
·    // in
y
```
- Pro: Accurately represents where delimiter will appear
- Pro: "Honest" about post-completion layout
- Con: Dot on otherwise blank line looks odd

### Multiple insertions on the same line

When there are multiple insertion points on the same line (e.g., nested incomplete
constructs inside complete parents), all completions are grouped into ONE offside
comment at the end of the line, ordered left-to-right:

```
(let x = 1·) + (let y = 2·    // in ? in ? )
```

- Two dots: after `1` (first let needs `in ?`) and after `2` (second let needs `in ?`, paren needs `)`)
- One offside: lists all three completions in order

This keeps the format simple. The dots show WHERE insertions happen; the offside
shows WHAT gets inserted (all of it, in reading order).

### Showing removed grout (future work)

When completion removes existing concave grout (`~`), we could show this:

**Text format options:**
- Diff-style: `// in ?  (-~)`
- Strikethrough: `// in ?  ~̶`

**GUI format options:**
- Strikethrough over actual grout in code
- Ghost/fade rendering of grout to be removed
- Color coding (additions vs removals)

Note: Delimiter insertion typically removes concave grout (shape mismatches) but not convex holes (semantic placeholders).

## Implementation Plan

### Phase 1: Delimiter-only visualization

**Goal:** Show what delimiters will be inserted and where.

**Data needed:**
1. For each incomplete tile: which shards are being added
2. The text of those shards (from `tile.label`)
3. The insertion position (end of last original shard)

**Current infrastructure:**
- `CanonicalCompletion.shard_record` tracks `tile_id` and `original_shards`
- After completion, we can compare to find added shards
- `Measured` provides position information for all pieces

**Actual implementation:**

1. **Extended `completion_result`** with insertion info:
   ```reason
   type delimiter_info = {
     text: string,       // The delimiter token (e.g., "in", "->", ")")
     needs_hole: bool,   // Whether a "?" follows this delimiter
   };

   type insertion = {
     adjacent_id: Id.t,              // ID of piece adjacent to insertion point
     side: Direction.t,              // Which side (Left or Right)
     delimiters: list(delimiter_info), // The delimiters with hole info
   };
   ```

2. **Insertions computed during completion**:
   - For each partition with incomplete tiles, record adjacent piece ID
   - Child insertions collected recursively and merged with top-level
   - Positions resolved later via `Measured.find_by_id`

3. **Text mockup in CompletionVisualization**:
   - Resolve positions from IDs using Measured
   - Group by row, insert dots at each position
   - Append offside comment with all completions for that line

### Phase 2: Include holes in visualization

**Goal:** Show not just delimiters but also the holes they introduce.

**Observation:** Inserted holes are always convex. Delimiters with concave right side (like `in`, `->`) get a convex hole; delimiters with convex right side (like `)`, `]`) don't need one.

**Data needed:**
- Which insertions result in holes
- Could derive from delimiter shape or track during regrouting

### Phase 3: GUI integration

**Goal:** Render visualization in the actual editor.

**Approach:**
- Dots rendered at between-character positions (like cursor)
- Offside display similar to refractor offside annotations
- No actual text insertion - purely visual overlay

### Phase 4: Show grout changes (optional)

**Goal:** Indicate when completion removes concave grout.

**Challenge:** Grout changes happen during regrouting, which operates on the whole segment. Would need to:
- Track grout IDs before completion
- Compare with grout IDs after completion
- Map removed grout to their original positions

## Test Cases

Based on existing `Test_CanonicalCompletion.re` cases, adapted for visualization:

```reason
// Simple trailing delimiter
("let x = 1", "let x = 1·    // in ?")

// No completion needed
("let x = 1 in x", "let x = 1 in x")

// Closing bracket (no hole)
("(1 + 2", "(1 + 2·    // )")

// Nested - multiple at same position
("let f = fun x", "let f = fun x·    // -> ? in ?")

// Column-0 partition
({|let a = 1
a|}, {|let a = 1·    // in
a|})

// Blank line partition
({|let x = 1

y|}, {|let x = 1
·    // in
y|})
```

## Current Implementation Status

### Completed (Phase 1 + Phase 2)

1. **Insertion tracking in CanonicalCompletion**:
   - `delimiter_info` type: delimiter text + whether it needs a hole
   - `insertion` type: `adjacent_id` + `side` + `delimiters` list
   - ID-based approach allows positions to be resolved later via Measured
   - `complete_segment_deep` collects child insertions recursively
   - Hole logic documented: all trailing delimiters have concave left,
     so they can never fill holes from preceding delimiters

2. **CompletionVisualization module**:
   - `mockup(seg)` generates text visualization
   - Resolves positions from IDs using Measured
   - Shows dots at insertion points
   - Shows offside comments with delimiter + hole info
   - Handles `has_following_content` to omit trailing holes
   - Multiple insertions on same line grouped into one offside comment

3. **Test coverage** (24 tests):
   - Simple cases (let, fun, parens, brackets, if, case)
   - Nested completions (multiple at same position)
   - Complex cases (multiple insertion points on same line)
   - Multi-line with column-0 and blank-line partitioning
   - Indented content with relative indent heuristic
   - Child insertions (incomplete syntax inside complete parents)

### Known Limitations

1. **Regrouting holes not shown**: Holes added by regrouting (e.g., the
   pattern hole in `let· // ? = ? in`) are not captured. The visualization
   shows `= ? in` instead of `? = ? in`.

2. **Grout changes not tracked**: We discussed showing removed grout,
   but this requires comparing before/after grout states.

## Open Questions

1. Should we show the `?` holes or just the delimiters?
   - Current: show holes for delimiter bodies, not regrouting holes
   - Could be extended to track regrouting changes

2. How to handle multiple completions at exactly the same position?
   - Current: one dot, space-separated text in offside
   - Working correctly

3. Line breaks inserted by completion (like blank line → `in` on its own line)?
   - Option B implemented: dot on the line where text will appear

4. Performance: recomputing insertions on every keystroke?
   - Insertions computed during completion pass
   - Could be optimized if needed

5. ~~Child insertions: How to capture completions in nested children?~~
   - **RESOLVED**: `complete_segment_deep` now collects and merges child insertions.
   - Insertions use `adjacent_id` + `side` instead of row/col, allowing positions
     to be looked up in Measured regardless of nesting level.
