# Remote Caret Shard Index Fix

## Overview

This document describes the problem with remote caret positioning for multi-shard tiles and the fix involving `shardIndex`.

---

## Part 1: Understanding the Data Structures

### Tile Structure

A tile in Hazel can have multiple "shards" (delimiters) with child segments between them:

```reason
type tile = {
  id: Id.t,
  label: Label.t,        // e.g., ["let", "=", "in"]
  mold: Mold.t,
  shards: list(int),     // which shard indices are present
  children: list(segment), // child segments between shards
};
```

**Example:** `let x = 1 in body`
- `label = ["let", "=", "in"]`
- `shards = [0, 1, 2]` (complete tile has all shards)
- `children = [x_segment, 1_segment]` (two child segments)

**Invariant:** `length(children) == length(shards) - 1`

### Tile Fragmentation

When navigating through a tile, it can be "popped" into single-shard fragments:

```reason
// From Tile.re
let shard_of = (t: t, i: int): t => {
  ...t,
  shards: [i],
  children: [],
};
```

So a fragment of the "in" shard would be:
- `shards = [2]` (just index 2)
- `children = []` (no children)

### Zipper Structure

The zipper represents the caret position:

```reason
type t = {
  selection: Selection.t,
  relatives: Relatives.t,  // siblings + ancestors
  caret: Outer | Inner(int),
  refractors: Refractor.t,
};
```

**Siblings** (`Siblings.t = (Segment.t, Segment.t)`):
- Left segment: pieces to the left of caret (last element is immediately left)
- Right segment: pieces to the right of caret (first element is immediately right)

**Ancestors** (when inside a tile's child):
```reason
type Ancestor.t = {
  id: Id.t,
  label: Label.t,
  mold: Mold.t,
  shards: (list(int), list(int)),    // (before_focal, after_focal)
  children: (list(Segment.t), list(Segment.t)), // (before_focal, after_focal)
};
```

### Measured Structure

The layout system stores shard positions separately:

```reason
type Measured.t = {
  tiles: Id.Map.t(Shards.t),  // tile_id -> list of (shard_index, measurement)
  grout: Id.Map.t(measurement),
  secondary: Id.Map.t(measurement),
  projectors: Id.Map.t(measurement),
  ...
};
```

**Critical:** For a tile ID "abc" with 3 shards, `measured.tiles["abc"]` contains:
```
[(0, m_let), (1, m_eq), (2, m_in)]
```

But `Measured.find_by_id("abc")` returns the **combined span**:
```
{ origin: m_let.origin, last: m_in.last }
```

---

## Part 2: How Caret Position is Determined

### The Problem with Naive Sibling Lookup

The **old** approach simply looked at siblings:

```reason
// OLD CODE - HAD A BUG
let (piece_opt, side) =
  switch (z.relatives.siblings, z.caret) {
  | ((_, [piece, ..._]), Outer) => (Some(piece), Some(Left))   // Normal: piece to right
  | ((_, [piece, ..._]), Inner(_)) => (Some(piece), None)      // Inside piece to right
  | (([_, ..._] as left, []), Outer) => (Some(last(left)), Some(Right))  // End of segment
  | (([_, ..._] as left, []), Inner(_)) => (Some(last(left)), None)      // Inside last piece
  | _ => (None, None)
  };
```

**The Bug:** If the first right sibling is a Secondary (whitespace), we'd send that piece's ID. But:
1. Secondary pieces don't have shards, so `shard_index = None`
2. The whitespace measurement is different from the actual code piece's measurement
3. Remote caret renders at the wrong position

### The `Indicated` Module Solution

`Indicated.for_index(z)` properly identifies the piece we're "on" by:
1. **Skipping Secondary pieces (whitespace)** - won't return whitespace as the indicated piece
2. **Handling Parent relations** - when at child segment boundaries, can return the parent tile

```reason
type relation = Parent | Sibling;
type piece = (Piece.t, Direction.t, relation);
```

The key insight from `Indicated.for_index`:
- Line 47: `| ((_, Some(r)), _) => Some((r, Right, Sibling))` - if R is not secondary, return it
- Line 26-27: `| ((Some(l), Some(r)), _) when !ign(l) && ign(r) => Some((l, Left, Sibling))` - skip secondary R

### Current `get_caret_position` Logic (Fixed)

```reason
switch (Indicated.for_index(z)) {
| None => None
| Some((piece, direction, _relation)) =>
  let piece_id = Piece.id(piece);
  // Extract shard index from the piece's own data (not Indicated.shard_index!)
  let shard_index = switch (piece) {
    | Tile(t) =>
      switch (t.shards) {
      | [i] => Some(i)  // Fragment - use its shard index
      | _ => None       // Complete tile - shard_index not needed
      }
    | _ => None
  };
  ...
```

**Important:** We use `Indicated.for_index` to get the correct piece, but we extract `shard_index` from the piece's own `t.shards` field, NOT from `Indicated.shard_index`. The `Indicated.shard_index` function computes which shard is "indicated" for UI purposes, which is different from the physical shard index in a tile fragment.

---

## Part 3: Worked Examples

### Example 1: Caret Adjacent to Single-Shard Tile (Works Correctly)

**Code:** `a + b` with caret right before `+`

**Visual:** `a |+ b` (caret shown as `|`)

**Zipper State:**
```
siblings: ([a, whitespace], [+_tile, whitespace, b])
caret: Outer
```

The `+` tile is a sibling with:
```
{ id: "xyz", label: ["+"], shards: [0], children: [] }
```

**What we send:**
- `piece_id = "xyz"`
- `shard_index = Some(0)` ← from `t.shards = [0]`
- `caret_offset = 0`
- `side = Some(Left)`

**What receiver does:**
- Look up `measured.tiles["xyz"]` → `[(0, m_plus)]`
- Find shard 0 → `m_plus`
- Position = `m_plus.origin`

**Result:** Correct! ✓

---

### Example 2: Caret at Child Segment Boundary (Currently Works)

**Code:** `let x = 1 in body` with caret between `1` and `in`

**Visual:** `let x = 1 |in body`

**Zipper State:**
```
siblings: ([..., 1_tile], [])  // empty right siblings!
caret: Outer
ancestors: [{
  id: "abc",
  label: ["let", "=", "in"],
  shards: ([0, 1], [2]),     // shards 0,1 before; shard 2 after
  children: ([x_seg], []),   // x_seg before; none after
}, ...]
```

**What `get_caret_position` returns:**
- `piece = 1_tile` (last of left siblings)
- `side = Some(Right)` (because right siblings empty)

**What we send:**
- `piece_id = "1_tile_id"`
- `shard_index = Some(0)` (assuming 1 is a single-shard number tile)
- `caret_offset = 0`
- `side = Some(Right)`

**What receiver does:**
- Look up `measured.tiles["1_tile_id"]`
- Position = `measurement.last` (because side=Right)

**Result:** Correct! The right edge of "1" is visually adjacent to "in". ✓

**Note:** We're NOT using the let tile's measurement here - we're using "1"'s measurement. The `side=Right` tells us to use `measurement.last`.

---

### Example 3: Caret Inside Multi-Shard Tile Fragment (THE BUG)

**Code:** `let x = 1 in body` with caret navigating through "in"

**Visual:** `let x = 1 i|n body` (caret inside "in" delimiter)

**Zipper State after navigating into "in":**

When you navigate character-by-character through "in", the tile gets fragmented. The zipper becomes:
```
siblings: ([..., 1_tile, whitespace, in_fragment], [...])
   where in_fragment = { id: "abc", shards: [2], children: [] }
caret: Inner(0)  // after "i", before "n"
```

**What we currently send:**
- `piece_id = "abc"` (the let tile's ID!)
- `caret_offset = 1` (Inner(0) → offset 1)
- `side = None`
- **shard_index = ???** (this is what's missing)

**What receiver currently does:**
- `Measured.find_by_id("abc")` returns combined span of ALL shards
- `origin = m_let.origin` (where "let" is on line 0)
- Position = `origin + offset` = line 0, column 1

**Result:** WRONG! Caret renders on line 0 inside "let" instead of on line 2 inside "in". ✗

**The Fix:**
- Send `shard_index = Some(2)` (from `t.shards = [2]`)
- Receiver looks up `measured.tiles["abc"]` → `[(0, m_let), (1, m_eq), (2, m_in)]`
- Find shard 2 → `m_in`
- Position = `m_in.origin + offset`

**Result:** Correct! ✓

---

### Example 4: The Whitespace Bug (FIXED)

**Code:** `if x then y else z` with caret inside "then" at Inner(0)

**Visual:** `if x t|hen y else z` (caret after 't', before 'hen')

**Zipper State:**
```
siblings: ([whitespace], [then_fragment, whitespace, y, ...])
   where then_fragment = { id: "abc", shards: [1], children: [] }
caret: Inner(0)
```

**What old code did (BUG):**
- Take first right sibling: `[whitespace, ...]` → gets the whitespace!
- `piece = whitespace` (a Secondary piece)
- `shard_index = None` (Secondary pieces have no shards)
- Send piece_id of the whitespace, not the "then" shard

**What receiver saw:**
- Look up whitespace piece → origin at column 7 (before "then")
- Position = `(0, 7) + offset 1` = `(0, 8)`
- But "then" starts at column 8, so caret appears at START of "then"
- Expected: caret INSIDE "then" at column 9

**What new code does (FIXED):**
- Use `Indicated.for_index(z)` which skips Secondary pieces
- Returns `then_fragment` as the piece
- `shard_index = Some(1)` from `t.shards = [1]`
- Send correct piece_id with correct shard_index

**What receiver now sees:**
- Look up shard 1 of tile "abc" → "then" measurement
- Position = `then.origin + offset 1`
- Correct!

---

### Example 5: Caret at Left Edge of Fragment (Outer)

**Code:** `let x = 1 in body` with caret right before "in"

**Visual:** `let x = 1 |in body` (but navigated TO the "in" shard specifically)

If we've navigated such that the "in" fragment is the first right sibling:
```
siblings: ([..., 1_tile, whitespace], [in_fragment, whitespace, body])
   where in_fragment = { id: "abc", shards: [2], children: [] }
caret: Outer
```

**What we send:**
- `piece_id = "abc"`
- `shard_index = Some(2)`
- `caret_offset = 0`
- `side = Some(Left)`

**What receiver does:**
- Look up shard 2 specifically → `m_in`
- Position = `m_in.origin`

**Result:** Correct! ✓

---

## Part 4: When Do Tile Fragments Appear as Siblings?

### The `Siblings.pop` Mechanism

When navigating token-by-token through a tile, `Siblings.pop` is called:

```reason
// Siblings.re
let pop = (from: Direction.t, (pre, suf): t): option((Piece.t, t)) =>
  switch (from) {
  | Left => ... Piece.pop_r(p) ...  // pops rightmost shard
  | Right => ... Piece.pop_l(p) ... // pops leftmost shard
  };
```

For tiles, `Piece.pop_l/pop_r` calls `Tile.pop_l/pop_r`:

```reason
// Tile.re
let pop_l = (tile: t): (piece, segment) =>
  disassemble(tile) |> split_first_opt ...;
```

`disassemble` converts a tile into interleaved shards and children:
```
let x = 1 in body  →  [let_shard, x_seg, eq_shard, 1_seg, in_shard]
```

So popping gives you a single-shard fragment.

### When Fragments Become Siblings

1. **Direct navigation:** Moving token-by-token across a delimiter
2. **After unzipping:** The tile's children become the focal segment, adjacent shards are in siblings
3. **During editing:** Cutting/pasting can leave fragments

---

## Part 5: The Complete Information Model

### Sending Side: What We Extract from Zipper

| Field | Source | Description |
|-------|--------|-------------|
| `piece_id` | `Piece.id(piece)` from siblings | ID of the piece |
| `shard_index` | `t.shards` if tile with single shard | Which shard (null for non-tiles) |
| `caret_offset` | `z.caret`: 0 for Outer, n+1 for Inner(n) | Column offset |
| `side` | Empty right siblings → Right, else Left | Which edge of piece |
| `shape` | `Zipper.Caret.direction(z)` | Visual caret shape |

### Receiving Side: How to Compute Position

```reason
let compute_position = (piece_id, shard_index, caret_offset, side, measured) => {
  // Step 1: Find the measurement
  let measurement = switch (shard_index) {
    | None => Measured.find_by_id(piece_id, measured)
    | Some(idx) =>
      switch (Id.Map.find_opt(piece_id, measured.tiles)) {
      | Some(shards) => ListUtil.assoc_opt(idx, shards)
      | None => Measured.find_by_id(piece_id, measured)  // fallback
      }
  };

  // Step 2: Compute position based on side
  switch (side, measurement) {
  | (Some(Right), Some(m)) => m.last  // right edge of piece
  | (_, Some(m)) => Point.{ row: m.origin.row, col: m.origin.col + caret_offset }
  | _ => Point.zero  // fallback
  };
};
```

---

## Part 6: Edge Cases and Potential Issues

### Case A: Complete Tiles as Siblings

If a multi-shard tile is a sibling (not fragmented), e.g., navigating past `[1, 2, 3]`:
- `t.shards = [0, 1, 2, 3]` (all shards present)
- Current code: `t.shards = [i]` pattern won't match, `shard_index = None`

**Is this a problem?**
- If `side = Some(Left)`: caret is at left edge, use first shard's origin → matches `find_by_id`'s `origin`
- If `side = Some(Right)`: caret is at right edge, use last shard's last → matches `find_by_id`'s `last`
- If `side = None` (Inner): You're inside... but which shard?

**Potential Issue:** If you can be at an Inner position within a complete multi-shard tile (unlikely?), we'd need to know which shard.

### Case B: The Indicated vs get_caret_position Divergence

When at a child segment boundary:
- `Indicated.piece(z)` returns `(parent_tile, direction, Parent)`
- `get_caret_position` returns `(sibling_piece, Right)`

These give different pieces! But both approaches work because:
- `get_caret_position` + `side=Right` → use sibling's `.last`
- `Indicated` + `shard_index` → use parent shard's position

The sibling's `.last` should be adjacent to the parent's delimiter.

### Case C: Whitespace Between Sibling and Delimiter

If there's whitespace between "1" and "in":
```
siblings: ([..., 1_tile, whitespace], [])
```

At end of segment, we get `piece = whitespace`, not `1_tile`!

**Question:** Does this cause issues? The whitespace measurement's `.last` should still be adjacent to "in".

### Case D: Inner Position in Whitespace

If `caret = Inner(n)` in whitespace:
- `shard_index = None` (whitespace isn't a tile)
- Position = `whitespace.origin + offset`

This should work correctly.

---

## Part 7: Implementation Plan

### Files to Modify

1. **`embed/src/types/patchworkmessages.d.ts`** - Add `shardIndex: number | null`
2. **`src/haz3lcore/patchwork/SyncReplace.re`** - Extract shard index from tile
3. **`src/haz3lcore/patchwork/PatchworkComm.re`** - Add to types and wire protocol
4. **`src/web/app/editors/decoration/RemoteCaretDec.re`** - Use shard-specific lookup
5. **`patchwork-extra/hazel tool.tsx`** - Forward shardIndex

### Implementation Details

#### SyncReplace.re Changes

```reason
let get_caret_position = (z: Zipper.t)
    : option((Id.t, option(int), int, option(Direction.t), option(Direction.t))) => {
  let (piece_opt, side) = /* existing logic */;
  switch (piece_opt) {
  | Some(piece) =>
    let piece_id = Piece.id(piece);
    let shard_index = switch (piece) {
      | Tile(t) =>
        switch (t.shards) {
        | [i] => Some(i)
        | _ => None  // Complete tile or empty - use combined measurement
        }
      | _ => None
    };
    let caret_offset = switch (z.caret) { | Outer => 0 | Inner(n) => n + 1 };
    let shape = switch (z.caret) { | Inner(_) => None | Outer => Zipper.Caret.direction(z) };
    Some((piece_id, shard_index, caret_offset, shape, side));
  | None => None
  };
};
```

#### RemoteCaretDec.re Changes

```reason
let find_shard_measurement = (piece_id, shard_index, measured) =>
  switch (shard_index) {
  | None => Measured.find_by_id(piece_id, measured)
  | Some(idx) =>
    switch (Id.Map.find_opt(piece_id, measured.tiles)) {
    | Some(shards) =>
      switch (ListUtil.assoc_opt(idx, shards)) {
      | Some(m) => Some(m)
      | None => Measured.find_by_id(piece_id, measured)
      }
    | None => Measured.find_by_id(piece_id, measured)
    }
  };
```

---

## Part 8: Testing Strategy

### Test Cases

1. **Single-shard tiles:** `a + b` - verify caret on `+` works
2. **Multi-shard, at boundary:** `let x = 1 in body` - caret between `1` and `in`
3. **Multi-shard, inside shard:** `let x = 1 in body` - caret inside `in` (if navigable)
4. **Multi-line:** Same tests but with line breaks between shards
5. **Nested:** `let x = if a then b else c in body` - carets at various positions
6. **With whitespace:** Ensure whitespace between shards doesn't break positioning

### Debugging Tips

Add logging to verify what's being sent:
```reason
Firebug.console##log(Js.string(
  "[CARET] piece_id=" ++ Id.to_string(piece_id) ++
  " shard_index=" ++ (switch(shard_index) { | Some(i) => string_of_int(i) | None => "None" }) ++
  " offset=" ++ string_of_int(caret_offset)
));
```

---

## Part 9: Open Questions

1. **Can you have Inner position in complete multi-shard tile?** If so, which shard?
2. **What about projectors?** They're not tiles - do they need special handling?
3. **Selection ranges:** Does this fix also apply to selection end positions?
4. **Performance:** Is the shard lookup (`ListUtil.assoc_opt`) fast enough?

---

## Part 10: Implementation Status

### Completed (in Hazel repo)

1. **TypeScript types** (`embed/src/types/patchworkmessages.d.ts`):
   - Added `shardIdx: number | null` to `CaretUpdate` and `RemoteCaret`
   - Note: Named `shardIdx` instead of `shardIndex` to avoid sed script mangling

2. **OCaml types** (`src/haz3lcore/patchwork/PatchworkMessages.mli`):
   - Regenerated with `pnpm type:patchworkmessages`

3. **SyncReplace.re** (UPDATED):
   - **Fix:** Now uses `Indicated.for_index(z)` to identify the piece we're "on"
   - This properly skips Secondary (whitespace) pieces that were causing the bug
   - Extracts shard index from the piece's `t.shards` field (NOT `Indicated.shard_index`)
   - Updated `send_caret` to pass shard_index with debug logging

4. **PatchworkComm.re**:
   - Added `shard_index: option(int)` to `remote_caret` type
   - Updated `send_caret` to include shardIdx in wire protocol
   - Updated listener to extract shardIdx from received messages

5. **RemoteCaretDec.re**:
   - Added `find_shard_measurement` helper for shard-specific lookup
   - Updated `view` to accept `~shard_index` parameter
   - Updated `view_all` to pass shard_index from remote_caret
   - Added debug logging for troubleshooting

### Completed (in patchwork-extra/hazel repo)

The `tool.tsx` file has been updated:

1. Interfaces include `shardIdx` and `side`:
```typescript
interface CaretPosition {
  pieceId: string;
  shardIdx: number | null;
  offset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}
```

2. `broadcastCaret` includes `shardIdx` and `side`

3. Ephemeral message handler extracts and stores `shardIdx` and `side`

4. Caret message handler passes `shardIdx` and `side` to broadcast

5. Remote caret forwarding includes `shardIdx` and `side`

### Bug Fix History

**Initial Problem:**
- Remote carets rendered at wrong positions for multi-shard tiles (let/in, if/then/else)
- The shard index wasn't being sent over the wire

**First Fix Attempt:**
- Added `shardIdx` to the protocol
- Extracted shard index from `t.shards = [i]` in fragmented tiles
- This worked for most cases but not for Inner positions with preceding whitespace

**Second Bug (Whitespace Issue):**
- When at `Inner(0)` inside a shard, if there's whitespace before the shard in siblings,
  the old code would take the whitespace piece instead of the actual code piece
- `shard_index` would be `None` because Secondary pieces don't have shards
- Position would be wrong because whitespace measurement doesn't match the shard measurement

**Final Fix:**
- Use `Indicated.for_index(z)` which properly skips Secondary pieces
- This returns the actual code piece the caret is "on"
- Then extract `shard_index` from that piece's `t.shards` field

### Build and Deploy

```bash
# Hazel repo
dune build

# patchwork-extra/hazel repo
cd /path/to/patchwork-extra/hazel
npx pnpm@9 build
patchwork push
```

---

## Changelog

- Initial version: Documented the problem and basic fix
- Updated: Added comprehensive data structure explanation, worked examples, edge cases
- Implemented: Added shardIdx to Hazel repo (TypeScript types, OCaml code)
- Implemented: Updated patchwork-extra/hazel tool.tsx to forward shardIdx
- Bug fix: Fixed whitespace issue by using `Indicated.for_index` instead of naive sibling lookup
