# Automerge Granular Sync Design

This document describes the plan to improve Hazel's Automerge integration to support finer-grained CRDT operations, reducing clobbering during concurrent edits.

## Prerequisites

**Read first:** [patchwork-integration.md](./patchwork-integration.md) - explains the current iframe architecture, message protocol, and how Hazel syncs with Patchwork via Automerge.

**Repositories involved:**

| Repo | Path | Purpose |
|------|------|---------|
| Hazel | `.` (current directory) | OCaml editor, iframe side |
| patchwork-extra/hazel | `../patchwork-extra/hazel` | TypeScript tool.tsx, Automerge integration |

**Key files to modify:**

| File | Repo | Changes |
|------|------|---------|
| `src/haz3lcore/patchwork/PatchworkComm.re` | Hazel | Add `before` state to outgoing message |
| `embed/src/types/patchworkmessages.d.ts` | Hazel | Update EditorState type to include `before` |
| `src/tool.tsx` | patchwork-extra/hazel | **Main work:** implement granular ops, shard change detection, diff computation |

**Summary of work distribution:**
- **Hazel side:** Minimal changes - just include `before` pieces in the state message
- **patchwork-extra/hazel side:** Main implementation - granular Automerge operations

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Problem Statement](#problem-statement)
3. [Key Insight: The Shard Change Rule](#key-insight-the-shard-change-rule)
4. [Automerge CRDT Background](#automerge-crdt-background)
5. [Current Architecture](#current-architecture)
6. [Hazel Data Structures](#hazel-data-structures)
7. [Clobbering Scenarios](#clobbering-scenarios)
8. [Proposed Solution](#proposed-solution)
9. [Concrete Examples](#concrete-examples)
10. [Implementation Plan](#implementation-plan)
11. [Future Work](#future-work)
12. [Appendix: Design Decision Rationale](#appendix-design-decision-rationale)

---

## Problem Statement

Hazel's current Automerge integration replaces entire pieces (tiles, grout, secondary) atomically. This causes concurrent edits to clobber each other even when they're logically independent:

- Two users editing different children of the same `let` tile → clobbered
- Two users inserting tokens at different positions in the same segment → clobbered
- User editing pattern while another edits definition → clobbered

The root cause: we're doing `d.pieces[id] = entireNewPiece` instead of using Automerge's granular list operations.

---

## Key Insight: The Shard Change Rule

**The fundamental rule governing this design:**

> If the `shards` field changes for any tile in the delta, fall back to atomic replacement for that tile and its parent. Otherwise, use granular CRDT operations on segment arrays.

**Why this rule exists:**

1. **Shard changes = restructuring.** When `shards` changes, delimiters are being added or removed. This means pieces move between segments (from parent into tile's children, or vice versa).

2. **Restructuring is inherently coupled.** A restructuring operation affects multiple fields (shards, children) and multiple tiles (the restructured tile and its parent) simultaneously. These changes must come from the same source to maintain invariants.

3. **Grout complexity.** Restructuring may trigger automatic grout insertion/removal, adding pieces that weren't explicitly part of the user's edit.

4. **Text-like operations.** Restructuring (adding/removing delimiters) is closer to "text editing" than "structured editing." It's reasonable that concurrent restructuring in the same region requires more coordination.

**What this means in practice:**

| Operation Type | Shard Change? | Merge Behavior |
|----------------|---------------|----------------|
| Edit atomic token (`foo` → `bar`) | No | Granular (RGA) |
| Insert new token in segment | No | Granular (RGA) |
| Delete token from segment | No | Granular (RGA) |
| Edit different children of same tile | No | Granular (RGA) |
| Add/remove delimiter (parens, let, etc.) | **Yes** | **Atomic (LWW)** |

**The acceptable tradeoff:**

Concurrent edits within stable structure merge cleanly. Concurrent edits where one user is restructuring may clobber. This is acceptable because:
- Restructuring is less common than content editing
- Restructuring affects segment boundaries, so concurrent edits in the same region are inherently in tension
- Users doing restructuring operations likely want "elbow room" anyway

See [Appendix: Design Decision Rationale](#appendix-design-decision-rationale) for detailed scenarios that motivated this rule.

---

## Automerge CRDT Background

### Data Types and Merge Semantics

| Type | Concurrent Behavior | Algorithm |
|------|---------------------|-----------|
| **Map** | Different keys merge cleanly; same key → last-writer-wins on value | - |
| **Array/List** | Concurrent insertions/deletions merge cleanly | RGA (Replicated Growable Array) |
| **Text** | Character-level concurrent edits merge | Peritext CRDT |
| **Scalar** (string, number, bool) | Last-writer-wins | - |

### RGA Algorithm (How Lists Work)

RGA assigns each element a unique operation ID (Lamport timestamp + actor ID). Insertions are tracked as "insert X after element Y" rather than "insert X at index N".

```
Initial: [a, b, c]  (each has unique OpId)

User A: insertAt(1, "x")  → "insert x after a"
User B: insertAt(2, "y")  → "insert y after b"

Merged: [a, x, b, y, c]  (both insertions preserved)
```

Concurrent insertions at the same position are ordered deterministically by OpId.

### Nested Structures

Automerge tracks paths through nested structures. You can do:

```javascript
handle.change(d => {
  d.pieces["uuid-123"].children[0].insertAt(2, "uuid-new");
});
```

This applies an RGA insertion to a specific nested array, not a wholesale replacement.

### Key Insight

**Currently**: `d.pieces[id] = newPiece` replaces the whole piece (map value replacement → LWW)

**Goal**: Apply granular operations to nested arrays within pieces, triggering RGA merge semantics.

---

## Current Architecture

### Data Flow: Local Edit → Sync

```
1. User types in Hazel iframe
2. SyncReplace.send_state() called after action
3. FlatConvert.seg_to_doc() converts Segment → flat Doc (both old and new)
4. PatchworkComm.compute_delta() finds changed, added, deleted pieces
5. Delta sent via PostMessage: { state: affectedPieces, deleted: [...] }
6. tool.tsx receives delta
7. tool.tsx applies to Automerge: d.pieces[id] = piece (WHOLESALE REPLACEMENT)
8. Automerge syncs to other clients
```

### Data Flow: Remote Edit → Apply

```
1. Automerge receives update from another client
2. tool.tsx useEffect[doc] detects change
3. tool.tsx computes delta from previous doc state
4. Sends affected pieces to iframe via PostMessage
5. PatchworkComm receives delta
6. SyncReplace.sync_replace() merges delta with current state
7. FlatConvert.doc_to_seg() converts Doc → Segment
8. Editor re-renders with preserved caret
```

### The Problem Point

Step 7 in the send flow: `d.pieces[id] = piece`

This is a map value replacement, which is last-writer-wins in Automerge. Even though pieces contain arrays (children), we're not using Automerge's array operations.

---

## Hazel Data Structures

### Tile (OCaml - Base.re)

```reason
type tile = {
  id: Id.t,
  label: Label.t,         // list(string), e.g., ["let"; "="; "in"]
  mold: Mold.t,
  shards: list(int),      // which label indices are present, e.g., [0, 1, 2]
  children: list(segment) // list of child segments
}
```

**Invariants:**
- `length(mold.in_) + 1 == length(label)`
- `length(shards) <= length(label)`
- `length(shards) == length(children) + 1`

### FlatTile (TypeScript - flatdoc.d.ts)

```typescript
interface FlatTile {
  readonly t: "Tile";
  readonly id: UUID;
  readonly label: string[];
  readonly mold: Mold;
  readonly shards: number[];
  readonly children: UUID[][];  // list of lists of piece UUIDs
}
```

### Example: `let x = 1 + 2 in x + 1`

```javascript
pieces = {
  "uuid-root": {
    t: "Tile",
    label: ["(", ")"],
    shards: [0, 1],
    children: [["uuid-let", "uuid-x2", "uuid-plus2", "uuid-one2"]]
  },
  "uuid-let": {
    t: "Tile",
    label: ["let", "=", "in"],
    shards: [0, 1, 2],
    children: [
      ["uuid-x"],           // pattern: x
      ["uuid-1", "uuid-plus", "uuid-2"]  // definition: 1 + 2
    ]
  },
  "uuid-x": { t: "Tile", label: ["x"], shards: [0], children: [] },
  "uuid-1": { t: "Tile", label: ["1"], shards: [0], children: [] },
  "uuid-plus": { t: "Tile", label: ["+"], shards: [0], children: [] },
  "uuid-2": { t: "Tile", label: ["2"], shards: [0], children: [] },
  // ... similar for x2, plus2, one2
}
```

### Key Observation

The `children` field is `UUID[][]` - a list of lists. Each inner list is a segment.

- Outer list: fixed size based on tile structure (e.g., let has 2 children slots)
- Inner lists: variable size, containing piece UUIDs

**Concurrent edits to different inner lists should merge cleanly** if we use Automerge's array operations.

---

## Clobbering Scenarios

### Scenario 1: Same Tile, Different Children

**Setup:** `let x = 1 in x`

```javascript
"uuid-let": {
  children: [
    ["uuid-x"],      // child 0: pattern
    ["uuid-1"]       // child 1: definition
  ]
}
```

**User A:** Changes pattern `x` to `x, y` (adds comma and y)
```javascript
children[0]: ["uuid-x"] → ["uuid-x", "uuid-comma", "uuid-y"]
```

**User B:** Changes definition `1` to `1 + 2` (adds plus and 2)
```javascript
children[1]: ["uuid-1"] → ["uuid-1", "uuid-plus", "uuid-2"]
```

**Current behavior:** Both users replace the entire `uuid-let` tile. Last writer wins. One edit lost.

**Desired behavior:** `children[0]` and `children[1]` are independent arrays. Both edits preserved.

### Scenario 2: Same Segment, Different Positions

**Setup:** Segment contains `[a, b, c]`

**User A:** Inserts `x` at beginning → `[x, a, b, c]`
**User B:** Inserts `y` at end → `[a, b, c, y]`

**Current behavior:** Parent tile replaced. One edit lost.

**Desired behavior:** RGA merges insertions → `[x, a, b, c, y]`

### Scenario 3: Same Segment, Same Position

**Setup:** Segment contains `[a, b, c]`

**User A:** Inserts `x` after `b`
**User B:** Inserts `y` after `b`

**Desired behavior:** Both inserted, deterministic order → `[a, b, x, y, c]` or `[a, b, y, x, c]`

### Scenario 4: Restructuring (Parentheses)

**Setup:** `( a b c` with incomplete parens

```javascript
parent.children[0]: ["uuid-parens", "uuid-a", "uuid-b", "uuid-c"]
"uuid-parens": { shards: [0], children: [[]] }  // only opening paren
```

**User completes parens after `b`:**

```javascript
parent.children[0]: ["uuid-parens", "uuid-c"]
"uuid-parens": { shards: [0, 1], children: [["uuid-a", "uuid-b"]] }
```

**Operations needed:**
1. Delete uuid-a, uuid-b from parent segment
2. Insert uuid-a, uuid-b into parens child segment
3. Update parens shards: [0] → [0, 1]

All in one Automerge change (atomic).

### Scenario 5: Concurrent Restructuring (Edge Case)

**User A:** Completes parens after `b` (moves a, b inside)
**User B:** Inserts `x` after `b` (in parent segment)

**Result:**
- a, b move inside parens (User A)
- x inserted "after b" but b is gone from parent
- x ends up in parent segment at b's former position

**Outcome:** Consistent but potentially surprising. User B's x is in parent, not inside parens with b. This is a semantic conflict the CRDT can't resolve - acceptable for now.

---

## Proposed Solution

### Architecture Change

**Current:** Hazel sends piece deltas → tool.tsx does `d.pieces[id] = piece`

**Proposed:** Hazel sends piece deltas → tool.tsx checks for shard changes → if restructuring, atomic replacement; otherwise, granular ops

### The Shard Change Check

Before applying any changes, tool.tsx scans for shard changes:

```typescript
function hasShardChanges(before: PiecesMap, after: PiecesMap, changedIds: string[]): Set<string> {
  const atomicIds = new Set<string>();

  for (const id of changedIds) {
    const b = before[id];
    const a = after[id];
    if (b?.t === "Tile" && a?.t === "Tile" && !arraysEqual(b.shards, a.shards)) {
      // This tile's shards changed - mark for atomic replacement
      atomicIds.add(id);
      // Also mark its parent for atomic replacement
      const parentId = findParent(after, id);
      if (parentId) atomicIds.add(parentId);
    }
  }

  return atomicIds;
}

function findParent(pieces: PiecesMap, childId: string): string | null {
  for (const [id, piece] of Object.entries(pieces)) {
    if (piece.t === "Tile") {
      for (const segment of piece.children) {
        if (segment.includes(childId)) {
          return id;
        }
      }
    }
  }
  return null;
}
```

### Sender Side (tool.tsx)

```typescript
function applyDelta(
  handle: DocHandle,
  before: PiecesMap,
  after: PiecesMap,
  changedIds: string[],
  deletedIds: string[]
) {
  // First, identify tiles that need atomic replacement due to shard changes
  const atomicIds = hasShardChanges(before, after, changedIds);

  handle.change(d => {
    // Handle deletions
    for (const id of deletedIds) {
      delete d.pieces[id];
    }

    // Handle changes
    for (const id of changedIds) {
      const beforePiece = before[id];
      const afterPiece = after[id];

      if (!beforePiece) {
        // New piece - add to map
        d.pieces[id] = afterPiece;
      } else if (atomicIds.has(id)) {
        // Shard change detected - atomic replacement
        d.pieces[id] = afterPiece;
      } else if (beforePiece.t === "Tile" && afterPiece.t === "Tile") {
        // Same structure - apply granular updates
        applyTileDiff(d.pieces[id], beforePiece, afterPiece);
      } else {
        // Non-tile - atomic replacement
        d.pieces[id] = afterPiece;
      }
    }
  });
}

function applyTileDiff(tile: AutomergeTile, before: FlatTile, after: FlatTile) {
  // Update scalar fields if changed (LWW is fine for these)
  if (!arraysEqual(before.label, after.label)) {
    tile.label = after.label;
  }
  if (!arraysEqual(before.shards, after.shards)) {
    tile.shards = after.shards;
  }
  if (!moldsEqual(before.mold, after.mold)) {
    tile.mold = after.mold;
  }

  // Apply granular ops to each child segment
  for (let i = 0; i < after.children.length; i++) {
    const oldSeg = before.children[i] || [];
    const newSeg = after.children[i] || [];
    applySegmentDiff(tile.children[i], oldSeg, newSeg);
  }

  // Handle children array length changes (restructuring)
  while (tile.children.length > after.children.length) {
    tile.children.pop();
  }
  while (tile.children.length < after.children.length) {
    tile.children.push([]);
  }
}

function applySegmentDiff(segment: AutomergeList, before: UUID[], after: UUID[]) {
  const ops = computeListOps(before, after);

  // Apply deletions in reverse order (to preserve indices)
  for (const op of ops.filter(o => o.type === 'delete').reverse()) {
    segment.deleteAt(op.index);
  }

  // Apply insertions in forward order
  for (const op of ops.filter(o => o.type === 'insert')) {
    segment.insertAt(op.index, op.id);
  }
}

function computeListOps(before: UUID[], after: UUID[]): ListOp[] {
  const ops: ListOp[] = [];
  const beforeSet = new Set(before);
  const afterSet = new Set(after);
  const afterPositions = new Map(after.map((id, i) => [id, i]));

  // Find deletions
  before.forEach((id, i) => {
    if (!afterSet.has(id)) {
      ops.push({ type: 'delete', index: i });
    }
  });

  // Find insertions with their target positions
  after.forEach((id, i) => {
    if (!beforeSet.has(id)) {
      ops.push({ type: 'insert', index: i, id });
    }
  });

  return ops;
}
```

### Receiver Side (Hazel iframe)

**Minimal changes needed.** Automerge handles the merge; tool.tsx sends the resulting pieces to Hazel. The existing `SyncReplace.sync_replace` logic should work:

1. Receive delta (affected pieces)
2. Merge with current flat doc (delta overrides)
3. Convert to segment
4. Restore caret position

The key difference: the "delta" now reflects properly-merged state from Automerge's CRDT, not just last-writer-wins.

### Message Protocol

Current message:
```typescript
{ t: "state", state: HazelDoc, deleted?: string[] }
```

Updated to include before state for changed pieces:
```typescript
{
  t: "state",
  state: HazelDoc,           // after state (affected pieces)
  before: HazelDoc,          // before state (for diff computation)
  deleted?: string[]
}
```

**Why Hazel sends `before` (Option A):** Hazel already computes the flattened before state in `SyncReplace.send_state_delta`. Sending it ensures tool.tsx computes diffs against exactly what Hazel intended, avoiding any potential desync between Hazel's view and tool.tsx's cached Automerge state. The message size increase is minimal - only the changed pieces are included in `before`, not the whole document.

---

## Concrete Examples

### Example 1: Concurrent Segment Insertions

**Initial state:** `1 + 2`

```javascript
"uuid-root": {
  children: [["uuid-1", "uuid-plus", "uuid-2"]]
}
```

**User A action:** Add `0 + ` at beginning
**User B action:** Add ` + 3` at end

**User A's before/after:**
```javascript
before.children[0]: ["uuid-1", "uuid-plus", "uuid-2"]
after.children[0]:  ["uuid-0", "uuid-plus-a", "uuid-1", "uuid-plus", "uuid-2"]
```

**User A's ops (computed by tool.tsx):**
```javascript
segment.insertAt(0, "uuid-0");
segment.insertAt(1, "uuid-plus-a");
```

**User B's before/after:**
```javascript
before.children[0]: ["uuid-1", "uuid-plus", "uuid-2"]
after.children[0]:  ["uuid-1", "uuid-plus", "uuid-2", "uuid-plus-b", "uuid-3"]
```

**User B's ops:**
```javascript
segment.insertAt(3, "uuid-plus-b");
segment.insertAt(4, "uuid-3");
```

**Automerge merge result:**
```javascript
["uuid-0", "uuid-plus-a", "uuid-1", "uuid-plus", "uuid-2", "uuid-plus-b", "uuid-3"]
```

Renders as: `0 + 1 + 2 + 3` ✓

### Example 2: Pattern vs Definition Concurrent Edits

**Initial state:** `let x = 1 in x`

```javascript
"uuid-let": {
  label: ["let", "=", "in"],
  shards: [0, 1, 2],
  children: [
    ["uuid-x"],   // pattern
    ["uuid-1"]    // definition
  ]
}
```

**User A:** Changes pattern to `x, y`
```javascript
children[0]: ["uuid-x"] → ["uuid-x", "uuid-comma", "uuid-y"]
```

**User B:** Changes definition to `1 + 2`
```javascript
children[1]: ["uuid-1"] → ["uuid-1", "uuid-plus", "uuid-2"]
```

**User A's ops:**
```javascript
tile.children[0].insertAt(1, "uuid-comma");
tile.children[0].insertAt(2, "uuid-y");
```

**User B's ops:**
```javascript
tile.children[1].insertAt(1, "uuid-plus");
tile.children[1].insertAt(2, "uuid-2");
```

**Merged result:**
```javascript
children: [
  ["uuid-x", "uuid-comma", "uuid-y"],
  ["uuid-1", "uuid-plus", "uuid-2"]
]
```

Renders as: `let x, y = 1 + 2 in x` ✓

### Example 3: Token Split

**Initial state:** `foobar`

```javascript
"uuid-foobar": { t: "Tile", label: ["foobar"], ... }
parent.children[0]: ["uuid-foobar"]
```

**User presses space in middle:** Creates `foo bar`

```javascript
// uuid-foobar's label changes (ID preserved on left half)
"uuid-foobar": { label: ["foo"], ... }

// New pieces added
"uuid-space": { t: "Secondary", content: { t: "Whitespace", content: " " } }
"uuid-bar": { t: "Tile", label: ["bar"], ... }

// Parent segment updated
parent.children[0]: ["uuid-foobar", "uuid-space", "uuid-bar"]
```

**Ops:**
```javascript
// Piece modifications
d.pieces["uuid-foobar"].label = ["foo"];  // LWW on label

// New pieces
d.pieces["uuid-space"] = { ... };
d.pieces["uuid-bar"] = { ... };

// Segment insertion
parent.children[0].insertAt(1, "uuid-space");
parent.children[0].insertAt(2, "uuid-bar");
```

### Example 4: Completing Parentheses (Restructuring)

**Initial state:** `( a b c` with incomplete parens

```javascript
"uuid-parent": {
  children: [["uuid-parens", "uuid-a", "uuid-b", "uuid-c"]]
}
"uuid-parens": {
  label: ["(", ")"],
  shards: [0],        // only opening paren
  children: [[]]      // empty child
}
```

**User adds `)` after `b`:**

```javascript
"uuid-parent": {
  children: [["uuid-parens", "uuid-c"]]
}
"uuid-parens": {
  shards: [0, 1],                      // both parens now
  children: [["uuid-a", "uuid-b"]]     // a, b moved inside
}
```

**Ops:**
```javascript
// Parent segment: remove a, b
d.pieces["uuid-parent"].children[0].deleteAt(2);  // uuid-b
d.pieces["uuid-parent"].children[0].deleteAt(1);  // uuid-a

// Parens tile: update shards
d.pieces["uuid-parens"].shards = [0, 1];

// Parens child: add a, b
d.pieces["uuid-parens"].children[0].insertAt(0, "uuid-a");
d.pieces["uuid-parens"].children[0].insertAt(1, "uuid-b");
```

Renders as: `( a b ) c` ✓

---

## Implementation Plan

### Phase 1: Hazel Message Update (Minimal)

**Goal:** Include `before` state in messages from Hazel to tool.tsx.

**Files:** `PatchworkComm.re`, `patchworkmessages.d.ts`

**Tasks:**

1. **Update message type** in `embed/src/types/patchworkmessages.d.ts`:
   ```typescript
   interface EditorState {
     t: "state";
     state: HazelDoc;    // after state
     before: HazelDoc;   // before state for changed pieces
     deleted?: string[];
   }
   ```

2. **Update `send_state`** in `PatchworkComm.re` to include before pieces:
   - Already computes `old_doc` and `new_doc`
   - Extract before versions of changed pieces and include in message

### Phase 2: tool.tsx Granular Updates

**Goal:** Replace wholesale piece replacement with granular operations.

**Files:** `../patchwork-extra/hazel/src/tool.tsx`

**Tasks:**

1. **Implement `hasShardChanges` function**
   - Scan changed pieces for shard field differences
   - Mark tiles with shard changes + their parents for atomic replacement

2. **Implement `applyTileDiff` function**
   - Compare before/after for each tile field
   - Apply field updates for label, shards, mold
   - Call `applySegmentDiff` for each child segment

3. **Implement `applySegmentDiff` function**
   - Compute insertions and deletions from before/after UUID lists
   - Apply deletions in reverse index order
   - Apply insertions in forward order

4. **Implement `computeListOps` function**
   - Given before/after UUID arrays, compute minimal edit operations
   - Handle pure insertions, pure deletions, and mixed cases

5. **Implement `findParent` function**
   - Given pieces map and a tile ID, find which tile contains it in children
   - Used for marking parent tiles for atomic replacement when child restructures

6. **Handle non-tile pieces**
   - Grout, Secondary, Projector: continue with wholesale replacement (acceptable for now)

### Phase 3: Testing & Edge Cases

**Challenges:** True concurrent edit testing requires two independent actors making simultaneous changes. A single person with two browser tabs cannot achieve this - by the time you switch tabs, sync has already occurred.

**Testing Strategy:**

1. **Unit tests for pure diff logic (in patchwork-extra/hazel)**

   The core algorithms are pure functions that can be tested without Automerge:

   ```typescript
   // Test computeListOps
   describe('computeListOps', () => {
     it('detects insertions at different positions', () => {
       const before = ['a', 'b', 'c'];
       const after = ['x', 'a', 'b', 'c', 'y'];
       const ops = computeListOps(before, after);
       expect(ops).toContainEqual({ type: 'insert', index: 0, id: 'x' });
       expect(ops).toContainEqual({ type: 'insert', index: 4, id: 'y' });
     });

     it('detects deletions', () => {
       const before = ['a', 'b', 'c'];
       const after = ['a', 'c'];
       const ops = computeListOps(before, after);
       expect(ops).toContainEqual({ type: 'delete', index: 1 });
     });

     it('handles mixed insert/delete', () => {
       const before = ['a', 'b', 'c'];
       const after = ['a', 'x', 'c'];
       const ops = computeListOps(before, after);
       expect(ops).toContainEqual({ type: 'delete', index: 1 });
       expect(ops).toContainEqual({ type: 'insert', index: 1, id: 'x' });
     });
   });

   // Test hasShardChanges
   describe('hasShardChanges', () => {
     it('detects shard changes', () => { ... });
     it('finds parent of restructured tile', () => { ... });
   });
   ```

2. **Concurrent edit testing (requires coordination)**

   Options for testing true concurrency:
   - **Two people:** Coordinate with a collaborator - "ready, set, go" simultaneous edits
   - **Browser automation:** Script two browser instances to make edits in parallel
   - **Network delay simulation:** Make edits in one tab while offline, then reconnect

3. **Regression testing**
   - Ensure SyncReplace still correctly repositions caret after receiving merged state
   - Verify undo/redo still works with granular updates
   - Test that the shard-change rule correctly falls back to atomic replacement

### Phase 4: Restructuring Robustness (Optional)

**Tasks:**

1. **Handle children array length changes**
   - Adding/removing delimiters changes number of child slots
   - Ensure Automerge arrays are resized correctly

2. **Concurrent restructuring edge cases**
   - Two users adding different delimiters
   - User editing while another restructures
   - Document behavior, accept limitations

### Phase 5: String-Level CRDT (Future)

**Tasks:**

1. **Evaluate Automerge.Text for comments**
   - Would enable concurrent character-level edits
   - Requires schema change and migration

2. **Evaluate for token labels**
   - Lower priority - tokens are usually short
   - Semantic conflicts likely anyway

---

## Future Work

### String-Level CRDT

For concurrent edits to the same string (e.g., two users editing a comment):

```typescript
// Change from:
interface SecondaryContent {
  content: string;
}

// To:
interface SecondaryContent {
  content: Automerge.Text;
}
```

Then use `Automerge.splice()` for character-level operations.

**Priority:** Low. Segment-level granularity addresses most real-world concurrent editing scenarios.

### Operational Transform for Complex Restructuring

For complex restructuring operations (concurrent delimiter changes), could implement OT-style transformation rules.

**Priority:** Low. These are rare edge cases with acceptable (consistent but potentially surprising) behavior under pure CRDT.

### Performance Optimization

- Cache previous flat doc to avoid recomputation
- Dirty-tracking to skip unchanged pieces
- Batch multiple segment ops into single Automerge change

---

## Appendix: Automerge API Reference

### Array Operations

```javascript
// Insert at index
arr.insertAt(index, value);

// Delete at index
arr.deleteAt(index);

// Bulk insert/delete
Automerge.splice(doc, ["path", "to", "arr"], startIndex, deleteCount, [...insertItems]);
```

### Change Batching

```javascript
handle.change(d => {
  // All operations here are atomic
  d.pieces["id1"].children[0].insertAt(0, "new");
  d.pieces["id2"].shards = [0, 1];
  delete d.pieces["id3"];
});
```

### Conflict Detection (Advanced)

```javascript
// Get all concurrent values for a key
const conflicts = Automerge.getConflicts(doc, "path");
```

---

## References

- [Automerge Documentation](https://automerge.org/docs/hello/)
- [RGA Algorithm](https://liangrunda.com/posts/automerge-internal-1/)
- [Automerge Merge Rules](https://automerge.org/docs/reference/under-the-hood/merge_rules/)
- [Hazel Patchwork Integration](./patchwork-integration.md)

---

## Appendix: Design Decision Rationale

This appendix documents the scenarios and reasoning that led to the "shard change rule" - the decision to use atomic replacement when shards change, and granular CRDT operations otherwise.

### A.1 Why Granular Operations Matter

**The core problem:** Currently, Hazel replaces entire pieces atomically:

```javascript
d.pieces[id] = newPiece;  // Whole-value replacement → last-writer-wins
```

This causes concurrent edits to clobber even when logically independent.

**Example:** Two users editing different children of a `let` tile.

```javascript
// Initial
"uuid-let": {
  shards: [0, 1, 2],
  children: [["uuid-x"], ["uuid-1"]]  // pattern: x, definition: 1
}

// User A changes pattern to "x, y"
children: [["uuid-x", "uuid-comma", "uuid-y"], ["uuid-1"]]

// User B changes definition to "1 + 2"
children: [["uuid-x"], ["uuid-1", "uuid-plus", "uuid-2"]]
```

With atomic replacement, one edit wins entirely. With granular operations on `children[0]` and `children[1]` independently, both edits merge.

### A.2 Why RGA Works for Segment Insertions

Automerge's RGA (Replicated Growable Array) tracks insertions as "insert X after element Y" rather than "insert X at index N."

**Example:** Concurrent insertions at different positions.

```
Initial segment: [a, b, c]

User A: insert x after a → "insert x after element a"
User B: insert y after c → "insert y after element c"

Merged: [a, x, b, c, y]  ← Both preserved
```

Even concurrent insertions at the same position produce a consistent (deterministic) result based on operation IDs.

### A.3 The Field Independence Problem

Automerge treats each field as an independent CRDT. In a `handle.change()` block:

```javascript
handle.change(d => {
  d.pieces[id].shards = [1, 2];
  d.pieces[id].children = [[...]];
});
```

These become separate operations. During merge, each field's "winner" is determined independently based on operation IDs. Usually the same actor wins both, but it's not guaranteed.

### A.4 Scenario: Concurrent Shard Deletion (Invariant Preserved)

**Initial:** Complete let tile
```javascript
shards: [0, 1, 2],  // let, =, in
children: [["pat"], ["def"]]
```

**User A deletes "let":**
```javascript
shards: [1, 2],
children: [["def"]]  // pattern spliced to parent
```

**User B deletes "in":**
```javascript
shards: [0, 1],
children: [["pat"]]  // definition spliced to parent
```

**Any LWW combination:**
- A's shards + A's children: `[1,2]` + `[[def]]` → length 2, length 1 → `2 = 1+1` ✓
- B's shards + B's children: `[0,1]` + `[[pat]]` → length 2, length 1 → `2 = 1+1` ✓
- A's shards + B's children: `[1,2]` + `[[pat]]` → length 2, length 1 → `2 = 1+1` ✓
- B's shards + A's children: `[0,1]` + `[[def]]` → length 2, length 1 → `2 = 1+1` ✓

**Observation:** Invariant preserved because both users started from same state and each made internally consistent changes. The cross-contamination is semantically odd (wrong child in slot) but not structurally broken.

### A.5 Scenario: Concurrent Add/Remove Shard (Invariant VIOLATED)

**Initial:** Incomplete let tile (missing "in")
```javascript
shards: [0, 1],  // let, =
children: [["pat"]]
```

**User A deletes "let":**
```javascript
shards: [1],
children: []  // pattern spliced to parent
```

**User B adds "in":**
```javascript
shards: [0, 1, 2],
children: [["pat"], ["def"]]  // new definition child
```

**Cross-contamination:**
- A's shards + B's children: `[1]` (length 1) + `[["pat"], ["def"]]` (length 2) → `1 ≠ 3` **BROKEN**
- B's shards + A's children: `[0,1,2]` (length 3) + `[]` (length 0) → `3 ≠ 1` **BROKEN**

**Conclusion:** When one user adds a shard and another removes a shard, the invariant `shards.length = children.length + 1` can be violated if fields are merged independently.

### A.6 Scenario: Piece Duplication/Loss

**Initial:**
```javascript
parent.children[0]: ["a", "let", "b"]
"let".children: [["pat"]]
```

**User A restructures (removes "let" delimiter):**
```javascript
parent.children[0]: ["a", "pat", "let", "b"]  // pat spliced out
"let".children: []
```

**User B edits something else, doesn't touch structure.**

**If RGA applied to parent segment, but LWW picks User B's let tile:**
- Parent (RGA): `["a", "pat", "let", "b"]` ← pat inserted
- Let tile (LWW, B wins): `children: [["pat"]]` ← pat still here

**Result:** `pat` appears in TWO places. Invalid.

**Conclusion:** When pieces move between segments during restructuring, the segment changes and tile changes must be atomic to prevent duplication or loss.

### A.7 Scenario: Grout Insertion Complexity

When restructuring occurs, Hazel may automatically insert or remove grout pieces to maintain syntactic well-formedness.

**Example:** Adding a closing paren might insert concave grout at the boundary.

These grout pieces are not explicitly part of the user's edit but appear in the delta. If granular operations are applied to segments while the grout piece isn't properly coordinated, the segment could reference a non-existent piece or have unexpected grout.

**Conclusion:** Restructuring operations have implicit side effects (grout) that make granular merging unreliable.

### A.8 The Shard Change Rule

Given the above scenarios, we adopt the rule:

> **If `shards` changes for any tile in the delta, use atomic replacement for that tile and its parent. Otherwise, use granular CRDT operations.**

**Rationale:**

1. **Shard changes indicate restructuring.** This is the reliable signal that delimiters are being added/removed.

2. **Restructuring is coupled.** The tile's shards, children, and parent's segment must change together.

3. **Atomic replacement preserves invariants.** One user's complete, consistent state wins.

4. **Granular ops work for the common case.** Non-restructuring edits (content changes, segment insertions) benefit from CRDT merging.

5. **Acceptable tradeoff.** Restructuring is less common than content editing. Users doing restructuring are affecting segment boundaries anyway.

### A.9 What Gets Clobbered

With this rule, the following concurrent scenarios clobber (one edit wins):

| User A | User B | Result |
|--------|--------|--------|
| Restructures tile T | Restructures tile T | One wins |
| Restructures tile T | Inserts in T's parent segment | One wins |
| Restructures tile T | Edits T's children content | **Both preserved** (different pieces) |
| Inserts in segment S | Inserts in segment S | **Both preserved** (RGA) |
| Edits tile T content | Edits tile T content | **Both preserved** (if different fields) |

### A.10 Alternative Approaches Considered

**A. Model shards as set operations (add/remove individual shards)**

Would allow `[0,1,2] → [1]` when both users remove different shards. But requires coordinating with children changes - complex and still doesn't handle add+remove case.

**B. Operational transform for restructuring**

Define transformation rules for how restructuring operations compose. Complex to implement correctly, and restructuring is rare enough that LWW is acceptable.

**C. Post-merge invariant recovery**

Apply whatever CRDT gives us, then detect and fix violations. Difficult to determine the "right" fix, and could produce surprising results.

**D. Higher-level operation log**

Sync operations like "break shard 0 of tile T" rather than field-level changes. Essentially OT - significant architectural change.

**Chosen approach:** The shard change rule is simple, preserves invariants, handles the common case well, and has predictable behavior for edge cases.
