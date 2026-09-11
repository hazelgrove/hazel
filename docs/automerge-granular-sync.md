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

**Assumptions to verify:**
- Automerge's nested array operations (`tile.children[i].insertAt()`, `tile.children[i].deleteAt()`) work correctly for arrays within objects within the top-level map. Most Automerge documentation focuses on top-level arrays; nested array RGA behavior should be verified with a minimal test before full implementation.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Problem Statement](#problem-statement)
3. [Invariants and Validation Framework](#invariants-and-validation-framework)
4. [Key Insight: The Shard Change Rule](#key-insight-the-shard-change-rule)
5. [Automerge CRDT Background](#automerge-crdt-background)
6. [Current Architecture](#current-architecture)
7. [Hazel Data Structures](#hazel-data-structures)
8. [Clobbering Scenarios](#clobbering-scenarios)
9. [Proposed Solution](#proposed-solution)
10. [Concrete Examples](#concrete-examples)
11. [Implementation Plan](#implementation-plan)
12. [Known Edge Cases and Limitations](#known-edge-cases-and-limitations)
13. [Future Work](#future-work)
14. [Appendix: Design Decision Rationale](#appendix-design-decision-rationale)

---

## Problem Statement

Hazel's current Automerge integration replaces entire pieces (tiles, grout, secondary) atomically. This causes concurrent edits to clobber each other even when they're logically independent:

- Two users editing different children of the same `let` tile → clobbered
- Two users inserting tokens at different positions in the same segment → clobbered
- User editing pattern while another edits definition → clobbered

The root cause: we're doing `d.pieces[id] = entireNewPiece` instead of using Automerge's granular list operations.

---

## Invariants and Validation Framework

### Core Invariants

Hazel maintains three invariants that must be preserved across all edit operations. When using granular CRDT operations, we must ensure Automerge's merge resolution doesn't violate these invariants.

| Invariant | Description | Defined In |
|-----------|-------------|------------|
| **UUID Uniqueness** | All piece IDs are unique across the entire edit state | Implicit |
| **Shards/Children Relationship** | For each tile: `length(shards) == length(children) + 1` | `Base.re:11-15` |
| **Segment Shape Consistency** | `Segment.skel(seg)` doesn't throw `Skel.Nonconvex_segment` | `Skel.re` |

### Approach

Our strategy for preserving invariants during granular sync:

1. **Define invariants** - The three invariants above
2. **Create validation predicates** - Development-mode checks that detect violations
3. **Identify violation scenarios** - Analyze when Automerge merging could break invariants
4. **Enforce atomicity** - For operations that could violate invariants if merged granularly, use atomic replacement

### Atomicity as Mitigation

When we identify a scenario where granular merging could violate an invariant, we fall back to atomic replacement. Currently, our "hammer" is piece-level atomicity - replacing entire pieces rather than updating fields granularly.

**Key insight:** All operations within a single `handle.change()` block in Automerge are atomic together. So when we update a tile and its parent in the same change block, they're guaranteed to be applied together, preventing mixed states.

**Open question:** Is piece-level atomicity always sufficient? In some cases, we may need to ensure multiple pieces are updated atomically together. The shard change rule does this by marking both the tile and its parent for atomic replacement in the same operation.

### Validation During Development

During development, we validate invariants after sync operations to detect any violations:

```reason
module SegmentValidator = {
  /* Check shards/children relationship for all tiles */
  let validate_shards_children = (seg: Segment.t): list(string) => {
    let errors = ref([]);
    let rec check = (seg, path) => {
      seg |> List.iteri((i, p) =>
        switch (p) {
        | Piece.Tile(t) =>
          let expected = List.length(t.children) + 1;
          let actual = List.length(t.shards);
          if (actual != expected) {
            errors := [
              path ++ "/tile" ++ string_of_int(i) ++
              ": shards=" ++ string_of_int(actual) ++
              " but children=" ++ string_of_int(List.length(t.children)),
              ...errors^
            ];
          };
          t.children |> List.iteri((j, child) =>
            check(child, path ++ "/tile" ++ string_of_int(i) ++ "/child" ++ string_of_int(j))
          );
        | _ => ()
        }
      );
    };
    check(seg, "root");
    errors^;
  };

  /* Check segment shape consistency via skeleton generation */
  let validate_shape = (seg: Segment.t): list(string) => {
    let errors = ref([]);
    let rec check = (seg, path) => {
      switch (Segment.skel(seg)) {
      | exception Skel.Nonconvex_segment =>
        errors := [path ++ ": Nonconvex segment (shape conflict)", ...errors^]
      | exception Skel.Input_contains_secondary =>
        () /* Shouldn't happen - skel filters secondary */
      | _ => ()
      };
      /* Recursively check tile children */
      seg |> List.iter(p =>
        switch (p) {
        | Piece.Tile(t) =>
          t.children |> List.iteri((j, child) =>
            check(child, path ++ "/child" ++ string_of_int(j))
          )
        | _ => ()
        }
      );
    };
    check(seg, "root");
    errors^;
  };

  /* Check UUID uniqueness across segment */
  let validate_unique_ids = (seg: Segment.t): list(string) => {
    let ids = Segment.ids(seg);
    let unique_ids = List.sort_uniq(Id.compare, ids);
    if (List.length(ids) != List.length(unique_ids)) {
      ["Duplicate piece IDs detected in segment"]
    } else {
      []
    };
  };

  /* Run all validations, log any errors */
  let validate_all = (seg: Segment.t): unit => {
    let errors =
      validate_shards_children(seg) @
      validate_shape(seg) @
      validate_unique_ids(seg);
    errors |> List.iter(e => Js.log("[SYNC VALIDATION] " ++ e));
  };
};
```

These checks are O(n) in segment size and should be disabled in production builds.

---

## Key Insight: The Shard Change Rule

**The fundamental rule governing this design:**

> If the `shards` field changes for any tile in the delta, fall back to atomic replacement for that tile and its parent. Otherwise, use granular CRDT operations on segment arrays.

**Invariant protected:** Shards/Children Relationship (`length(shards) == length(children) + 1`)

**Why this rule exists:**

1. **Shard changes = restructuring.** When `shards` changes, delimiters are being added or removed. This means pieces move between segments (from parent into tile's children, or vice versa).

2. **Restructuring is inherently coupled.** A restructuring operation affects multiple fields (shards, children) and multiple tiles (the restructured tile and its parent) simultaneously. If Automerge merges the `shards` field from one user with the `children` field from another, the invariant could be violated.

3. **Grout and remold complexity.** Restructuring may trigger automatic grout insertion/removal and piece remolding. These side effects must stay coupled with the restructuring operation.

4. **Atomicity ensures consistency.** By making the tile and its parent atomic together in the same `handle.change()` block, we guarantee the invariant is preserved.

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

**Same-position insertion behavior (concrete Hazel example):**

Initial: `1 + 2` (segment: `[one, ws, plus, ws, two]`)

User A and User B both position cursor after `+` and type simultaneously:
- User A types `3 * ` → wants `1 + 3 * 2`
- User B types `4 / ` → wants `1 + 4 / 2`

Both operations are "insert after the whitespace following `+`" in RGA terms.

Merged result (deterministic based on actor IDs): `1 + 3 * 4 / 2` or `1 + 4 / 3 * 2`

Both insertions are preserved, but the order is determined by RGA tie-breaking (operation IDs include actor IDs). Neither user gets exactly what they intended—User A wanted `3 * 2` as a subexpression, User B wanted `4 / 2`, but one of them gets their operator applied to the other's operand.

This is inherent RGA behavior: it preserves concurrent insertions at the same position in a deterministic order, but can't understand semantic intent. It's acceptable because:
- Truly simultaneous edits at the exact same position are rare
- The result is at least consistent across all clients
- There's no generally better solution without operational transform or manual conflict resolution

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
      // Mark parent for atomic IF it's in the delta (i.e., it changed too)
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

**Important: `findParent` only searches the delta pieces, not the full Automerge document.**

This works because of an invariant in Hazel's restructuring semantics:

- **If pieces cross the parent-child boundary** (moving into or out of a tile's children), **the parent's `children` field changes**, so the parent is in the delta.
- **If pieces don't cross the boundary** (e.g., deleting the middle delimiter of a `let`, which rearranges children within the tile but doesn't move pieces to/from parent), **the parent doesn't change** and doesn't need atomic treatment.

**Example: Middle delimiter change (parent unchanged)**

`let x = 1 in y` → delete the `=`:
```javascript
// Before
let_tile: { shards: [0, 1, 2], children: [[x], [1]] }
parent: { children: [[let_tile, y]] }

// After
let_tile: { shards: [0, 2], children: [[x, 1]] }  // x and 1 merge into one slot
parent: { children: [[let_tile, y]] }  // UNCHANGED
```

Here `let_tile` changes (shards and children), but `parent` doesn't—pieces `x` and `1` were already inside `let_tile` and just rearranged within its children. Since `parent` isn't in the delta, `findParent` returns null, and we don't mark it for atomic. This is correct: the restructuring is entirely within `let_tile`'s subtree.

**The invariant:** "Parent needs atomic treatment" implies "parent is in the delta." We never need to search beyond the delta pieces.

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
```

**Index calculation correctness:**

The delete/insert ordering works because:
- Delete indices are from `before` array positions
- Insert indices are from `after` array positions
- After all deletions (applied in reverse to preserve indices), the array contains exactly the LCS elements
- Insertions in forward order place new elements at their `after` positions, which correctly accounts for LCS elements and previously-inserted new elements

Example: `[a, b, c]` → `[x, a, c]` (delete b, insert x):
1. LCS = `[a, c]`
2. Delete b at index 1: `[a, c]`
3. Insert x at index 0: `[x, a, c]` ✓

This ordering should be verified with unit tests for complex reorder cases before implementation.

```typescript
/**
 * Compute minimal list operations to transform `before` into `after`.
 * Uses LCS (Longest Common Subsequence) to handle reorders correctly.
 *
 * Why LCS: A simple set-based diff (in before but not after = delete, etc.)
 * fails for reorders. If element X moves from position 1 to position 4,
 * set-diff sees X in both sets and generates no operations, leaving
 * Automerge's array unchanged while Hazel's local state has the new order.
 * This creates divergence that compounds on subsequent edits.
 *
 * Note: Automerge doesn't have native move operations yet (research ongoing,
 * see https://arxiv.org/abs/2311.14007). We implement moves as delete+insert.
 *
 * Complexity: O(n*m) for LCS where n,m are array lengths. Acceptable since
 * Hazel segments are typically small (dozens of pieces, not thousands).
 */
function computeListOps(before: UUID[], after: UUID[]): ListOp[] {
  const ops: ListOp[] = [];

  // Compute LCS to find elements that stay in relative order
  const lcs = computeLCS(before, after);
  const lcsSet = new Set(lcs);

  // Elements in before but not in LCS need to be deleted
  // (either truly deleted, or moved to a new position)
  before.forEach((id, i) => {
    if (!lcsSet.has(id)) {
      ops.push({ type: 'delete', index: i, id });
    }
  });

  // Elements in after but not in LCS need to be inserted
  // (either truly new, or moved from old position)
  after.forEach((id, i) => {
    if (!lcsSet.has(id)) {
      ops.push({ type: 'insert', index: i, id });
    }
  });

  return ops;
}

/**
 * Compute Longest Common Subsequence of two arrays.
 * Returns the elements (not indices) that appear in both arrays
 * in the same relative order.
 *
 * Complexity: O(n*m) time and space. For small arrays this is fine.
 * Could optimize with Hunt-Szymanski for arrays with few matches.
 */
function computeLCS<T>(a: T[], b: T[]): T[] {
  const n = a.length;
  const m = b.length;

  // dp[i][j] = length of LCS of a[0..i-1] and b[0..j-1]
  const dp: number[][] = Array(n + 1).fill(null).map(() => Array(m + 1).fill(0));

  for (let i = 1; i <= n; i++) {
    for (let j = 1; j <= m; j++) {
      if (a[i - 1] === b[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }

  // Backtrack to find actual LCS elements
  const lcs: T[] = [];
  let i = n, j = m;
  while (i > 0 && j > 0) {
    if (a[i - 1] === b[j - 1]) {
      lcs.unshift(a[i - 1]);
      i--; j--;
    } else if (dp[i - 1][j] > dp[i][j - 1]) {
      i--;
    } else {
      j--;
    }
  }

  return lcs;
}
```

**Reorder handling via LCS:**

The LCS-based approach correctly handles moves/reorders by treating moved elements as delete+insert pairs. For example, if `[a, X, b, c]` becomes `[a, b, c, X]`:
- LCS is `[a, b, c]` (X moved, not in common subsequence)
- Delete X at old position 1
- Insert X at new position 3

This generates proper RGA operations that Automerge can merge with concurrent edits.

**Concurrent moves:** If two users move the same element to different positions, both delete+insert pairs are applied. RGA determines the final position via operation ID ordering. The result is deterministic but may not match either user's intent—acceptable for this rare edge case.

### Receiver Side (Hazel iframe)

**Minimal changes needed.** Automerge handles the merge; tool.tsx sends the resulting pieces to Hazel. The `SyncReplace.sync_replace` logic is unchanged:

1. Receive delta (affected pieces)
2. Merge with current flat doc (delta overrides)
3. Convert to segment
4. Restore caret position

The key difference: the "delta" now reflects properly-merged state from Automerge's CRDT, not just last-writer-wins.

### Why Not Post-Merge Regrouting?

We initially considered adding a "post-merge regrouting" step to fix any shape conflicts from concurrent edits. However, this approach was rejected—see [Phase 2.5](#phase-25-hazel-invariant-validation-syncreplacere) for the detailed reasoning. In short: regrouting creates new grout pieces with fresh UUIDs, which would cause divergence between clients who both need to regrout after the same merge.

Instead, we rely on RGA operations correctly handling segment structure (see [Grout Scenario Analysis](#grout-scenario-analysis)) and the shard change rule for restructuring atomicity.

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

**Important clarification:** The `before` state is used solely to compute what operations *Hazel intended* to perform—specifically for shard change detection and diff computation. It does **not** represent the current Automerge state (which may have diverged due to concurrent edits from other users). Automerge's CRDT semantics handle the actual merge; `before` just tells tool.tsx which RGA operations to generate based on Hazel's local edit.

**Note on backwards compatibility:** This message protocol change is not backwards compatible. An updated tool.tsx receiving messages from an old Hazel (without `before`) won't work correctly. This is acceptable since the Hazel-Patchwork integration is pre-alpha; we require coordinated deployment of both Hazel and tool.tsx.

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

2. **Update delta type** in `PatchworkComm.re` to include before pieces:
   ```reason
   type delta = {
     changed: Id.Map.t(FlatConvert.Flat.piece),
     changed_before: Id.Map.t(FlatConvert.Flat.piece),  // NEW: old versions
     added: Id.Map.t(FlatConvert.Flat.piece),
     deleted: list(Id.t),
   };
   ```

3. **Update `compute_delta`** to collect before versions:
   ```reason
   // In the changed piece detection:
   | Some(old_piece) =>
     if (old_piece != new_piece) {
       changed := Id.Map.add(id, new_piece, changed^);
       changed_before := Id.Map.add(id, old_piece, changed_before^);  // ADD THIS
     };
   ```

4. **Update `js_of_flatdoc`** (or create new function) to include both `state` and `before` in the message:
   - `state` contains changed + added pieces (after versions)
   - `before` contains changed pieces only (before versions)
   - `deleted` remains unchanged

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

7. **Implement `computeLCS` function**
   - Longest Common Subsequence algorithm for reorder detection
   - O(n*m) time/space, acceptable for typical segment sizes
   - Used by `computeListOps` to correctly handle moves

**Complexity notes:**
- `hasShardChanges`: O(changed_pieces) for shard comparison, O(pieces × children) for `findParent` but only called for tiles with shard changes (typically rare)
- `computeListOps`: O(n*m) for LCS where n,m are segment lengths (typically small)
- Overall `applyDelta`: O(changed_pieces × max_segment_length²)

### Phase 2.5: Hazel Invariant Validation (SyncReplace.re)

**Goal:** Validate that merged state satisfies all invariants before applying to editor.

**Files:** `src/haz3lcore/patchwork/SyncReplace.re`, new `src/haz3lcore/patchwork/SegmentValidator.re`

**Tasks:**

1. **Implement SegmentValidator module**

   Create the validation module as defined in [Invariants and Validation Framework](#invariants-and-validation-framework). This module checks:
   - Shards/children relationship for all tiles
   - Segment shape consistency via `Segment.skel`
   - UUID uniqueness across the segment

2. **Add validation call in sync_replace**

   After `doc_to_seg`, call `SegmentValidator.validate_all`:

   ```reason
   // In sync_replace, after doc_to_seg:
   let new_seg =
     PerfLog.measure("doc_to_seg_merged", () =>
       FlatConvert.doc_to_seg(merged_doc)
     );

   // Validate invariants - comment out for production
   SegmentValidator.validate_all(new_seg);
   ```

3. **Consider adding validation on send side**

   Optionally validate before sending to catch issues at the source:
   ```reason
   // In send_state_delta, before sending - comment out for production
   SegmentValidator.validate_all(new_seg);
   ```

**Why not post-merge regrouting?**

We initially considered calling `Segment.regrout` after merge to fix shape conflicts. However, this approach is flawed:

1. Regrouting creates new grout pieces with fresh UUIDs via `Id.mk()`
2. If both clients need to regrout after the same merge, they generate different UUIDs
3. This causes divergence between clients - worse than the original problem

Instead, we rely on:
- The shard change rule to make restructuring operations atomic
- Validation to detect any violations that slip through
- The analysis below showing that RGA operations on segment arrays handle most cases correctly

See [Grout Scenario Analysis](#grout-scenario-analysis) in Known Edge Cases for detailed reasoning.

### Phase 3: Testing & Edge Cases

**Challenges:** True concurrent edit testing requires two independent actors making simultaneous changes. A single person with two browser tabs cannot achieve this - by the time you switch tabs, sync has already occurred.

**Acceptable scope:** The testing strategy below is sufficient for initial implementation. More comprehensive concurrent testing can be added as the integration matures.

**Testing Strategy:**

1. **Unit tests for pure diff logic (in patchwork-extra/hazel)**

   The core algorithms are pure functions that can be tested without Automerge:

   ```typescript
   // Test computeLCS
   describe('computeLCS', () => {
     it('finds common subsequence', () => {
       expect(computeLCS(['a', 'b', 'c'], ['a', 'c'])).toEqual(['a', 'c']);
     });

     it('handles reorders', () => {
       // X moved from position 1 to position 3
       expect(computeLCS(['a', 'X', 'b', 'c'], ['a', 'b', 'c', 'X'])).toEqual(['a', 'b', 'c']);
     });

     it('handles empty arrays', () => {
       expect(computeLCS([], ['a', 'b'])).toEqual([]);
       expect(computeLCS(['a', 'b'], [])).toEqual([]);
     });

     it('handles no common elements', () => {
       expect(computeLCS(['a', 'b'], ['x', 'y'])).toEqual([]);
     });
   });

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
       expect(ops).toContainEqual({ type: 'delete', index: 1, id: 'b' });
     });

     it('handles mixed insert/delete', () => {
       const before = ['a', 'b', 'c'];
       const after = ['a', 'x', 'c'];
       const ops = computeListOps(before, after);
       expect(ops).toContainEqual({ type: 'delete', index: 1, id: 'b' });
       expect(ops).toContainEqual({ type: 'insert', index: 1, id: 'x' });
     });

     it('handles reorders (move element)', () => {
       // Move X from position 1 to position 3
       const before = ['a', 'X', 'b', 'c'];
       const after = ['a', 'b', 'c', 'X'];
       const ops = computeListOps(before, after);
       // X is not in LCS, so it gets delete+insert
       expect(ops).toContainEqual({ type: 'delete', index: 1, id: 'X' });
       expect(ops).toContainEqual({ type: 'insert', index: 3, id: 'X' });
     });

     it('handles swap (two elements exchange positions)', () => {
       const before = ['a', 'b', 'c'];
       const after = ['c', 'b', 'a'];
       const ops = computeListOps(before, after);
       // LCS is ['b'], so a and c both need delete+insert
       expect(ops.filter(o => o.type === 'delete')).toHaveLength(2);
       expect(ops.filter(o => o.type === 'insert')).toHaveLength(2);
     });
   });

   // Test hasShardChanges
   describe('hasShardChanges', () => {
     it('detects shard addition (delimiter added)', () => {
       const before = { t: 'Tile', id: 't1', shards: [0, 1], children: [[]] };
       const after = { t: 'Tile', id: 't1', shards: [0, 1, 2], children: [[], []] };
       expect(hasShardsChanged(before, after)).toBe(true);
     });

     it('detects shard removal (delimiter removed)', () => {
       const before = { t: 'Tile', shards: [0, 1, 2], children: [[], []] };
       const after = { t: 'Tile', shards: [0, 2], children: [[]] };
       expect(hasShardsChanged(before, after)).toBe(true);
     });

     it('returns false when only children content changes', () => {
       const before = { t: 'Tile', shards: [0, 1], children: [['a']] };
       const after = { t: 'Tile', shards: [0, 1], children: [['a', 'b']] };
       expect(hasShardsChanged(before, after)).toBe(false);
     });

     it('returns false for non-Tile pieces', () => {
       const before = { t: 'Grout', id: 'g1' };
       const after = { t: 'Grout', id: 'g1' };
       expect(hasShardsChanged(before, after)).toBe(false);
     });
   });

   // Test atomicIds marking
   describe('atomicIds marking', () => {
     it('marks parent when child restructures', () => {
       const before = {
         'parent': { t: 'Tile', children: [['child']] },
         'child': { t: 'Tile', shards: [0], children: [[]] }
       };
       const after = {
         'parent': { t: 'Tile', children: [['child']] },
         'child': { t: 'Tile', shards: [0, 1], children: [[], []] }
       };
       const atomicIds = hasShardChanges(before, after, ['child', 'parent']);
       expect(atomicIds.has('child')).toBe(true);
       expect(atomicIds.has('parent')).toBe(true);
     });

     it('transitively marks via chain of shard changes', () => {
       // grandparent -> parent -> child, both parent and child have shard changes
       const changedIds = ['parent', 'child'];
       // child shard change -> parent marked
       // parent shard change -> grandparent marked
       // All three should be in atomicIds
     });

     it('does NOT mark grandparent when only child restructures', () => {
       // grandparent -> parent -> child
       // Only child has shard change, parent is in delta but no shard change
       // Grandparent should NOT be atomic (restructuring contained in parent's subtree)
       const changedIds = ['parent', 'child'];
       // child shard change -> parent marked
       // parent NO shard change -> grandparent NOT marked
     });
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

4. **Verification scenario: Concurrent restructure + edit**

   This scenario should be manually verified to ensure the shard-change rule works correctly when restructuring and content editing happen concurrently on related pieces:

   **Setup:** `( a b c` (incomplete parens)
   ```javascript
   parent.children[0]: [parens, a, b, c]
   parens: { shards: [0], children: [[]] }
   a: { label: ["a"] }
   ```

   **User A:** Completes parens after `b`, moving `a, b` inside
   ```javascript
   parent.children[0]: [parens, c]
   parens: { shards: [0, 1], children: [[a, b]] }
   ```

   **User B:** Edits piece `a`, changing label from `a` to `x`
   ```javascript
   a: { label: ["x"] }
   ```

   **Expected merge result:**
   - `parens` gets User A's restructuring (atomic, shards changed)
   - `a` gets User B's label edit (granular, different piece)
   - Final: `( x b ) c`

   Both edits should be preserved because they operate on different pieces. The restructuring determines position (a is inside parens), the edit determines content (a is now x).

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

### Garbage Collection for Orphan Pieces

Orphan pieces (in pieces map but not referenced by any segment) can accumulate from concurrent delete+modify conflicts. A future optimization could:
- Walk tree from root periodically to identify all reachable pieces
- Delete unreferenced pieces from the Automerge document
- Run on document load or as a background task

### Additional Invariant Detection

As we discover new invariant violations through validation, the framework allows us to:
1. Add new validation predicates to `SegmentValidator`
2. Identify the operations that cause violations
3. Add appropriate atomicity rules (similar to shard change rule)

### Projector Sync Interactions

Projectors wrap syntax pieces and have their own model state. The current plan treats projectors with atomic replacement (like Grout and Secondary). Future work should analyze:

- **Model vs. syntax conflicts:** What happens when User A edits the projector model while User B edits the wrapped syntax (a separate piece)? These *should* merge correctly since they're different pieces, but the interaction deserves explicit verification.
- **Projector-specific atomicity:** Should projector model changes be granular (if models have internal structure) or remain atomic?
- **Wrapped syntax boundaries:** When the wrapped syntax piece changes shards (restructuring), does the projector need special handling?

---

## Known Edge Cases and Limitations

### Grout Scenario Analysis

This section documents the reasoning about whether grout operations require special atomicity handling.

**Initial concern:** Could concurrent edits involving grout create shape conflicts that violate the Segment Shape Consistency invariant?

**Scenario examined:**

```
Initial: [x, G, y] in parent's children[0]
  - x is convex-right
  - G is concave grout between x and y
  - y is convex-left

User A: Deletes y, which triggers auto-removal of G
  - A's segment ops: deleteAt(1) for G, deleteAt(2) for y
  - A's pieces map: delete G, delete y

User B: Modifies y (e.g., label change)
  - B's segment ops: none (structure unchanged)
  - B's pieces map: update y to y'
```

**Two possible outcomes considered:**

**Hypothesis 1 (initial concern):** After merge, segment is `[x, y']` with shape conflict.

**Hypothesis 2 (actual behavior with RGA):** After merge, segment is `[x]`, y' is orphan.

**Analysis:**

With granular RGA operations:
- A's deleteAt operations apply to the segment array
- These delete G and y from the array, regardless of B's piece modification
- Result: segment array is `[x]`
- B's modification to y survives in the pieces map (Automerge's "set beats delete")
- But y' is an orphan - in the map but not referenced by any segment

**Conclusion:** The segment `[x]` is properly shaped - no shape conflict occurs. The issue is orphaned y', not a shape violation.

**Why shape conflicts don't occur:**

The segment array and pieces map are updated separately:
- Segment array changes via RGA operations (deleteAt, insertAt)
- Piece content changes via map operations (set, delete)

When A deletes y from the segment array, that deletion applies via RGA. B's modification to y's content doesn't prevent the array deletion - it only affects the pieces map.

**Why post-merge regrouting doesn't work:**

We initially considered calling `Segment.regrout` after merge as a safety net. However:
1. `Segment.regrout` is not recursive - it only regrouts at one level
2. Even if recursive, regrouting creates new grout pieces with `Id.mk()` - fresh UUIDs
3. If both clients need to regrout after the same merge, they generate different UUIDs
4. This causes divergence between clients - worse than the original problem

**Final approach:** Rely on:
- The shard change rule for restructuring operations
- Validation predicates to detect any violations
- RGA handling of segment arrays, which correctly manages structure

### Message Size Increase

Sending `before` state alongside `after` state approximately doubles the payload for changed pieces. This is acceptable because:
- Only changed pieces are included (not the whole document)
- Most edits affect few pieces
- Large edits are already slow for other reasons (rendering, parsing)
- The payload increase is proportional to edit size, not document size

### Orphan Pieces

**Scenario:** Concurrent delete and modify of the same piece.

**Example:**
- User A: Deletes piece y (and auto-deletes grout G)
- User B: Modifies piece y

**Merge result:**
- Segment array: `[x]` (A's deletions apply via RGA)
- Pieces map: y' exists (B's modification via "set beats delete")

**Result:** y' is an "orphan" - exists in pieces map but unreferenced by any segment.

**Impact:** Memory leak (orphans accumulate), not corruption. The tree structure remains valid. This is a genuine conflict (A deleted what B was editing) with an acceptable resolution.

**Mitigation (future):** Periodic garbage collection - walk tree from root, delete unreferenced pieces. Could run on document load or periodically during editing.

### Remolding and Shape Changes

**Question:** Can a piece's shape change without its shards changing?

**Answer:** Yes, through remolding. Example: the minus token (`-`) can be either prefix (in `-1`) or infix (in `2-1`). If you insert `2` before `-1`, the `-` remolds from prefix to infix.

**Why this doesn't cause merge problems:**

Remolding happens as part of Hazel's edit processing. When User A's edit triggers remolding:
1. Hazel processes the edit locally
2. The remolded pieces are included in A's delta
3. The segment A sends is valid

If A's remolding and B's edit both affect the same segment:
- A's delta includes the remolded pieces
- B's delta includes B's changes
- Automerge merges them (piece-level granularity or LWW)
- The result is one consistent state

The shard change rule isn't needed for remolding because remolding doesn't change segment structure (which pieces are in which segments) - it only changes piece content (the mold field).

### Concurrent Moves

If two users move the same element to different positions:
- Both generate delete+insert operations via LCS
- RGA determines final position by operation ID ordering
- Result is deterministic but may not match either user's intent

This is an inherent limitation of CRDTs without native move support. Automerge move operations are [being researched](https://arxiv.org/abs/2311.14007) but not yet available.

---

## References

- [Automerge Documentation](https://automerge.org/docs/hello/)
- [RGA Algorithm](https://liangrunda.com/posts/automerge-internal-1/)
- [Automerge Merge Rules](https://automerge.org/docs/reference/under-the-hood/merge_rules/)
- [Hazel Patchwork Integration](./patchwork-integration.md)
- [Extending JSON CRDTs with Move Operations](https://arxiv.org/abs/2311.14007) - Research on native move support for Automerge (not yet implemented)

---

## Appendix: Design Decision Rationale

> **Note:** This appendix preserves the detailed analysis that motivated the shard change rule. The main concepts are summarized in [Invariants and Validation Framework](#invariants-and-validation-framework) and [Key Insight: The Shard Change Rule](#key-insight-the-shard-change-rule). Skip to [Implementation Plan](#implementation-plan) if you just want to implement.

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
