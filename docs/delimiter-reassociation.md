# Delimiter Reassociation

## Problem Statement

The Hazel editor's semi-structured editing model uses ID-based shard matching to
associate delimiters (e.g., `fun` with `->`, `(` with `)`). When a user types
left to right, the "backpack" mechanism (computed from missing shards of
incomplete tiles) determines which incomplete form a new delimiter should join.

This works well in the simple case but breaks down in several scenarios where
delimiters end up mis-associated, and there is no mechanism to correct the
association after the fact. The result is editing states that diverge from what a
text-based reparse would produce, violating user expectations.

## Background: Current Mechanisms

Three mechanisms currently handle delimiter association, each partial and ad-hoc:

**Expansion** (Insert.re, insert time): When a token is typed, `Form.Expansion`
determines if it should create a multi-token tile. E.g., typing `fun` creates
incomplete `["fun","->"][0]`. Sort-dependent gates exist: `|` only expands to
Rule form `["|","=>"]` when inside a case (otherwise stays standalone, since
`||` is more common in Exp context). These gates are ad-hoc checks on zipper
context (ancestor/sibling structure), not principled sort-based decisions.

**Backpack matching** (Insert.re, insert time): When a token is typed,
`backpack_find` checks if it matches a missing shard of an existing incomplete
tile. If so, the token gets the incomplete tile's ID. The "ambiguity gate"
limits polymorphs like `->` to only check the backpack HEAD, preventing
incorrect matches (e.g., `fun f : (Int -> Bool) -> x` — the type arrow `->` must
not match `fun`'s `->` through the shadowing `(`).

**Rescan** (Segment.re, post-edit): After each insert/destruct, walks flat
siblings left-to-right with a stack-based scoping mechanism. Matches standalone
monotiles against frames pushed by incomplete tiles. Handles retroactive matching
(out-of-order typing) and cross-form re-association (orphaned shards via
`presplit_orphans` and effective-label matching).

The fundamental issue: **expansion and backpack make one-shot decisions at insert
time that are never revisited.** Rescan handles retroactive matching but is
sort-blind — it can't redo expansion decisions when sort context changes.

## The Guiding Property

The soft goal: **the structural editor should feel like a text editor.** There
should be a limit — ideally zero — to how wrong a user can go by treating it
as a text editor.

**Property T (Text-Edit Faithfulness)**: For any state S, a single-character
structural edit producing state S' should satisfy:
1. `print(S')` is what applying the same character edit to `print(S)` as plain
   text would produce (the text change is predictable)
2. S' is **edit-equivalent** to `freshParse(print(S'))` — their difference
   never prevents the user from reaching a complete state via further edits

**What counts as observable**: NOT decorations, highlighting, or semantic
feedback. Those are best-effort for incomplete states. What matters is
**edit-level observability**: whether a structural difference affects the outcome
of future edits. A difference matters only if it prevents a sequence of
text-like edits from reaching a complete state that the same edits would reach
from a fresh parse.

See also Property H (history independence — stronger, same text → same
structure) and Property R (reachability — weaker, can eventually get there).
We target T.

## Motivating Scenarios

### Scenario 1: Out-of-order typing — SOLVED by rescan

Type `-> x`, move to start, type `fun a `. Rescan retroactively matches `fun`
with `->`. (Also handles `let`/`=`/`in`, `if`/`then`/`else`.)

### Scenario 2: Cross-form re-association — SOLVED by rescan

Delete `fun` from `fun a -> x`, type `fix`. Rescan's effective-label matching
and `presplit_orphans` let orphaned `->[1]` match `fix[0]`'s frame.
(Also handles `let`→`type`.)

### Scenario 3: `fun (a, b -> )` parent-trapping — recovery workflow exists

The ambiguity gate + parent-trapping means `->` gets stuck inside parens.
**Recovery**: insert `)` after `b` (backpack matches `(`), move to end, delete
old `)`. Rescan matches `fun` with `->`. Result: `fun (a, b) -> a`.
Parent-breaking (automatically extracting trapped delimiters) is probably not
needed given this recovery path.

### Scenario 4: case/end child remolding — CURRENT BUG

Paste `casex | a => 0 end`, add space between `case` and `x`. Rescan matches
`case` with `end`, reassemble forms the tile with children `[x, |, a, =>, 0]`.
But `|` and `=>` were created as standalone Exp-sort tokens (the `|` gate in
Insert.re prevented Rule expansion because there was no case context at insert
time). The post-edit pipeline can't fix this: `remold_tile` skips children when
the mold's `in_` sort didn't change (Rul == Rul), and even if it remolded, it
can't change `|`'s label from `["|"]` to `["|","=>"]` — remold only changes
molds within the same label, never promotes standalone tokens to multi-token
forms.

**Root cause**: Rescan is sort-blind. It can match tokens to existing incomplete
tiles' frames, but it can't create NEW incomplete tiles based on sort context.
The expansion logic that creates incomplete tiles lives only in Insert.re and
runs only at insert time.

## The Normalize Plan

### Design Principle

Expansion, backpack matching, rescan, reassembly, and remolding are all aspects
of one underlying operation: **given a segment and a sort context, produce the
correct tile structure.** Currently they're scattered across Insert.re (eager,
at insert time) and multiple post-edit passes (lazy, each partial). The fix is
to create a single unified operation — `normalize` — that handles all of these,
parameterized by sort, and applied recursively to children.

### Phase 3: Sort-Aware `normalize` — NEXT

A new `Segment.normalize(sort, seg)` function that composes:
1. **Sort-aware rescan**: the existing rescan walk, extended with
   `try_sort_expand`. For each standalone singleton token, check if
   `Form.Expansion.get(token)` yields a multi-token label with a mold whose
   `out == sort`. If so, promote the token to shard 0 of that form (making it
   incomplete). Then the existing frame-matching logic handles the rest.
2. **Reassemble**: existing `Segment.reassemble`, unchanged.
3. **Normalize children**: for each complete tile in the result, recursively
   call `normalize(child_sort, child)` using `mold.in_` for child sorts.
4. **Remold**: existing `Segment.remold(seg, sort)`.

`normalize` and `normalize_children` are mutually recursive. For already-correct
tiles, each step hits a fast path (no incomplete tiles → return unchanged), so
the cost of processing all children (not just newly-assembled ones) is minimal.

**How this fixes the case/end bug**: After rescan matches `case` with `end` and
reassemble forms the tile, `normalize_children` runs `normalize(Rul, child)` on
the child segment. Sort-aware rescan promotes `|` to `["|","=>"][0]` (because
Rule form has `out=Rul`), pushes a frame with `=>`. Standalone `=>` matches the
frame. Reassemble groups them into a Rule tile. `normalize_children` recurses
into Rule's child (`a`), remolding it from Exp to Pat sort. Remold fixes outer
molds in Rul context.

**Why sort-aware expansion replaces the `|` gate**: The Insert.re gate prevents
`|` from expanding to Rule outside a case context (otherwise typing `|` for
`||` would create a distracting backpack entry). `try_sort_expand` achieves the
same effect principally: it only promotes `|` to Rule when `sort == Rul`, which
only happens inside a case tile's child. No ad-hoc zipper checks needed.

**New code** (~30 lines in Segment.re, ~5 lines plumbing in Siblings.re/Zipper.re):
- `try_sort_expand(sort, tile)`: ~12 lines. Calls existing `Form.Expansion.get`
  and `Form.Molds.get`. Returns promoted tile or None.
- Integration in `rescan(~sort=Any, seg)`: ~4 lines. Apply `try_sort_expand`
  before existing matching logic. Promoted tiles flow through naturally (they're
  incomplete singletons → push frame).
- `normalize(sort, seg)` + `normalize_children(seg)`: ~15 lines. Mutually
  recursive. Composes existing functions.
- `Siblings.rescan(~sort)`, `Zipper.rescan_reassemble`: pass sort through.

**Zero duplication**: `try_sort_expand` is new logic (not duplicated from
Insert.re). `normalize` composes existing functions. `reassemble` is unchanged.

### Phase 4: Consolidate Post-Edit Processing

Currently Insert.go has redundant processing:

```
insert_or_append:
  1. insert_shard        — expansion + backpack (eager)
  2. remold_regrout      — remold + regrout
  3. merge_or_noop       — merge adjacent tokens, remold_regrout again
go wrapper:
  4. Triggers.insert     — conditional operations
  5. rescan_reassemble   — rescan + reassemble + remold + regrout (normalize)
```

Steps 2 and 5 both remold+regrout. With normalize handling this, step 2's
`remold_regrout` is redundant. Similarly `merge_or_noop` calls `remold_regrout`.

**Changes**: Remove `remold_regrout` calls from `insert_or_append` (step 2) and
`merge_or_noop` (step 3). Test that normalize at the end (step 5) subsumes them.

**Risk**: Step 2's remold happens before merge, which depends on token shapes.
But `merge_or_noop` checks `Token.is_potential_token` (text-level), not molds.
Likely safe to remove.

### Phase 5: Simplify Insert.re

With normalize handling expansion and matching reliably post-edit, Insert.re's
eager versions become redundant:

**Changes**:
- Remove the `|` gate from `Insert.expansion` (~5 lines). Sort-aware expansion
  in normalize handles it.
- Remove sibling-level backpack matching from `insert_shard`: always create
  standalone tokens, let normalize match them. (~10 lines simplified)
- Remove the `expansion` function and its `before_case_shard`/`inside_case`
  context checks (~25 lines).

After this, `insert_shard` simplifies to: create a standalone token and place
it. All structural decisions happen in normalize.

**Risk**: `merge_or_noop` calls `replace_shard` → `insert_shard`. Without
expansion, the merged token is standalone. Normalize fixes it, but the
intermediate state changes. Needs testing.

**What remains necessary in Insert.re**: tokenization (character → token),
cursor management (caret positioning, string/comment entry), merge logic
(adjacent token combination), Triggers. These are genuinely insert-time
concerns, orthogonal to structural normalization.

**What remains necessary outside normalize**:
- `Relatives.reassemble`: handles ancestor-level reassembly during cursor
  movement. Normalize only operates on siblings.
- `Relatives.remold`: may still be needed after `Relatives.reassemble` (which
  can modify siblings via parent reassembly).
- Ancestor-level backpack matching: a token matching a parent tile's missing
  shard. This involves the ancestor stack, not siblings. Normalize doesn't
  handle this. (Eventually, normalize could be extended to handle ancestors too,
  but that's a larger change.)

## Implementation Details

### Rescan: Stack-Based Scoping (Phases 1-2, done)

`Segment.rescan` walks siblings left-to-right. Incomplete tiles push frames
with their `right_missing_shards`. Singletons (shard count == 1) try to match
the top frame before checking incompleteness (enables cross-form matching of
orphaned shards like `->[1]` matching `fix[0]`). Multi-shard orphans are
pre-split by `presplit_orphans` into individual singletons with children
extracted as flat segments.

Integration point: `Zipper.rescan_reassemble` called from `Insert.go` and
`Destruct.go` after all normal processing. NOT from `Relatives.reassemble`
(cursor movement path).

### Ambiguity Gate (existing, load-bearing)

The backpack gate (only checking HEAD for polymorphs like `->`) is load-bearing.
Example: `fun f : (Int -> Bool) -> x`. Without the gate, the type arrow inside
parens would incorrectly match `fun`'s `->`. The gate can't be relaxed. Rescan's
stack-based scoping achieves the same effect naturally (inner tiles shadow outer
frames), so this is not a concern for normalize.

## Tests (Editing.Rescan section)

All rescan tests use `test_complete`: checks printer output AND that no
incomplete tiles remain anywhere in the zipped segment (recursive check).

- **Baselines**: Left-to-right `fun a -> x` and `let y = 1 in x`
- **Out-of-order**: fun/`->`, let/`=`/`in`, if/`then`/`else`
- **Cross-form**: fix reuses fun's `->`, type reuses let's `=` and `in`
- **Recovery**: `fun (a, b -> a)` insert-then-delete workflow
- **TODO**: case/end child remolding (Phase 3 test)
