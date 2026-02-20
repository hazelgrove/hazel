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
as a text editor. This means each single-character insertion or deletion should
behave the way it would in a text editor with an incremental parser behind it.

We've considered several formalizations, in decreasing order of strength:

**Property H (History Independence)**: The structure is a pure function of the
text content. `text(s1) = text(s2)` implies `s1 = s2`. After every edit, the
structure is what a fresh left-to-right parse of the current text would produce.
This is clean and would enable property-based testing (random edits; check
`structure == freshParse(text)`). But it may require parent-breaking (see below)
and may be stronger than necessary.

**Property T (Text-Edit Faithfulness)**: For any state S, a single-character
structural edit producing state S' should satisfy:
1. `print(S')` is what applying the same character edit to `print(S)` as plain
   text would produce (the text change is predictable)
2. S' is **edit-equivalent** to `freshParse(print(S'))` — their difference
   never prevents the user from reaching a complete state via further edits

This is weaker than H: it allows S' and `freshParse(print(S'))` to have
different internal structure, as long as the difference doesn't affect editing
dynamics. Internal differences are "distinctions without a difference" if they
never block or redirect the user's path to completeness.

**What counts as observable**: NOT decorations, highlighting, or semantic
feedback. Those are best-effort for incomplete states and may differ between
structurally different representations of the same text — that's fine. What
matters is **edit-level observability**: whether a structural difference affects
the outcome of future edits. Specifically, a difference matters if it prevents
a sequence of text-like edits from reaching a complete state that the same
edits would reach from a fresh parse. The system's interpretation of incomplete
states (for display purposes) is informational, not authoritative — the user
knows what they're typing toward. The property is about not getting in their
way.

**Property R (Reachability)**: For any complete state A and target text B, if
B is reachable from `text(A)` via text edits, then some complete state with
text B is reachable from A via structural edits. This is the weakest — it says
you can "eventually get there" but doesn't constrain intermediate states.

**What we think we want**: T, not necessarily H. The user should be able to
treat the editor as a text editor, with each edit step producing the expected
result. But if the internal structure sometimes differs from a fresh parse in
unobservable ways, that's acceptable. H is sufficient for T but may not be
necessary. R is probably too weak — it doesn't guarantee that each individual
edit step is faithful, only that recovery is eventually possible.

**Current status**: The rescan gets us much closer to T. Many cases that
previously diverged now behave correctly (out-of-order typing, cross-form
re-association). But T is not proven to hold in general, and may not hold for
cases involving delimiters trapped inside children (see parent-breaking below).
The `fun (a, b -> )` recovery test shows R holds for that case, and observed
behavior suggests T may also hold (inserting `)` after `b` correctly matched
the parens via backpack), but this hasn't been systematically verified.

**Open question**: Whether T can be achieved without parent-breaking, or
whether parent-breaking is needed for some cases. Also: whether there exist
cases where two states with the same text but different structure actually
diverge in editing dynamics (i.e., a text-edit sequence reaches completeness
from one but not the other).

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
2. **Reassemble-and-normalize**: a variant of `Segment.reassemble` that, when
   it forms a complete tile from shards, recursively calls `normalize` on each
   child with its expected sort from `mold.in_`. This is NOT the same as the
   existing `reassemble` — it only recurses into children of tiles it actually
   assembles this call. If `incomplete_tiles(seg)` is empty, returns immediately.
3. **Remold**: existing `Segment.remold(seg, sort)`.

`normalize` and `reassemble_normalize` are mutually recursive:

```
normalize(sort, seg) =
  seg |> presplit_orphans |> rescan(~sort)
      |> reassemble_normalize(sort) |> remold(_, sort)

reassemble_normalize(sort, seg) =
  switch (incomplete_tiles(seg))
  | [] => seg    /* fast path: nothing to assemble */
  | [t, ...] =>
    ...find and group shards of t...
    children = if complete(t):
      map2((child, child_sort) =>
        if (child_sort != sort) normalize(child_sort, child)
        else child,                        /* sort-change gate */
        t.children, t.mold.in_)
    else:
      map(reassemble_normalize(sort), t.children)
    ...continue with remainder...
```

### Performance: The Sort-Change Gate

**The problem without gating**: If `reassemble_normalize` unconditionally calls
`normalize` on every child, forming a tile around large content would trigger
O(program-size) work. E.g., typing `(` before and `)` after a program creates
parens with child = entire program → `normalize(Exp, entire_program)` walks
everything for no benefit.

**The optimization**: Only recurse into children where `child_sort != sort`.
The content that becomes a child was previously at the sibling level, in sort
`sort`. Sibling-level processing (rescan + reassemble + remold) already ran on
it in that sort. If the child sort (from `mold.in_`) equals `sort`, nothing
changed — skip entirely.

**Concrete examples**:
- Parens (`in_=[Exp]`): child Exp == parent Exp → **skip**
- Case (`in_=[Rul]`): child Rul != parent Exp → **recurse** (case body only)
- Fun (`in_=[Pat]`): child Pat != parent Exp → **recurse** (pattern only)
- Let (`in_=[Pat,Exp]`): Pat != Exp → recurse on binding; Exp == Exp → skip body
- Rule (`in_=[Pat]`): Pat != Rul → recurse on pattern

Recursion depth is bounded by the number of sort transitions in the newly-
assembled chain, which in practice is 1-2 levels.

### Correctness of the Sort-Change Gate

The sort-change gate assumes: if the child sort equals the parent sort, then
no operation in normalize would change the child segment. This must hold for
every operation normalize performs:

**1. presplit_orphans** — Sort-irrelevant. Splits multi-shard orphan tiles into
singletons. Sibling-level `Siblings.rescan` already calls presplit on the flat
siblings before the main rescan walk. After reassembly, the child segment is
formed from content that was already presplit. No new orphans appear from
reassembly itself (reassembly only combines shards, it doesn't create orphans).
**Gate sufficient: yes.** ✓

**2. rescan — frame matching** — Sort-irrelevant. Matches standalone tokens
against incomplete tiles' missing-shard frames. Sibling-level rescan already
ran frame matching on this content (it was part of the flat siblings — rescan
operates on the combined `pre @ suf` before reassembly splits them into
children). Any matchable singleton/frame pairs were already matched.
**Gate sufficient: yes.** ✓

**3. rescan — try_sort_expand** — Sort-dependent. Promotes standalone tokens
to multi-token incomplete tiles when the expansion has `out == sort`. If the
sort didn't change, the same expansions were available at the sibling level.
Specifically: `try_sort_expand(sort, tile)` checks `Form.Expansion.get(token)`
and filters by `mold.out == sort`. With the same sort, the same filter applies.
Any token that should have been promoted in this sort was already promoted by
sibling-level sort-aware rescan (or by Insert.re at insert time).
**Gate sufficient: yes.** ✓

**4. reassemble** — Depends on incomplete tiles. If rescan didn't create any
new incomplete tiles (because try_sort_expand found nothing to promote), and
sibling-level rescan already matched everything, there are no incomplete tiles
in the segment. `incomplete_tiles(seg)` returns empty, reassemble returns
immediately. **Gate sufficient: yes.** ✓

**5. remold** — Sort-dependent. Re-evaluates each tile's mold in the given
sort context via `Form.Molds.get(label) |> filter(m.out == sort)`. Sibling-level
`Relatives.remold` already ran on these tiles in this same sort (it calls
`Siblings.remold(siblings, Ancestors.sort(ancestors))` — the ancestor sort is
the same `sort` parameter). Tiles already have correct molds.
**Gate sufficient: yes.** ✓

**The chain of reasoning**: if the sort didn't change, then (a) no tokens need
re-expansion → (b) no new incomplete tiles → (c) reassemble is a no-op →
(d) no tiles need re-molding. Every operation in normalize is either
sort-dependent (and the sort didn't change) or was already handled at the
sibling level (because the content was part of the flat siblings when
sibling-level processing ran).

**Key assumption**: all of normalize's operations are parameterized solely by
sort. If a future operation depended on other context (e.g., the identity of
the parent tile, or position within the parent), the sort-change gate would
not be sufficient for that operation. This assumption should be verified if
normalize gains new operations.

**How this fixes the case/end bug**: After rescan matches `case` with `end`,
`reassemble_normalize` forms the tile and calls `normalize(Rul, child)` on the
child segment. Sort-aware rescan promotes `|` to `["|","=>"][0]` (because Rule
form has `out=Rul`), pushes a frame with `=>`. Standalone `=>` matches the
frame. `reassemble_normalize` groups them into a Rule tile and recurses into
Rule's child (`a`), remolding it from Exp to Pat sort. Remold fixes outer molds
in Rul context.

**Why sort-aware expansion replaces the `|` gate**: The Insert.re gate prevents
`|` from expanding to Rule outside a case context (otherwise typing `|` for
`||` would create a distracting backpack entry). `try_sort_expand` achieves the
same effect principally: it only promotes `|` to Rule when `sort == Rul`, which
only happens inside a case tile's child. No ad-hoc zipper checks needed.

**New code** (~45 lines in Segment.re, ~5 lines plumbing in Siblings.re/Zipper.re):
- `try_sort_expand(sort, tile)`: ~12 lines. Calls existing `Form.Expansion.get`
  and `Form.Molds.get`. Returns promoted tile or None.
- Integration in `rescan(~sort=Any, seg)`: ~4 lines. Apply `try_sort_expand`
  before existing matching logic. Promoted tiles flow through naturally (they're
  incomplete singletons → push frame).
- `reassemble_normalize(sort, seg)`: ~15 lines. Mirrors `reassemble`'s switch
  structure but calls `normalize` on children of complete tiles instead of just
  `reassemble`. This is ~15 lines of structural duplication with `reassemble`,
  justified by keeping the performance-critical `reassemble` (used during cursor
  movement) lean.
- `normalize(sort, seg)`: ~5 lines. Composes presplit + rescan + reassemble_normalize + remold.
- `Siblings.rescan(~sort)`, `Zipper.rescan_reassemble`: pass sort through.

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
