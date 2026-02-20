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

The guiding invariant: **if the entire editor content were retyped left to right,
you would get the expected structure.**

## Background: The Backpack Mechanism

The "backpack" is virtual — `local_backpack(z)` computes missing shards from
incomplete tiles via `Relatives.local_missing_shards`:

- **Siblings**: `Siblings.local_missing_shards` considers `right_missing_shards`
  for left-side incomplete tiles (reversed, so closest first) and
  `left_missing_shards` for right-side incomplete tiles.
- **Ancestors**: `Ancestors.local_missing_shards` considers
  `missing_middle_shards` of the immediate ancestor tile.

At insert time (`Insert.insert_shard`), newly typed tokens check `backpack_find`:
- **Non-ambiguous tokens**: search the entire backpack by effective label
- **Ambiguous polymorphs** (tokens like `->` that are both standalone forms and
  delimiters): only check the backpack HEAD, giving the standalone form a chance

If matched, the shard is inserted with the **existing tile's ID**, so
`Segment.reassemble` (which matches by ID) can combine them.

## Motivating Scenarios

### Scenario 1: `fun (a, b -> )` (ambiguity + shadowing + parent trapping)

Typing left to right: `fun (a, b -> )`

After `fun (a, b`: backpack = [`(`'s `)`, `fun`'s `->`]. The `(`'s `)` shadows
`fun`'s `->`. When `->` is typed, the ambiguity gate checks only the head
(`)`), doesn't match, and `->` stays standalone. When `)` is typed, it matches
`(`, trapping `->` inside the parens' child.

**This involves three sub-problems**: the ambiguity gate preventing the match,
the lack of retroactive reassociation, AND the parent-breaking problem (the `->`
is inside a bidelimited child after `)` closes).

### Scenario 2: Out-of-order typing at the same level

Type `-> x`, move to start, type `fun a `. The `->` is a standalone monotile
and `fun` is incomplete — they're at the same sibling level but the backpack
mechanism ran at insert time (when `->` was typed, `fun` didn't exist yet).

**This is what the basic rescan solves.**

### Scenario 3: Cross-form re-association (`fix`/`fun`, `let`/`type`)

Start with `fun a -> x`, delete `fun`. The `->` remains as an orphaned shard
with label `["fun", "->"]`, shards `[1]`. Type `fix` to the left. The backpack
sees the orphaned `->[1]`'s left-missing shard has effective_label `["fun"]`,
which doesn't match `"fix"`. So `fix` gets a fresh ID and the `->` stays
orphaned.

**But conceptually**, the `->` should be able to attach to `fix` (label
`["fix", "->"]`) just as well as `fun`. The obstacle is that matching is done
against the **original label**, not the effective label of the remaining shards.

Similarly: delete `let` from `let x = 1 in y`, orphaned `=[1]` and `in[2]`
retain label `["let", "=", "in"]`. Typing `type` doesn't match.

**This is what effective-label matching in rescan would solve.**

### Scenario 4: Parent-breaking

Inside `(a, b -> x)` (complete parens), the `->` is trapped in the parens'
child. To reassociate it with an outer `fun`, you'd need to break the parens
tile — extract `->` from the child and place it at the parent level.

This is a fundamentally different operation from sibling-level rescan.

## Implementation Status

### Phase 1: Basic Sibling-Level Rescan — DONE

**What it does**: After each insert/destruct, `Segment.rescan` walks the flat
siblings left-to-right with a stack-based scoping mechanism. When it encounters
an incomplete tile, it pushes a frame with its `right_missing_shards`. Standalone
monotiles (label length 1, shard count 1) to the right are checked against the
top frame. If matched, the monotile is replaced with the target shard (giving it
the incomplete tile's ID). Then `reassemble` groups same-ID shards.

**Stack-based scoping**: Each incomplete tile pushes a new frame, shadowing the
previous. This prevents tokens inside a closer tile's conceptual child from
being incorrectly matched with a farther tile. E.g., `=` inside `(l=String)`
is not stolen by `let`'s `=`.

**Integration point**: `Zipper.rescan_reassemble` is called from `Insert.go` and
`Destruct.go` after all normal processing. NOT from `Relatives.reassemble`
(which runs on every cursor movement — too hot a path, and at intermediate
states ancestors get disassembled exposing children).

**Files changed**:
- `src/haz3lcore/tiles/Segment.re` — `rescan` function (stack-based)
- `src/haz3lcore/zipper/Siblings.re` — `rescan` wrapper
- `src/haz3lcore/zipper/Relatives.re` — `rescan_siblings`
- `src/haz3lcore/zipper/Zipper.re` — `rescan_reassemble`
- `src/haz3lcore/zipper/action/Insert.re` — wired in
- `src/haz3lcore/zipper/action/Destruct.re` — wired in

### Phase 2: Effective-Label Matching in Rescan — DONE

Two changes enable cross-form re-association:

**1. Singleton matching before incompleteness check**: The rescan now checks
singleton tiles (shard count == 1) for frame-matching BEFORE checking whether
they're incomplete. Previously, an orphaned shard like `->[1]` (label
`["fun","->"]`, incomplete because label length != shard count) would hit the
`!is_complete` branch first and push an empty frame, shadowing the useful frame
from `fix[0]`. Now it tries to match first, and only falls through to pushing
a frame if unmatched.

**2. Pre-splitting multi-shard orphans** (`presplit_orphans`): When the leading
delimiter is deleted from a multi-delimiter form (e.g., deleting `let` from
`let y = 1 in x`), the remaining shards stay as one multi-shard tile
(`T([let,=,in],[1,2])`). The rescan's singleton logic can't handle this.
`presplit_orphans` splits such tiles into individual singletons with children
extracted as flat segments: `T([let,=,in],[1]) ++ child ++ T([let,=,in],[2])`.
This runs in `Siblings.rescan` before the main rescan walk, on each side of
the cursor independently (to maintain the cursor split point).

This handles:
- `fix`/`fun` interchangeability: orphaned `->[1]` (effective label `["->"]`)
  matches `fix[0]`'s right-missing `->` frame entry
- `let`/`type` interchangeability: orphaned `=[1]` and `in[2]` match `type[0]`

### Phase 3: Open Design Questions

Three related but potentially independent problems:

**A. Backpack/rescan unification**: The backpack handles forward matching
(typing a new token that matches an ancestor's missing shard). The rescan
handles retroactive matching (existing tiles that should match a newly typed
incomplete tile). The ancestor case is intrinsically different (the incomplete
tile is a parent, not a sibling), but the sibling-level backpack logic overlaps
with rescan. Could the sibling portion of `backpack_find` be replaced by "always
create standalone + let rescan convert"? This might simplify the logic but needs
care around the ambiguity gate.

**B. Parent-breaking**: When a delimiter is trapped inside a bidelimited child
(e.g., `->` inside parens), the rescan can't reach it. Fixing this requires
breaking the parent tile to extract the delimiter. This involves the parent
(like ancestor matching in the backpack) but is structurally different — it's
breaking a complete ancestor, not filling in a missing shard.

**C. The full `fun (a, b -> )` scenario**: This combines all three sub-problems:
the ambiguity gate (preventing `->` from matching `fun` during left-to-right
typing), the lack of retroactive reassociation (once `->` is standalone), AND
parent-breaking (once `)` traps `->` inside parens). The arrow disambiguation
adds an extra wrinkle on top of the parent-breaking problem.

**D. Child remolding after parent formation** (possibly related): Pasting or
editing `casex | a => 0 end` then adding a space between `case` and `x` causes
`case` and `end` to combine (via rescan), trapping `| a => 0` as a child. But
`|` and `=>` were molded as Exp-sort standalone tokens and remain inert — they
don't become proper rule delimiters in the case's child sort. This may require
interplay between rescan (to form the parent tile) and remold (to re-mold
child content in the correct sort context). Needs investigation.

## Current Tests (Editing.Rescan section)

All rescan tests use `test_complete`, which checks:
1. Printer output matches expected string
2. **No incomplete tiles remain** anywhere in the zipped segment (recursive)

The "no incomplete tiles" predicate is the key discriminator — it detects
whether shards were properly reassociated, unlike printer output which looks
the same for matched and unmatched delimiters.

### Phase 1 tests (out-of-order typing)

- **fun/->**: Type `¦a -> x`, then `fun ` → `fun a -> x` with no incomplete tiles
- **let/=/in**: Type `¦= 1 in x`, then `let y ` → `let y = 1 in x`
- **if/then/else**: Type `¦a then b else c`, then `if ` → `if a then b else c`
- **Baselines**: Left-to-right `fun a -> x` and `let y = 1 in x`

### Phase 2 tests (cross-form re-association)

- **fix/fun**: Start with `fun a -> x`, delete `fun` (3 destructs), type `fix`
  → `fix a -> x` with orphaned `->[1]` re-associated to `fix`
- **type/let**: Start with `let y = 1 in x`, delete `let` (3 destructs), type
  `type` → `type y = 1 in x` with orphaned `=[1]` and `in[2]` re-associated

## Plan

### Next: Phase 3 — Design decisions

- Evaluate backpack/rescan unification
- Design parent-breaking mechanism
- Consider the full `fun (a, b -> )` scenario
