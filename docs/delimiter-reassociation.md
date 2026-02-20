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

### The Guiding Property (work in progress)

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
2. S' is **bisimilar** to `freshParse(print(S'))` — they produce the same
   observable behavior under all future single-character edits

This is weaker than H: it allows S' and `freshParse(print(S'))` to have
different internal structure, as long as they behave identically. Internal
differences are "distinctions without a difference" if the user can never
observe them through any sequence of edits.

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
whether parent-breaking is needed for some cases. Also: precisely defining
"bisimilar" in this context (what counts as observable behavior?).

### Open Design Questions

**A. Backpack/rescan unification**: The backpack handles forward matching
(typing a new token that matches an ancestor's missing shard). The rescan
handles retroactive matching (existing tiles that should match a newly typed
incomplete tile). The ancestor case is intrinsically different (the incomplete
tile is a parent, not a sibling), but the sibling-level backpack logic overlaps
with rescan. Could the sibling portion of `backpack_find` be replaced by "always
create standalone + let rescan convert"? This might simplify the logic but needs
care around the ambiguity gate.

**B. Parent-breaking — status unclear**: When a delimiter is trapped inside a
bidelimited child (e.g., `->` inside parens in `fun (a, b -> )`), the rescan
can't reach it. Recovery is possible via delete-then-retype:

> Starting from `fun (a, b -> a)`: insert `)` after `b` (backpack matches
> it with `(`), move to end, delete old `)`. The rescan matches `fun` with
> `->`. Result: `fun (a, b) -> a`. Two edits, correct structure. (Tested.)

Parent-breaking would give H (history independence) for this case — inserting
`)` would automatically displace the old match. Without it, the intermediate
state after just the insert may differ from a fresh parse. The question for T
(text-edit faithfulness) is: does this intermediate difference matter? Is the
state after inserting `)` bisimilar to a fresh parse of the same text? Our
one observation suggests it might be (the backpack matched the new `)` with
`(` immediately), but this needs more investigation.

UX tension: matched parens represent user intent. Inserting `)` inside them
could mean "create nested parens" rather than "break the existing match." The
left-to-right parse would re-match, but the user's intent is ambiguous.

**Ambiguity gate note**: The gate (only checking the backpack head for tokens
like `->` that are both standalone forms and multi-token delimiters) is
load-bearing. Example: `fun f : (Int -> Bool) -> x`. After typing `fun f : (`,
the backpack is [`(`'s `)`, `fun`'s `->`]. When `->` is typed (for the type
arrow), the gate checks only the head (`)`) — no match — so it correctly stays
standalone. Without the gate, it would incorrectly match `fun`'s `->`. This
means the gate can't simply be relaxed to solve the `fun (a, b -> )` problem.

**C. Child remolding after parent formation**: Editing `casex | a => 0 end`
then adding a space between `case` and `x` causes `case` and `end` to combine
(via rescan), trapping `| a => 0` as a child. But `|` and `=>` were molded as
Exp-sort standalone tokens and remain inert — they don't become proper rule
delimiters in the case's child sort. This may require interplay between rescan
(to form the parent tile) and remold (to re-mold child content in the correct
sort context). Needs investigation.

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
