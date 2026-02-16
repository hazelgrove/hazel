# Pretty Printer for Hazel: Design Analysis & Options

## Problem Statement

Given a Hazel segment (or term) and a target line width, insert line breaks
(and adjust indentation) so that the rendered code does not exceed that width
and "looks good." This is needed for:
- Machine-generated code (e.g., from evaluation, code generation)
- The `hazel format` CLI command
- Potentially: editor reflow on window resize

The output must be a valid Hazel `Segment.t` with `Secondary(linebreak)` pieces
inserted at appropriate positions.

## Current Codebase State

### What exists

1. **`PrettySegment.re`** — Explicitly a **placeholder**. Comment reads: "ideally
   an algorithm would be implemented here... but that hasn't been implemented yet."
   Currently `pretty = Segment.t` and all combinators (`p_or`, `p_just`, etc.)
   are identity/no-ops.

2. **`ExpToSegment.re`** — Converts `Exp.t` → `Segment.t`. Already has:
   - An `inline: bool` setting (when `false`, inserts newlines after `let...in`,
     `type...in`, `if...then`, `case` rules, `;`)
   - `should_add_space` heuristic for spacing between tokens
   - `wrap_with_secondary` for preserving stored secondary in round-trip mode
   - Many `// TODO: Add optional newlines` comments throughout

3. **`Indentation.re`** — Computes indent levels for existing linebreaks based on
   surrounding tiles. Uses `is_incrementor` (tiles concave on right with 2+ labels,
   excluding `in`-terminated forms). Works on completed segments.

4. **`Printer.re`** — Renders `Segment.t` → `String`, adding indentation based
   on `Measured.re` output.

5. **`Measured.re`** — Computes positions (row, col) for every piece in a segment,
   handling line breaks, projectors, and indentation.

### The pipeline today

```
Exp.t --[ExpToSegment]--> Segment.t --[Printer.of_segment]--> String
                                  ^
                                  |
                          (PrettySegment is a no-op placeholder)
```

### Key data structures

- **`Segment.t`** = `list(Piece.t)` — the target representation
- **`Piece.t`** = `Tile(tile) | Grout(grout) | Secondary(secondary) | Projector(projector)`
- **`tile`** = `{ id, label: list(string), mold: Mold.t, shards, children: list(Segment.t) }`
- **`Mold.t`** = `{ out: Sort.t, in_: list(Sort.t), nibs: (Nib.t, Nib.t) }`
- **`Secondary.t`** = `{ id, content: Whitespace(string) | Comment(string) }`
- Line breaks are `Secondary({ content: Whitespace("\n"), ... })`
- Spaces are `Secondary({ content: Whitespace(" "), ... })`

### How forms work

Each language form has:
- A **label** (delimiter tokens): e.g., `["let", "=", "in"]`, `["if", "then", "else"]`
- A **mold** encoding sort, precedence, and shape (prefix/infix/postfix/operator)
- **Children** (segments between delimiters)

The segment tree structure is: pieces at each level are flat, but tiles contain
children (segments). So `let x = 5 in x + 1` is roughly:
```
[Tile{label:["let","=","in"], children:[pat_seg, exp_seg]}, ...body pieces...]
```

## Literature Survey

### The Lineage

1. **Oppen (1980)** — Imperative, streaming. Two parallel processes: scan
   (lookahead to compute block sizes) and print (decide breaks). O(n) time,
   O(linewidth) space. Foundational but imperative and hard to extend.

2. **Hughes (1995)** — Functional combinator library. Algebraic design with
   laws. Greedy layout algorithm. The `Doc` type represents sets of possible
   layouts. Key insight: combinators should satisfy algebraic laws.

3. **Wadler (1997/2003), "A Prettier Printer"** — Simplified Hughes with
   cleaner algebra. Core type:
   ```
   Doc = Nil | Text(str, Doc) | Line(indent, Doc) | Union(Doc, Doc)
   ```
   Key combinator: `group(x)` = try to flatten x onto one line; if it fits,
   do so; otherwise keep multi-line. `flatten` replaces all `Line`s with spaces.
   O(n) time via greedy algorithm. The `group` combinator is "all-or-nothing":
   either the entire group fits on one line, or none of it does.

4. **Leijen/Pottier (wl-pprint / PPrint)** — Practical Wadler implementations.
   Pottier's OCaml PPrint is the standard OCaml pretty-printing combinator
   library. Uses same core ideas but adapted for strict evaluation.

5. **Bernardy (2017), "A Pretty But Not Greedy Printer"** — Uses DP to find
   optimal layouts. Maintains Pareto frontier of non-dominated layouts (by
   width, last-line-width, height). Avoids greedy's failure case: sometimes
   breaking one group allows another to fit. Exponential worst case tamed by
   Pareto pruning. More complex, slower in practice.

6. **Pombrio (2024), "A Twist on Wadler's Printer"** — Replaces `group` with
   explicit `Choice(x, y)` + `Flat(x)`. More expressive (e.g., trailing commas
   in multi-line but not single-line). Same O(n) complexity. "The Rule": in
   `x | y`, shortest first line of y ≥ every first line of x.

7. **Podkopaev & Boulytchev (2023), "A Pretty Expressive Printer"** — Most
   expressive; provably optimal. More complex implementation.

### OCaml's Format module (Oppen-style)

Uses **boxes** (hv, h, v, hov, b) with **break hints**. Imperative, streaming.
Well-suited for OCaml but not combinator-based and doesn't produce a data structure.

### Practical formatters (ocamlformat, rustfmt, prettier)

- **Prettier (JS)**: Uses Wadler-Leijen Doc IR. Language-specific rules convert
  AST → Doc, then engine renders Doc → String. IR is key to separating
  language-specific decisions from layout engine.
- **ocamlformat**: Uses OCaml's Format module under the hood, with extensive
  AST-walking rules.
- **rustfmt/gofmt**: More opinionated, less combinator-based.

## Analysis: Where to Intervene

### Option A: Operate on Segment directly (no intermediate representation)

**How it works**: Walk the segment, compute widths, insert `Secondary(linebreak)`
pieces when content would exceed target width.

**Advantages**:
- No new types needed
- Direct manipulation of the actual output representation
- Can leverage existing `Measured.re` for width computation
- Can leverage existing `Indentation.re` for indent levels
- Conceptually simple: "insert line breaks into this flat list"

**Disadvantages**:
- Width computation requires a measurement pass first
- Hard to express "try flat, fall back to broken" without lookahead
- Decisions are coupled: inserting one break changes widths downstream
- Harder to get "nice" results for nested structures
- No way to express alternative layouts (the core idea of all good pretty printers)

**Verdict**: Possible for a very simple version, but likely to produce mediocre
results. The lack of choice/group semantics means we can't do "fit on one line
if possible, otherwise break" in a principled way.

### Option B: Operate on Term (Exp.t) level

**How it works**: Walk the term tree, make layout decisions, pass hints to
`ExpToSegment` which inserts the appropriate secondary.

**Advantages**:
- Clean tree structure, easy to reason about
- Already have `ExpToSegment` which converts term → segment
- Precedence information is explicit in the term structure
- Can use molds/labels to drive decisions

**Disadvantages**:
- Terms lose some structural info (segment is closer to concrete syntax)
- Round-trip concern: term→segment→term isn't always identity
- Have to deal with `ExpToSegment`'s existing spacing heuristics
- Term level doesn't see segments directly (e.g., user-entered comments)

**Verdict**: Reasonable but somewhat disconnected from the actual output.

### Option C: Wadler-style Doc IR (intermediate representation)

**How it works**: Define a `Doc.t` type. Convert Segment (or Term) → Doc.
Layout engine converts Doc → Segment (with line breaks). Separate concerns:
_what_ to pretty-print (Doc construction) from _how_ (layout algorithm).

**Core Doc type** (adapted for Hazel):
```reasonml
type doc =
  | Empty
  | Text(Piece.t)              /* A single non-secondary piece */
  | Break(string)              /* Space if flat, newline+indent if broken */
  | Concat(doc, doc)           /* Sequential composition */
  | Nest(int, doc)             /* Increase indent for line breaks within */
  | Group(doc)                 /* Try flat; if doesn't fit, use breaks */
  | HardLine                   /* Always break (e.g., after "in" in let) */
  | FlatChoice(doc, doc)       /* Explicit choice: flat vs broken layout */
```

**Advantages**:
- Clean separation of concerns
- Well-understood algorithm with good theoretical properties
- O(n) greedy layout is fast and produces good results
- `Group` gives us the critical "fit on one line?" semantics
- Easy to add language-specific rules (just change Doc construction)
- Can support both "ignore user breaks" and "preserve user breaks" modes
- Literature-backed: decades of refinement
- `PrettySegment.re` was literally designed as the placeholder for this

**Disadvantages**:
- New type to maintain
- Two conversion steps: Segment → Doc → Segment (or Term → Doc → Segment)
- Need to measure piece widths (straightforward: labels are known, tokens have
  known widths)

**Verdict**: The principled approach. This is what the PL community has converged
on, and what `PrettySegment.re` was designed to support.

### Option D: Hybrid — operate on Segment with group annotations

**How it works**: Don't create a separate Doc type. Instead, annotate the
existing walk of the segment with "group" boundaries, using the mold/label
structure to determine where groups are. Use a Wadler-style greedy algorithm
but directly on the segment structure.

**How groups are determined** (key insight for Hazel):
- Each **tile with children** is a natural group boundary
  - `let x = 5 in ...` → group the whole let, with breaks after `=` and before body
  - `if c then t else e` → group with breaks after `then` and `else`
  - `(...)` → group the contents
  - `[...]` → group the contents
  - `case...end` → group with break before each `|`
- Each **infix operator sequence** can be a group
  - `a + b + c` → try flat, break before operators if too wide
- **Comma-separated items** are a group
  - `(a, b, c)` → try flat, break after each comma

**Advantages**:
- No new Doc type; leverage mold structure directly
- Molds tell us almost everything: compound forms (2+ labels) are groups,
  infix ops have known precedence, etc.
- Fewer conversion steps
- Still gets the benefit of group/flatten semantics
- Language-specific decisions are driven by molds + a few label checks

**Disadvantages**:
- Slightly more coupled than pure Doc approach
- Harder to unit test the layout algorithm independently
- Mold-based grouping might miss some cases

**Verdict**: A pragmatic middle ground. Gets most benefits of the Doc approach
with less infrastructure.

## Recommendation: Option C (Doc IR), implemented in PrettySegment.re

### Why

1. **PrettySegment.re already exists as a placeholder** for exactly this purpose.
   The module defines `type pretty = Segment.t` and has no-op combinators. The
   intent was always to implement a real algorithm here.

2. **The Doc type is small and well-understood.** About 7-8 constructors. The
   layout algorithm is ~50 lines. This is not a large amount of new code.

3. **Clean separation** means we can:
   - Test the layout engine independently
   - Change language-specific rules without touching the engine
   - Support multiple output modes (ignore user breaks, preserve user breaks)

4. **The conversion from Segment/Term → Doc can leverage molds.** We can write
   a generic function that examines each tile's mold to determine group
   boundaries:
   - Compound forms (label length ≥ 2) create groups with breaks between children
   - Infix operators within sequences create groups
   - A small number of label-specific rules handle special cases

5. **O(n) greedy is perfectly adequate** for our use case. We're not
   typesetting books; we're formatting programs. Greedy produces good results
   for code, and the cases where it fails (greedy breaks a group that would
   have fit if another group had broken first) are rare in practice.

### Proposed Architecture

```
                    ┌─────────────────┐
Segment.t ─────────►  segment_to_doc  │
(or Exp.t → Segment)│  (uses molds +  │
                    │   label rules)  │
                    └────────┬────────┘
                             │ Doc.t
                    ┌────────▼────────┐
                    │   layout(doc,   │
                    │    width)       │
                    │  (Wadler-style  │
                    │   greedy)       │
                    └────────┬────────┘
                             │ Segment.t
                    ┌────────▼────────┐
                    │  Printer.re /   │
                    │  Indentation.re │
                    │  (existing)     │
                    └────────┬────────┘
                             │ String
```

### Implementation Plan

#### Phase 1: Doc type and layout engine (~200 lines)

In `PrettySegment.re`:

```reasonml
type doc =
  | Empty
  | Piece(Piece.t)           /* Emit this piece as-is */
  | Break                    /* Space if flat, newline if broken */
  | HardBreak                /* Always newline */
  | Cat(doc, doc)            /* Concatenation */
  | Nest(int, doc)           /* Indent breaks within by n */
  | Group(doc);              /* Try flat first */

/* Flattened doc for "flat mode" checking */
type sdoc =
  | SEmpty
  | SText(Piece.t, sdoc)
  | SLine(int, sdoc);        /* newline + indent spaces */

/* Width of a piece (for layout decisions) */
let piece_width: Piece.t => int;

/* Core layout: greedy, Wadler-style */
let layout: (~width: int, doc) => Segment.t;
```

The layout algorithm uses a work-list with mode (Flat | Break) tracking:
```
layout(width, doc):
  fits(remaining_width, doc) → bool  (lookahead: does flat rendering fit?)
  best(column, mode_stack, doc) → sdoc  (greedy choice at each Group)
  sdoc_to_segment(sdoc) → Segment.t  (convert to final output)
```

#### Phase 2: Segment → Doc conversion (~150 lines)

```reasonml
/* Convert segment to Doc, using mold information for layout hints */
let segment_to_doc: Segment.t => doc;
```

Key rules (driven by molds + labels):
1. **Tiles with children** (compound forms):
   - `label.length >= 2` → `Group(...)` with `Break` between children
   - Special: `let...=...in` → `Nest(2, ...) + HardBreak` before body
   - Special: `if...then...else` → breaks around `then`/`else` children
   - Special: `case...end` → `HardBreak` before each `|...=>`
   - Special: `fun...->` → break before body
2. **Infix operators** (`mold.is_infix_op`):
   - Group sequences of same-precedence infix ops
   - Break before operator
3. **Paired delimiters** (`(...)`, `[...]`):
   - Group contents; `Nest(2, Break + contents + Break)`
4. **Commas** (`,`):
   - Break after comma
5. **Semicolons** (`;`):
   - `HardBreak` after semicolon (cell boundaries are always line-separated)

#### Phase 3: Integration

1. Wire `PrettySegment.pretty` type to be `Doc.t` instead of `Segment.t`
2. Update `ExpToSegment.re` combinators (`p_or`, `p_just`, etc.) to build
   `Doc.t` values — OR keep them as-is and add a `segment_to_doc` pass
3. Add `--width` flag to `hazel format` CLI command
4. Optionally: mode flag for "preserve user breaks" vs "reformat completely"

### Mold-based heuristics (the lightweight language-specific part)

The key insight for Hazel: **most formatting decisions can be derived from the
mold and a few label patterns**, without a giant per-form switch statement.

```reasonml
let classify_tile = (t: Tile.t) => {
  let n_labels = List.length(t.label);
  let (l_nib, r_nib) = Tile.nibs(t);
  switch () {
  | _ when n_labels >= 2 && l_nib.shape == Convex && r_nib.shape == Concave(_) =>
    /* Prefix compound form: let...=...in, fun...->>, if...then...else */
    PrefixCompound
  | _ when n_labels >= 2 && l_nib.shape == Convex && r_nib.shape == Convex =>
    /* Operator compound form: (...)  [...], case...end, test...end */
    OperatorCompound
  | _ when n_labels >= 2 && l_nib.shape == Concave(_) && r_nib.shape == Convex =>
    /* Postfix compound form: f(...) */
    PostfixCompound
  | _ when n_labels == 1 && Mold.is_infix_op(t.mold) =>
    InfixOp
  | _ when n_labels == 1 && Mold.is_prefix_op(t.mold) =>
    PrefixOp
  | _ =>
    Atomic
  };
};
```

Only a few label-specific overrides needed:
- `["let", "=", "in"]` → hard break before body
- `[";"]` → hard break after
- `[","]` → soft break after
- `["case", "end"]` → hard break before each rule
- `["|", "=>"]` → hard break after (unless short)

### Performance

- **Doc construction**: O(n) — single pass over segment
- **Layout**: O(n) — Wadler's greedy algorithm
- **Total**: O(n) where n = number of pieces in segment
- Should be sub-millisecond for typical programs

### Width of a piece

```reasonml
let piece_width = (p: Piece.t) => {
  switch (p) {
  | Tile(t) =>
    /* Sum of label token widths + children widths (in flat mode) */
    List.fold_left((+), 0, List.map(String.length, t.label))
    + List.fold_left((+), 0, List.map(segment_flat_width, t.children))
  | Grout(_) => 1
  | Secondary(s) => Secondary.length(s)
  | Projector(_) => 10  /* approximate */
  };
};
```

## Alternative: Simpler "Insert Breaks" Approach (Option D)

If the Doc IR feels like too much infrastructure, a simpler approach:

### How it works

1. Compute the flat width of the segment using a pass similar to `Measured.re`
2. Walk the segment with a "current column" tracker
3. At each "break opportunity" (determined by mold analysis), check:
   - Would the next chunk fit on this line?
   - If not, insert a `Secondary(linebreak)` and reset column to indent level
4. Break opportunities are: before/after tile children, after commas, after
   semicolons, before infix operators in long sequences

### Pros
- No new types at all
- Very simple to implement (~100 lines)
- Gets 80% of the benefit

### Cons
- No "try flat first" semantics — can't group things
- Greedy in a weaker sense: just "does the next token fit?"
- Produces worse output for nested structures
- Harder to get right for things like: should `let x = very_long_expression in`
  break after `=` or put the whole let on its own?

### Recommendation

Start with **Option C (Doc IR)** because:
1. `PrettySegment.re` was designed for it
2. It's ~350 lines total, not significantly more than the simple approach
3. It produces significantly better output
4. It's well-understood and battle-tested in the literature
5. It cleanly separates layout decisions from the layout engine

If Option C proves too complex in practice, we can fall back to Option D as
an MVP.

## Implementation Status

Option C (Doc IR) has been implemented in `PrettySegment.re`. The implementation
uses a Wadler/Lindig-style Doc IR with a greedy O(n) layout algorithm.

### Doc IR

```reasonml
type doc =
  | Empty
  | Piece(Piece.t, int)  /* piece with pre-computed flat width */
  | Space                /* always emits a space */
  | Break                /* space if flat, newline if broken */
  | HardBreak            /* always a newline */
  | Cat(doc, doc)        /* concatenation */
  | Group(doc);          /* try flat first; if doesn't fit, use breaks */
```

### Integration Points

- **Cmd+S in web editor**: Triggers `PrettyPrint` action → `Perform.re` →
  `Zipper.unselect_and_zip(z)` → `PrettySegment.prettify` → `Zipper.unzip`.
  Preserves refractors (probes/projectors).
- **CLI**: `hazel format [--width N] file.hz` (default width 80)
- **Tests**: 26 tests in `Test_PrettyPrint.re` covering flat, breaking,
  delimiters, case expressions, and complex combinations.

---

## Formatting Policy (Current Decisions)

### Structural Rules (always break, regardless of width)

These use `HardBreak` in the doc IR, which always produces a newline:

1. **Let-chains**: Consecutive compound prefix forms (`let...in`, `fun...->`,
   `if...then...else`) at the same segment level always break between each
   other. The last compound prefix before a non-compound body uses a `Break`
   (width-dependent) wrapped in its own `Group` for independent flat/broken
   decisions.

   ```
   let x = 1 in    ← HardBreak (always)
   let y = 2 in    ← HardBreak (always)
   let z = x + y in z * 2   ← Break (width-dependent, via Group)
   ```

2. **Case rules**: Case rules (`| pat => body`) always appear on separate
   lines. HardBreak between the scrutinee and first rule, and between
   consecutive rules. Each rule's body can independently go flat or break.

   ```
   case x          ← HardBreak before first rule
   | 0 => "zero"   ← HardBreak between rules
   | 1 => "one"
   end
   ```

3. **Semicolons**: Semicolons always produce a newline after (cell boundaries).

### Width-Dependent Rules (break only when doesn't fit)

These use `Break` in the doc IR, which becomes a space in flat mode or a
newline in breaking mode. `Group` wrappers determine flat vs broken.

1. **Single compound prefix + body**: `let x = 5 in x + 1` stays flat when
   it fits. When it doesn't, break after the prefix, body in its own Group.

2. **Infix operators**: All-or-nothing breaking within a sequence. Either
   the entire chain `a + b + c` fits on one line, or each operator gets its
   own line (break before operator).

3. **Comma-separated items**: All-or-nothing. Either `(a, b, c)` fits flat,
   or each item gets its own line (break after comma).

4. **Tile children**: When a tile (e.g., `if...then...else`) is too wide for
   flat layout, its children break at boundaries defined by
   `child_break_style`.

### Post-Processing Rules

1. **Tight function application**: Whitespace (both spaces and newlines)
   between a convex-right piece and a following paren/bracket piece is
   removed. This ensures `f(x)` never becomes `f (x)` or `f\n(x)`.

2. **Blank line preservation**: Blank lines (2+ consecutive newlines) in the
   original segment are detected before formatting and re-inserted afterward.
   This preserves user-intended paragraph breaks between definitions. Only
   works when formatting from the original segment (Cmd+S path), not when
   reconstructing from AST (CLI format path).

### Key `fits` Behavior

The `fits` function determines if a Group can go flat:
- `(Flat, HardBreak)` → **false**: Any Group containing a HardBreak is forced
  to go Breaking. This prevents HardBreaks from causing Breaks in the same
  Group to become spaces.
- `(Breaking, HardBreak)` → **true**: Line is done, next content starts fresh.

### Multiline Children

When a tile's children contain newlines (from structural breaks in recursive
formatting), the tile skips the flat attempt and goes directly to breaking
mode. This ensures compound tiles like `case...end` properly break when
their child content has structural breaks.

---

## Formatting Design Options (Open Questions)

### Trailing Delimiter Style (**config candidate**)

Current: `end` stays on the same line as the last case rule.

```
case x
| 0 => "zero"
| 1 => "one" end
```

This is controlled by `child_break_style` for `["case", "end"]`: `(false, false)`.
Changing the second value to `true` puts `end` on its own line.

Options:
- **Same line** (current): `| _ => body end`. More compact.
- **Own line**: `end` on a line by itself. Clearer block structure.

This is a strong candidate for a configuration option. The `in` keyword
(end of `let...=...in` tiles) could be correlated — both are trailing
keywords that close a block. Currently `in` stays on the same line as the
end of its binding, which is the natural behavior. A "trailing keywords on
own line" config could flip both `end` and potentially affect how `in`
bodies are laid out.

### Sum Type Constructor Layout

Currently width-dependent. Options:
- **Always vertical for 3+ constructors**: More readable for type definitions.
- **Width-dependent** (current): May inline short definitions.
- Needs more investigation with real programs.

### Comment Handling

Currently, comments on their own line may merge with the following code line.
This is because `strip_whitespace` removes the newline between the comment
and the next piece. Needs investigation — may need special handling to detect
standalone comments and preserve their line boundary.

Options:
- Detect comment pieces and always HardBreak after them
- Preserve the whitespace around comments during strip_whitespace
- Treat comments as structural breaks

### Opening Delimiter Placement After Breaks

When a binding's body starts with a delimiter (parens, brackets), and the
body breaks to a new line, the opening delimiter ends up alone on an
indented line:

```
let result =
    (
        alpha,
        beta,
        gamma,
        delta
    ) in
result
```

An alternative is "cuddle" style, where the opening delimiter stays on the
same line as the `=`, and the closing delimiter outdents:

```
let result = (
    alpha,
    beta,
    gamma,
    delta
) in
result
```

The cuddle style is common in JavaScript/TypeScript (objects after `=` or
in function arguments) and avoids using a line just for `(`. The current
style is a natural consequence of the algorithm — the body is a single
Group that either goes flat or breaks as a whole, and when it breaks, the
entire body (including the `(`) moves to the next line.

Implementing cuddle style would require the layout engine to detect when
the first token of a broken body is an opening delimiter, and keep it
attached to the preceding line while still indenting the contents. This
is non-trivial in Wadler-style formatters.

Options:
- **Current** (body on own line): Simple, consistent. The `(` on its own
  line is the cost of uniformity.
- **Cuddle style**: More compact, familiar to JS/Python developers. Would
  require special-case logic or a new combinator (e.g., `SoftIndent` that
  only indents after the first break).
- **Hybrid**: Only cuddle for specific contexts (let bindings, function
  args) where it's most natural.

### Preserve User Breaks Mode

The formatter currently normalizes all whitespace. A "preserve user breaks"
mode could keep existing line breaks and only add new ones where needed.
Blank line preservation is a first step toward this.

### Width Configuration

- Default width: 60 columns (both CLI `--width` and `PrettySegment.prettify`)
- Configurable via `--width` / `-w` CLI flag
- Web editor: could use editor viewport width or a fixed setting
- Indent amount: currently 4 spaces (via `child_width = width - 4`).
  The Printer uses the `~indent` parameter (default `"  "` = 2 spaces) for
  rendering. These should be coordinated.
