# Secondary in Terms: Preserving Formatting Through Round-Trips (v2)

## Problem Statement

Converting between segments and terms loses formatting information:

```
Segment → MakeTerm → Term → ExpToSegment → Segment'
```

The round-trip `Segment' ≠ Segment` because:

1. **MakeTerm filters secondary**: `Segment.skel` filters out secondary pieces before skeleton construction
2. **ExpToSegment generates new formatting**: It creates segments using heuristic spacing rules
3. **No secondary storage in terms**: `IdTag.t` only stores `ids: list(Id.t)`

**Goal:** Enable exact round-tripping by storing secondary in term annotations.

## Design

### Outer Secondary Model

Each term stores the secondary that appears *outside* it—the secondary immediately adjacent to the term from its parent's perspective. This is in contrast to an "inner" model where terms store secondary created by their internal structure.

**Key property:** Every term has exactly 2 runs: `[before, after]`.

- `before`: secondary immediately before this term (after preceding delimiter or sibling)
- `after`: secondary immediately after this term (before following delimiter or sibling)

### Extending IdTag.t

```reason
// In IdTagged.re
module IdTag = {
  type t = {
    ids: list(Id.t),
    secondary: (list(Secondary.t), list(Secondary.t)),  // (before, after)
  };

  let fresh = () => {ids: [Id.mk()], secondary: ([], [])};
};
```

This is backwards compatible: existing code creates terms with empty secondary `([], [])`.

### Why Outer Secondary?

1. **Uniform representation**: Every term has exactly 2 runs, regardless of form type. An inner model would require a `list(list(Secondary.t))` where the length varies by form—binary ops need 2 runs, let needs 5, and n-ary forms like tuples, list literals, and matches need a variable number depending on element count.
2. **Leading/trailing whitespace**: Naturally captured on the root term. An inner model for atomic (convex) terms would have nowhere to store leading/trailing whitespace since they have no internal structure.
3. **Simpler absorption**: No special logic needed for ListLit/Match—children already own their boundary secondary.
4. **Intuitive extraction**: Secondary "travels with" the term when extracted.

## Worked Examples

Notation: `·` = space, `↵` = line break, `#...#` = comment

### Binary Operation: `1·+·2`

| Term | Secondary (before, after) |
|------|---------------------------|
| `1` | `([], [·])` |
| `2` | `([·], [])` |
| `1 + 2` | `([], [])` |

The spaces are owned by the operands, not the operator.

**Reconstitution:**
```
BinOp.before + 1.before + "1" + 1.after + "+" + 2.before + "2" + 2.after + BinOp.after
= [] + [] + "1" + [·] + "+" + [·] + "2" + [] + []
= 1·+·2 ✓
```

### Let Expression: `let·x·=·1·in↵x`

| Term | Secondary (before, after) |
|------|---------------------------|
| `x` (pat) | `([·], [·])` — after `let`, before `=` |
| `1` | `([·], [·])` — after `=`, before `in` |
| `x` (body) | `([↵], [])` — after `in`, end of term |
| Let | `([], [])` — outside (root) |

**Reconstitution:**
```
Let.before + "let" + pat.before + "x" + pat.after + "=" + def.before + "1" + def.after + "in" + body.before + "x" + body.after + Let.after
= [] + "let" + [·] + "x" + [·] + "=" + [·] + "1" + [·] + "in" + [↵] + "x" + [] + []
= let·x·=·1·in↵x ✓
```

### Bare Tuple: `1,·2,·3`

| Term | Secondary (before, after) |
|------|---------------------------|
| `1` | `([], [])` — before `,` |
| `2` | `([·], [])` — after `,`, before `,` |
| `3` | `([·], [])` — after `,` |
| Tuple | `([], [])` — outside |

**Reconstitution:**
```
Tuple.before + 1.before + "1" + 1.after + "," + 2.before + "2" + 2.after + "," + 3.before + "3" + 3.after + Tuple.after
= [] + [] + "1" + [] + "," + [·] + "2" + [] + "," + [·] + "3" + [] + []
= 1,·2,·3 ✓
```

### Parenthesized Tuple: `(1,·2,·3)`

| Term | Secondary (before, after) |
|------|---------------------------|
| `1` | `([], [])` |
| `2` | `([·], [])` |
| `3` | `([·], [])` |
| Tuple | `([], [])` — inside parens, outside tuple |
| Parens | `([], [])` — outside parens |

**Reconstitution:**
```
Parens.before + "(" + Tuple.before + 1.before + "1" + 1.after + "," + ... + Tuple.after + ")" + Parens.after
= (1,·2,·3) ✓
```

### List Literal: `[1,·2,·3]`

| Term | Secondary (before, after) |
|------|---------------------------|
| `1` | `([], [])` — after `[`, before `,` |
| `2` | `([·], [])` — after `,`, before `,` |
| `3` | `([·], [])` — after `,`, before `]` |
| ListLit | `([], [])` — outside |

**Reconstitution:**
```
ListLit.before + "[" + 1.before + "1" + 1.after + "," + 2.before + "2" + 2.after + "," + 3.before + "3" + 3.after + "]" + ListLit.after
= [] + "[" + [] + "1" + [] + "," + [·] + "2" + [] + "," + [·] + "3" + [] + "]" + []
= [1,·2,·3] ✓
```

Note: No special absorption logic needed. The elements own their surrounding secondary directly.

### Case Expression: `case·x↵|·1·=>·a↵|·2·=>·b↵end`

| Term | Secondary (before, after) |
|------|---------------------------|
| `x` (scrutinee) | `([·], [↵])` — after `case`, before `\|` |
| `1` (pat1) | `([·], [·])` — after `\|`, before `=>` |
| `a` (body1) | `([·], [↵])` — after `=>`, before `\|` |
| `2` (pat2) | `([·], [·])` — after `\|`, before `=>` |
| `b` (body2) | `([·], [↵])` — after `=>`, before `end` |
| Match | `([], [])` — outside |

**Reconstitution:**
```
Match.before + "case" + scrut.before + "x" + scrut.after
  + "|" + pat1.before + "1" + pat1.after + "=>" + body1.before + "a" + body1.after
  + "|" + pat2.before + "2" + pat2.after + "=>" + body2.before + "b" + body2.after
  + "end" + Match.after
= case·x↵|·1·=>·a↵|·2·=>·b↵end ✓
```

### Leading/Trailing Whitespace: `··x··`

| Term | Secondary (before, after) |
|------|---------------------------|
| `x` | `([··], [··])` |

The root term captures leading and trailing whitespace naturally. This was impossible with the inner approach for convex (atomic) root terms.

## Implementation Plan

Each phase corresponds to a commit. This allows incremental progress and easy backtracking if needed.

### Phase 1: Extend IdTag.t

Add the `secondary` field with default `([], [])`:

```reason
type t = {
  ids: list(Id.t),
  secondary: (list(Secondary.t), list(Secondary.t)),
};

let fresh = () => {ids: [Id.mk()], secondary: ([], [])};
```

**Testing:** Compiles cleanly. All existing tests pass (empty secondary is valid).

### Phase 2: Secondary Collection Function

Create a function to collect outer secondary for each term during parsing:

```reason
let collect_outer_secondary: (Segment.t, Skel.t) => Id.Map.t((list(Secondary.t), list(Secondary.t)))
```

For each term in the skeleton, this determines:
- `before`: secondary between the preceding delimiter/sibling and this term
- `after`: secondary between this term and the following delimiter/sibling

**Approach:** Modify `Segment.skel` to track secondary positions during filtering. As we identify where each term sits in the segment, we can determine the secondary on either side.

**Testing:** Unit tests using the worked examples above.

### Phase 3: MakeTerm Integration

Modify MakeTerm to:
1. Call secondary collection after skeleton construction
2. For each term, look up its `(before, after)` secondary and store in the annotation

Since children own their own boundary secondary, no special absorption logic is needed for ListLit or Match.

**Testing:** Existing tests still pass. New tests verify secondary is collected correctly.

### Phase 4: ExpToSegment Integration

Modify ExpToSegment to emit secondary around each term:

```reason
let emit_term = (term) => {
  let (before, after) = term.annotation.secondary;
  secondary_to_segment(before) @ term_content(term) @ secondary_to_segment(after)
};
```

When secondary is empty `([], [])`, fall back to current heuristic spacing.

**Testing:** Terms without secondary get current behavior. Terms with secondary round-trip correctly.

### Phase 5: Round-Trip Tests

Property-based tests:

```reason
let test_roundtrip = (seg: Segment.t) => {
  let term = MakeTerm.go(seg);
  let seg' = ExpToSegment.exp_to_segment(term);
  Segment.equal(seg, seg');
};
```

Start simple (atoms, binary ops), progress to complex (nested expressions, n-ary forms). Include tests with leading/trailing whitespace.

## File Changes

| File | Change |
|------|--------|
| `IdTagged.re` | Add `secondary` field as `(before, after)` pair |
| `Segment.re` | Add function to collect outer secondary positions |
| `MakeTerm.re` | Store outer secondary in term annotations |
| `ExpToSegment.re` | Emit secondary around terms when present |
| `test/` | Add round-trip tests |

## Notes

### Tuples Are N-ary Infix

Commas are infix operators at the tile level, but at the Skel and term level, chained commas form a single n-ary Tuple. With outer secondary, each tuple element owns its surrounding secondary—no special tuple logic needed.

### Forms Considered But Not Requiring Special Handling

**Deferral / DeferredAp:** When `_` appears inside function application, the term becomes `DeferredAp` instead of `Ap`. The outer secondary model handles this the same as regular Ap—each child owns its `(before, after)`.

**TupLabel:** Standalone `x=1` parses as `TupLabel` then gets wrapped in a synthetic `Tuple([...])`. The TupLabel's children own their outer secondary; the synthetic wrapper has `([], [])`.

**ListLit / Match Absorption:** With outer secondary, absorption becomes simpler. When a Tuple is absorbed into ListLit, the tuple elements already own their boundary secondary. No special combining logic is needed—just use the children's secondary directly.

### Syntax Transformation Implications

With outer secondary, secondary "travels with" the term:

- **Extracting** a term: Secondary comes along
- **Replacing** a term: May want to transfer the old term's secondary to the new term
- **Inserting** a new term: Need to provide appropriate secondary

For transformations where you want to *discard* surrounding whitespace, explicitly set secondary to `([], [])`.

### Tuple Parenthesization

ExpToSegment auto-parenthesizes tuples. For round-tripping, add a flag to disable this behavior.

### ID Preservation

Round-tripping should preserve IDs exactly since they're stored in the annotation.

### Normalization

Preserve secondary exactly during collection. Normalization (e.g., collapsing multiple spaces) can be a separate optional pass.
