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

Start simple (atoms, binary ops), progress to complex (nested expressions, n-ary forms). Include tests with leading/trailing whitespace. Eventually we want property-based tests; check out some of the other tests in the codebase for inspiration here.

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

---

## Addendum: Learnings from Initial Implementation

### Completed Work (Phases 1-3)

Phases 1-3 were implemented successfully:
- `IdTag.t` extended with `secondary: (list(Secondary.t), list(Secondary.t))`
- `Segment.SecondaryCollection` module created to collect outer secondary
- `MakeTerm` integrated to populate term annotations during parsing

### Phase 4 Correction: ExpToSegment Design

The original Phase 4 description said:
> When secondary is empty `([], [])`, fall back to current heuristic spacing.

This was a flawed design. The problem: empty secondary `([], [])` is meaningful—it means "the user wrote no whitespace here." Falling back to heuristics for empty secondary means `1+2` becomes `1 + 2`, which breaks round-tripping.

**Correct approach:** ExpToSegment needs two clearly separated modes:

1. **PreserveExact**: Use exactly what's stored in term annotations
   - Empty secondary `([], [])` means emit nothing
   - Don't use the heuristic `@` operator—use plain list concatenation
   - Ignore `inline` and other auto-formatting settings

2. **AutoFormat**: Generate secondary heuristically (current/original behavior)
   - Use the heuristic `@` operator that adds spaces between pieces
   - `inline` setting controls newline insertion
   - This is what ExpToSegment did before this work

### Revised ExpToSegment.Settings

```reason
module Settings = {
  type secondary_handling =
    | PreserveExact   // Round-trip: use exactly what's stored
    | AutoFormat;     // Display: generate heuristically

  type t = {
    secondary: secondary_handling,
    inline: bool,  // Only applies when secondary = AutoFormat
    fold_case_clauses: bool,
    fold_fn_bodies: [`Fold | `Text | `NoFold],
    hide_fixpoints: bool,
    show_filters: bool,
    show_unknown_as_hole: bool,
  };
};
```

**Key insight:** The `inline` setting (and potentially other auto-formatting options) only makes sense with `AutoFormat`. With `PreserveExact`, we use exactly what's in the term—no heuristic decisions.

**Note on folding options:** `fold_case_clauses`, `fold_fn_bodies`, `hide_fixpoints` are about projector display, not secondary/whitespace. They remain orthogonal to `secondary_handling`. For round-trip tests, we'll set these to their non-folding defaults.

### Implementation Strategy for PreserveExact

When `secondary = PreserveExact`:

1. **Don't use heuristic `@`**: The custom `@` operator in PrettySegment adds spaces between pieces. With `PreserveExact`, use `List.append` (or `Stdlib.(@)`) instead.

2. **Emit stored secondary unconditionally**:
   ```reason
   let emit_with_secondary = (term, content) => {
     let (before, after) = term.annotation.secondary;
     secondary_to_segment(before) @ content @ secondary_to_segment(after)
   };
   ```
   Even when `before` and `after` are empty lists, this is correct—it emits nothing.

3. **Thread the mode through**: The `secondary_handling` setting needs to affect how segments are joined throughout the pretty-printing process, not just at the `wrap` stage.

### Phase 5 Revision: Testing Strategy

**Unit tests for each syntactic form:**
- Test with standard spacing (spaces around operators, after commas)
- Test with no spacing (compact: `1+2`, `(1,2,3)`)
- Test with extra spacing (`1  +  2`)
- Test with newlines where applicable
- Use `{| |}` string syntax for multi-line tests

**Test organization:**
- Passing tests: forms that round-trip correctly
- Skipped tests: known limitations (document why)
- Build up incrementally: atoms → binary ops → let/fun → tuples/lists → case → nested

**Known limitations to document:**
- Projectors won't round-trip (future work)
- Any edge cases discovered during testing

**Eventual goal:** Property-based tests that verify arbitrary segments round-trip, but start with comprehensive unit tests first.

### Files Still Requiring Changes

| File | Change |
|------|--------|
| `ExpToSegment.re` | Add `secondary_handling` type; implement `PreserveExact` mode with plain concatenation |
| `Test_ExpToSegment.re` | Refactor tests to use new settings; add comprehensive per-form tests |

### Phase 4 Completed: Selective Collection for Compound Expressions

During testing, we discovered that chained binary operations like `1 + 2 + 3` caused duplication—the same whitespace was being stored on multiple terms. For example, the space between `2` and the second `+` was being claimed by both `2`'s after-secondary AND the inner BinOp's after-secondary.

**Root Cause Analysis:**

For skeleton nodes, `Skel.range` returns the full span of the expression including operands. This means:
- For `Bin(left, op, right)`: range = (left.start, right.end)
- The parent's `before` position = left child's `before` position
- The parent's `after` position = right child's `after` position

Both parent and child were collecting from the same position, causing duplication.

**Solution: Selective Collection by Node Type**

Each skeleton node type has different boundary ownership:

| Node Type | Store before? | Store after? | Reason |
|-----------|---------------|--------------|--------|
| **Op** (leaf) | Yes | Yes | No children to conflict |
| **Bin** | No | No | Both boundaries overlap with children |
| **Pre** | Yes | No | Before is operator, after overlaps with operand |
| **Post** | No | Yes | Before overlaps with operand, after is operator |

This ensures each piece of secondary is collected exactly once:
- Spaces adjacent to operands → stored on the operand (leaves)
- Spaces adjacent to operators (not operands) → stored on the compound expression (Pre/Post)

**Verification Examples:**

`1 + 2 + 3` with standard spacing:
- "1": after=[" "] (index 1)
- "2": before=[" "] (index 3), after=[" "] (index 5)
- "3": before=[" "] (index 7)
- Inner Bin: nothing (per rule)
- Outer Bin: nothing (per rule)
- ✓ All 4 spaces assigned exactly once

`a + ! b` (prefix in binary context):
- "a": after=[" "] (index 1)
- Pre `!b`: before=[" "] (index 3) — the space before the operator
- "b": before=[" "] (index 5) — the space between `!` and `b`
- Outer Bin: nothing
- ✓ All 3 spaces assigned exactly once

**Implementation:** Modified `SecondaryCollection.collect_from_skel` to conditionally collect before/after based on the skeleton node type.

### Implementation Complete

All phases are now complete:

1. ✅ Phase 1: Extended `IdTag.t` with `secondary` field
2. ✅ Phase 2: Created `SecondaryCollection` module with selective collection
3. ✅ Phase 3: Integrated collection into `MakeTerm`
4. ✅ Phase 4: Implemented `PreserveExact` mode in `ExpToSegment`
5. ✅ Phase 5: Comprehensive round-trip tests (54 tests covering all major forms)

**Test Coverage:**
- Simple atoms (integers, floats, booleans, strings, variables)
- Binary operations (standard, compact, chained, 4-term chains)
- Prefix operators (negation, not, with/without spaces)
- Mixed prefix/binary combinations
- Let expressions (standard, compact, nested)
- Tuples and lists (standard, compact, extra spaces, empty)
- Functions (standard, compact, with body spaces)
- Case expressions (single and multiple clauses)
- Type annotations and type aliases
- If expressions
- Nested/complex expressions
- Application (single and multiple args)

All 1303 tests pass, including 73 dedicated round-trip tests.

### Sum Types: Special Case for Variant Storage

Sum types presented a unique challenge because their constructors are stored in `ConstructorMap.variant`, not as regular terms:

```reason
type variant('a) =
  | Variant(Constructor.t, list(Id.t), option('a))  // Before
  | BadEntry('a);
```

The `list(Id.t)` parameter had no secondary storage, so whitespace in sum type definitions was lost during round-tripping. For example, `type T = +A(Int) + B in T` would lose the space between `+A(Int)` and `+ B`.

**Solution:** Add a `variant_ann` type to `ConstructorMap` that stores both ids and secondary:

```reason
type secondary_runs = (list(Secondary.t), list(Secondary.t));
type variant_ann = {
  ids: list(Id.t),
  secondary: secondary_runs,
};

type variant('a) =
  | Variant(Constructor.t, variant_ann, option('a))  // After
  | BadEntry('a);
```

**Key Changes:**

1. **ConstructorMap.re**: Added `variant_ann` type with `secondary_runs` (duplicated from `IdTagged` to avoid dependency cycles)

2. **MakeTerm.re (`parse_sum_term`)**: Capture secondary from term annotations when parsing sum type constructors:
   - For bare constructors `A`: use the constructor's secondary directly
   - For constructor applications `A(T)`: combine inner before (constructor) with outer after (application) for correct round-tripping

3. **ExpToSegment.re (`go_constructor`)**: Emit secondary from `variant_ann` when in `PreserveExact` mode:
   ```reason
   let wrap_variant_secondary = (ann: variant_ann, seg: Segment.t) =>
     switch (settings.secondary) {
     | PreserveExact =>
       let (before, after) = ann.secondary;
       secondary_to_segment(before) @ seg @ secondary_to_segment(after)
     | AutoFormat => seg
     };
   ```

4. **All variant usages updated**: Many files needed updating from `Variant(c, ids, t)` to `Variant(c, ann, t)` with helper functions `empty_variant_ann` and `mk_variant_ann(~ids, ())` for convenience.

**Test Coverage:**
- `type T = +A in T` (single constructor)
- `type T = +A + B in T` (two constructors)
- `type T = +A(Int) + B in T` (with args)
- `type T = + A + B in T` (extra spacing)
- `type T = +A+B in T` (compact)

### Known Limitation: Defensive Parenthesization

Some forms don't fully round-trip due to **defensive parenthesization** in ExpToSegment. This is a structural issue, not a secondary storage issue—the secondary is correctly stored and emitted, but ExpToSegment adds parentheses to ensure correct re-parsing.

**Example: Rec/Poly types after type annotation**

```
Input:  1 : rec t -> t
Output: 1 :( rec t -> t)
```

The space before `rec` is preserved (note the space after `:`), but ExpToSegment wraps the type in parentheses. This happens because `rec` and `poly` types have low precedence, and without parens, the output might be ambiguous or parse differently.

Similarly for poly types:
```
Input:  1 : poly a -> a
Output: 1 :( poly a -> a)
```

**Related cases:**
- **Tuple parenthesization** (mentioned in "Notes" section): Tuples may get wrapped in parens
- **Function parameters**: Complex types in function parameter positions may get wrapped
- **Nested arrows**: `(Int -> Bool) -> x` shows how arrows on the left of an arrow need parens

**Why this happens:**

ExpToSegment uses a `precedence` function to determine when to add parentheses. When converting a type after `:`, if the type's precedence is below a threshold, it wraps in parens to avoid parsing ambiguity. For example, without parens:
- `1 : rec t -> t` could potentially parse as `(1 : rec t) -> t` depending on grammar

**Potential fixes (future work):**
1. Adjust precedence values so rec/poly types don't trigger parenthesization in that context
2. Add context-awareness to know when parens are truly needed vs. defensive
3. Store explicit "was parenthesized" in the term structure (but this adds complexity)

**Test coverage:** See `roundtrip_known_limitations` in `test/Test_ExpToSegment.re` for documented examples.

### Out of Scope for Round-Trip Testing

The following forms are explicitly **not tested** for round-tripping:

1. **Projectors and related display features**
   - Projectors/Refractors (`^^projector_name` syntax)
   - LivelitName (`^livelit`) - part of projector/livelit system
   - These might involve new term nodes instead of new annotations

2. **Legacy/experimental syntax**
   - BlockExp (`{...}`) - preliminary syntax for probe user study
   - LogicalOrLegacy (`\/`) - legacy logical OR syntax, low priority

### Known Limitations

#### Defensive Parenthesization (forms with arrow trailing delimiters)

ExpToSegment adds parentheses for forms like `rec`/`poly`/`typfun`/`forall` after `:` because they share low precedence with their `->` trailing delimiter. Related: `fun`/`fix` also use `->` but typically don't appear after `:`.

See Issue #1913 for related edge cases with forall regrouting.

**Examples:**
- `1 : rec t -> t` becomes `1 :( rec t -> t)`
- `1 : poly a -> a` becomes `1 :( poly a -> a)`
- `typfun a -> fun x : a -> x` - the inner `: a` gets wrapped

#### Other limitations

1. **QuotedLabel backticks**
   - Simple quoted labels like `` `a` `` lose their backticks and become `a`
   - Labels with spaces (`` `hello world` ``) or empty labels (``` `` ```) work because backticks are required to parse them
   - This is an ExpToSegment issue, not secondary storage

2. **Float literal normalization**
   - Float literals are normalized to full precision: `2.0` becomes `2.000000`
   - This is a parser normalization, not a secondary storage issue

### Remaining Work (Needs Investigation)

1. **Grout (convex and concave)**
   - Grout pieces appear during editing as placeholders
   - Secondary preservation for grout needs investigation
   - May require changes to how grout is handled in collection/emission

2. **Explicit Holes (`?`)**
   - `MakeTerm.re:220` has special handling via `is_hole_label`
   - May need adjustment to secondary collection logic
   - Currently untested for exact round-tripping

3. **LLMHole (`??...??`)**
   - LLM-assist holes
   - Similar concerns to explicit holes
