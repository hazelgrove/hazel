# Associativity-selection integration notes

## Goal

Bring the finalized structural-selection behavior from
`associativity-selection` into the current Rocq/stepper branch without
replacing the newer written-step, proof-search, math-profile, or export work.

The branches share merge base `68dd90ade70b7d3e7b87174df7355cefc95caec3`,
but have diverged substantially. In particular, the version of
`MissingStep.re` on `associativity-selection` predates the current proof-search
UI. The branch should therefore be treated as a source for a focused port, not
merged or cherry-picked wholesale.

## Selection design to preserve

The important upstream change is the finalized `SelectionEffective` model:

- A normal selection is represented as `Existing(id)`.
- An associative slice with no corresponding AST node is represented as
  `Virtual({segment, exp, container_id})`.
- One effective-selection value determines the visible highlight, selected
  expression, containing/root ID, and replacement target.
- Standard range-based selection remains authoritative whenever associative
  selection cannot produce a valid virtual slice.
- Whitespace is ignored when deciding whether a virtual slice contains the
  user's selected syntax, while comments and syntax-bearing pieces remain
  significant.
- A virtual candidate is accepted only when its containing expression and
  concrete segment agree. Replacement also verifies that the matched concrete
  range is exactly the selected range.

This unified representation fixes the current split in which highlighting uses
associative expansion while `MissingStep` independently recovers an expression
with `TermData.get_root_id_using_ranges`. That split is especially visible for
derivative notation: `deriv body by x` is internally an application, so a drag
near its argument or an adjacent `+` can highlight one range while proof search
receives another.

## Relevant upstream history

Use the final files on `associativity-selection` as the reference, while
consulting these commits for intent and regression coverage:

- `21e73825e1` — validates exact selection roots instead of returning an
  enclosing associative root for a smaller visible range.
- `3dbf981e62` — finalizes the effective-selection integration and routes the
  stepper through it.
- `456f910aa6` — regrouts siblings after associative replacement; relevant if
  replacement behavior is ported with the selection model.

## Porting sequence

1. Port the finalized `SelectionEffective.t`, `target`, `standard_selection`,
   `effective_selection`, `root_id`, `selected_exp`, and replacement API.
2. Retain the current branch's narrow associative operator classification in
   `AssocSelection.re`; remove only helpers made obsolete by the unified API.
3. Adapt the current `MissingStep.re` incrementally:
   - calculate one effective selection from the current editor;
   - derive both `selected_id` and `selected_exp` from it;
   - keep the current visible/elaborated-term fallbacks only where required by
     hidden stepper syntax;
   - route rewrite, written-step, assumption, Rocq search, and replacement
     actions through that same selection value;
   - preserve all current async proof-search IDs, trace summaries, active math
     profiles, and JSCoq status handling.
4. Update highlighting to consume `effective_selection.segment`, avoiding a
   second independent associative-selection calculation.
5. Port the selection and replacement tests before deleting compatibility
   helpers.

## Regression coverage required

Port or adapt the upstream tests for:

- associative selections nested inside a function argument;
- selecting an application function without capturing its argument;
- selection across an application argument comma;
- selection ending on an application's closing delimiter;
- checker expression and visible highlight resolving to the same application;
- standard application selections remaining non-virtual;
- whitespace at either boundary of an associative slice;
- nested associative replacement using the correct containing expression.

Add equivalent cases for the new derivative syntax, not only legacy
`diff(body, variable)`:

- selecting the body inside `deriv body by x` stays inside the derivative;
- dragging from `deriv` through `by x` selects exactly one derivative;
- selecting one derivative in `deriv u by x + deriv v by x` does not capture
  the surrounding sum;
- selecting the middle `+` may still create the intended virtual additive
  slice;
- Search's displayed `From`, the highlighted range, and the expression replaced
  by a successful step are identical.

Finally run `make`, focused `SelectionEffective` and `RewriteChecker` tests, and
a manual stepper smoke test on a derivative chain. The manual check should
exercise selection, Rocq validation, replacement, and proof export rather than
stopping after the selection highlight looks correct.

## Avoid

- Do not merge `associativity-selection` directly: its old `MissingStep.re`
  would remove the current proof-search and math-profile implementation.
- Do not add derivative-specific snapping rules. Derivatives should work
  because application and effective-selection boundaries are correct.
- Do not let highlighting, checking, and replacement recompute selection by
  different mechanisms.
