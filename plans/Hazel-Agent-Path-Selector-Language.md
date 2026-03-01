# Hazel Agent Path & Selector Language (v0)

This document specifies a **concise, surface-oriented selector language** for addressing Hazel syntax in a coding agent. It is intended to be **self-contained** and sufficiently precise to implement:

- parsing selectors,
- resolving them against a Hazel syntax tree,
- and using resolutions to drive read/edit actions (update/delete/insert/move).

We aim for:

- **terse and readable** selectors that look like abbreviated syntax,
- **compositional** combination via descendant search,
- **good defaults** (no need to write closing delimiters most of the time),
- and **clear behavior** under ambiguity (query vs edit actions).

We **do not** introduce any “virtual semicolon” syntax for flattening nested lets in expression bodies in this v0.

---

## 0. Core concepts

### Selector vs Match vs Focus

A selector matches one or more regions of syntax and can designate a specific **focused subtree** to return.

- `_` matches one syntactic slot/item.
- `_...` matches zero or more slots/items along the **current spine** (rendered as `…` in examples).
- `⋱` means **descendant search** within the previously matched region (“diagonal ellipsis”).
- `*` marks the **focused** subtree: “select the next syntactic unit/slot following `*`”.

### S1 name policy

A **bare name** like `x` matches occurrences of the identifier `x` anywhere (binders or references), grep-style.
Binder/member navigation is done using either:

- explicit surface patterns (`let x`, `module M`, `type T`), or
- the binder-chain sugar `A/B/C` (see §4).

### Two usage modes

- **Query/view** operations may return 0..N matches.
- **Edit** operations (update/delete/insert/move) should usually require **exactly 1** match; otherwise they fail with a disambiguation report.

---

## 1. Spines and what `_` / `…` mean

Many constructs form an **ordered spine** of landmarks (keywords/delimiters) and slots/items.

Examples:

- `if` spine: `if <cond> then <then> else <else>`
- `let` spine: `let <pat> = <def> in <body>`
- `case` spine: `case <scrut> | <pat> => <body> | <pat> => <body> end`
- module item spine: `{ item0; item1; item2; ... }`
- list spine: `[ e0, e1, e2, ... ]`

**Rule (uniform):**

- `_` matches one slot/item in the current spine.
- `…` (`_...`) matches zero or more slots/items in the current spine.
- `…` never descends into children; it ranges only over siblings/items at this spine level.

This operator is intentionally “polymorphic”: it works the same way across different spine domains.

---

## 2. Focus `*` (what it selects)

**Single rule:** `*` selects the **next syntactic unit/slot** that follows it.

Common patterns:

- `if *` selects the condition slot.
- `if _ then *` selects the then slot.
- `if … else *` selects the else slot without counting.
- `let x = *` selects x’s RHS definition expression.
- `let x … in *` selects x’s body expression after `in`.
- `| Increment => *` selects the arm body.
- `case *` selects the scrutinee slot.
- `module M = *` selects the module RHS expression (often `{ ... }`).
- `* let x` selects the whole `let ... in ...` expression binding x.
- `* module M` selects the whole module-definition expression.

**Note:** You generally do not need closing delimiters (`end`, `}`) in selectors unless you are constraining structure for disambiguation.

---

## 3. Descendant search `⋱`

`P ⋱ Q` means:

1. match `P` (possibly multiple times),
2. for each match of `P`, match `Q` somewhere **inside that subtree**.

This is the main “descend arbitrary levels” operator and is the primary way to avoid writing fully explicit slot-by-slot paths.

Input alternative note (optional):

- `⋱` may be entered as the ASCII sequence `\...` and rendered as `⋱` in displays.

---

## 4. Binder-chain sugar `A/B/C` (explicitly: sugar)

### Intent

`A/B/C` is a specialized shorthand for navigating nested **binder/member definitions**. It is **not** the same as writing `A B C` or relying on bare-name S1 behavior.

### Semantics

`A/B/C` means:

- resolve a binder/member named `A`,
- enter **A’s definition term**,
- within it resolve binder/member `B`,
- enter **B’s definition term**,
- within it resolve binder/member `C`, etc.

“Definition term” depends on binder form:

- `let x = <def> in <body>`: definition term is `<def>`
- `module M = <modexpr> in <body>`: definition term is `<modexpr>` (often `{ items }`)
- `type T = <ty> in <body>` (if used): definition term is `<ty>`

### Sugar expansion (conceptual)

In the **semantic selector** (internal), a chain elaborates to repeated:

- `FindBinderByName(name)`
- `EnterDefinitionTerm` (except after the last segment)

This is worth implementing as sugar so the parser stays simple and the resolver does the real work.

### Examples

- `A/B/x = *` selects RHS of binder `x` nested inside binder `B` nested inside binder `A`.
- `A/B/f ⋱ if … else *` selects the else branch of an `if` somewhere inside `f`’s definition.

---

## 5. Concrete syntax (surface)

A selector is a whitespace-separated sequence of tokens (keywords, delimiters, names, `_`, `…`, `*`), with optional `⋱` to introduce descendant search. `A/B/C` parses as a chain token.

We keep the surface grammar intentionally permissive; correctness is enforced by resolution against the program’s syntax tree.

---

## 6. Resolution results (what to return from the resolver)

For each match, return at least:

- the focused subtree (the thing selected by `*`),
- a stable internal identifier/path to that subtree (recommended),
- and optionally a “breadcrumb” string for disambiguation in error messages.

This makes it easy to:

- show results to the model with their addresses,
- and perform edits robustly.

---

## 7. Ambiguity policy

- **Query/view** actions return all matches.
- **Edit** actions require exactly one match:
  - if 0: “no match”
  - if >1: “ambiguous”; return a short list of candidates (paths/snippets)

We can add an explicit “allow many” flag later, but it is not required for v0.

---

# 8. Worked Examples

## Example 1: If inside a function binding

```hazel
let f = fun x ->
  if x > 0 then x else 0
in
f 5
```

1. Condition

- `let f = ⋱ if *`
  - `* = (x > 0)`

2. Then branch

- `let f = ⋱ if _ then *`
  - `* = x`

3. Else branch (minimum)

- `let f = ⋱ if … else *`
  - `* = 0`

4. Entire let-binding expression

- `* let f`
  - `* = (let f = ... in ...)`

---

## Example 2: Case arms

```hazel
let update = fun msg -> fun model ->
  case msg
  | Increment => { count = model.count + 1 }
  | Decrement => { count = model.count - 1 }
  end
in
update
```

1. Scrutinee

- `let update = ⋱ case *`
  - `* = msg`

2. Increment arm body

- `let update = ⋱ | Increment => *`
  - `* = { count = model.count + 1 }`

- More explicit (skip over any earlier arms/items):
  - `let update = ⋱ case … | Increment => *`
  - `* = { count = model.count + 1 }`

3. Decrement arm body

- `let update = ⋱ | Decrement => *`
  - `* = { count = model.count - 1 }`

---

## Example 3: Module items + nested let inside a member

```hazel
module M = {
  let x = 1;
  let y = (let z = 2 in z + x);
  test y == 3 end;
  y
} in
M
```

1. Module RHS

- `module M = *`
  - `* = { ... }`

2. Member `x` RHS

- `module M = ⋱ let x = *`
  - `* = 1`

3. Member `y` RHS

- `module M = ⋱ let y = *`
  - `* = (let z = 2 in z + x)`

4. Inner `z` RHS

- `module M = ⋱ let y = ⋱ let z = *`
  - `* = 2`

5. Inner `z` body

- `module M = ⋱ let y = ⋱ let z … in *`
  - `* = z + x`

---

## Example 4: Nested modules, shadowing, binder-chain sugar

```hazel
module A = {
  let x = 1;
  module B = {
    let x = 2;
    let f = fun u -> if u > x then u else x;
  };
  let g = fun v -> if v > x then v else x;
} in
A
```

1. B.f else branch (chain + descend)

- `A/B/f ⋱ if … else *`
  - `* = x` (the identifier occurrence in the else branch of B.f)

2. B.x RHS

- `A/B/x = *`
  - `* = 2`

3. A.x RHS

- `A/x = *`
  - `* = 1`

Note: bare `x` (S1) matches all occurrences of `x` anywhere; use `let x` or chains to target binders.

---

## Example 5: List spine (variable arity)

```hazel
let xs = [a, b, c, d] in xs
```

1. Last element

- `[ … , * ]`
  - `* = d`

2. First element

- `[ * … ]` (or `[ * , … ]` depending on your list tokenization)
  - `* = a`

---

# 9. Edit actions and how selectors interact

Selectors resolve to **nodes/subtrees** (focused by `*`). Some edit actions need an anchor that denotes a **definition/item node** rather than a raw identifier occurrence.

### Update / Delete

- `Update(selector, code)` replaces the focused subtree.
- `Delete(selector)` deletes the focused subtree (subject to host rules).

Edits should require the selector to resolve uniquely.

### Insert Before / After (uniform action-level semantics)

Insertion is defined relative to an **anchor selector** that must resolve uniquely to an _insertion-eligible node_ (e.g., a definition/item).

Important clarification (per discussion):

- For insertion anchors, the selector should usually target the **definition/item construct**, e.g. `* let x`, not the bare name `x` (which under S1 is grep-style).

Examples (schematic):

Given:

```hazel
let x = ax in
BODY
```

- `InsertAfter(anchor = (* let x), code = let y = ay)` produces:

```hazel
let x = ax in
let y = ay in
BODY
```

- `InsertBefore(anchor = (* let x), code = let y = ay)` produces:

```hazel
let y = ay in
let x = ax in
BODY
```

For module item lists, “before/after” is literal list insertion among `;`-separated items.

This interpretation is uniform at the action level; implementation may use rewriting for nested-expression lets.

### Move

`Move(anchor, Before/After, otherAnchor)` can be implemented as `Delete(anchor)` + `Insert(Before/After otherAnchor, deletedItem)`.

---

# 10. Implementation notes (condensed)

### Two-layer internal representation (recommended)

1. **Surface selector AST**: close to user tokens (`⋱`, atoms, chains).
2. **Elaborated semantic selector**: expands chains and defaults, making evaluation easier.

Why: surface AST is great for parsing + error messages; semantic AST is nicer for structured recursion over Hazel syntax.

### Resolution strategy (high-level)

- Parse selector.
- Elaborate:
  - expand chains into binder-lookup + enter-def-term steps.
  - interpret `…` with the appropriate spine domain at runtime.

- Evaluate against the program syntax tree:
  - return matches + focused subtree + stable internal id/path.

---

# 11. Notation notes for presentation vs typing

- In examples, we render `_...` as `…` for readability.
- Implementation should accept `_...` as the canonical typed form.
- `⋱` may have an ASCII input alias; choose one and normalize internally.

---

## Design note: selectors as partial patterns

Selectors are best understood as **partial pattern matching against syntax**.
Each selector reads as the opening/forward spine of a Hazel form — the
keywords and delimiters that identify the form and its slots — with `_`/`_...`
for wildcards and `*` for capture. You never need to spell out closing
delimiters because the opening spine already identifies the form.

This connection to pattern matching is the source of the language's
compositionality: just as you can nest patterns (`(x, (y, z))`), you can
nest selectors via `\...` to descend into matched subterms. The three-layer
structure — context navigation, descent, spine pattern — mirrors how
patterns compose: outer shape, recursive entry, inner shape.

See `plans/selector-compositionality-analysis.md` for a detailed analysis
of compositionality principles and architectural implications.

## Status / scope of v0

This v0 spec:

- supports matching across `let`, `if`, `case`, module item lists, list literals,
- supports diagonal descent `⋱`,
- supports binder-chain sugar `A/B/C`,
- defines consistent focus selection with `*`,
- defines how insertion uses anchors like `* let x`,
- and supports selector-driven edits via `SelectorUpdate(sel, code)` and
  `SelectorDelete(sel)`.

Future extensions (not in v0):

- explicit “gap/location” selection for insertion sites (zipper boundaries),
- explicit tagging for binder vs reference queries (e.g., `@refs(x)`),
- explicit indexing (`@0`, `@1`, `@last`) for disambiguation,
- derived “scope spine” views for mixed let/test sequences without explicit separators,
- spine-schema unification (generic resolver parameterized by form structure),
- nested selector patterns (e.g., `let (_, x) = *` for tuple-pattern lets),
- pattern variables (`let $name = $def` with named capture groups).

---

If you want one final pass before handing to the agent, tell me whether you want **requiring `*` always** (simpler) or allowing a few default focuses (like `case *` selecting scrutinee even without `*`). I kept examples using `*` explicitly, but the implementation can be strict or permissive.
