# Selector Language: Compositionality Analysis

This document examines the selector language as a **compositional system**, working
through a realistic program and identifying the underlying principles, gaps, and
potential generalizations.

## 1. The Uniform Resolution Model

The selector language is built on two primitives:

1. **Spine walking**: Each syntactic form (let, if, case, fun, list, tuple, module)
   exposes an ordered sequence of landmarks (keywords/delimiters) and slots.
   `_`, `_...`, and `*` navigate within a single spine.

2. **Descent**: `\...` (or `⋱`) switches from walking the current spine to
   searching all descendants of the current node.

Everything else — binder chains, keywords, names — is sugar or shorthand for
particular navigation patterns.

### The fundamental operation

At any point during resolution, we have:
- A **current node** (subtree of the program)
- A **remaining selector** (list of semantic steps)

Resolution consumes steps, using the current node's structure to decide how
to advance. This is essentially a **zipper-guided walk** over the AST.

### What "compositional" means here

A selector is compositional if:
- Each piece can be understood independently
- Combining pieces produces predictable results
- The same sub-pattern works in any context where the syntactic form appears

## 2. Test Program

```hazel
module App = {
  type msg = +Increment +Decrement +Reset;

  let init : Int = 0;

  let update : msg -> Int -> Int = fun msg -> fun model ->
    case msg
    | Increment => model + 1
    | Decrement => model - 1
    | Reset => 0
    end;

  let view : Int -> (String, List(String)) = fun model ->
    let label = int_to_string(model) in
    ("counter", [
      ("button", ["+"], fun _ -> Increment),
      ("span", [label], fun _ -> model),
      ("button", ["-"], fun _ -> Decrement)
    ]);

  test update(Increment, 0) == 1 end;
  test update(Decrement, 1) == 0 end;
  test update(Reset, 42) == 0 end;
} in
let result = App.update(App.init, Increment) in
result
```

## 3. Selector Catalog (what should work)

### 3.1 Direct binder access (binder chains)

| Selector | Expected | Principle |
|----------|----------|-----------|
| `App/init = *` | `0` | Chain into module, focus on def |
| `App/update = *` | `fun msg -> ...` | Chain into module, focus on def |
| `App/view = *` | `fun model -> ...` | Chain into module, focus on def |
| `module App = *` | `{ ... }` | Module def without chain |
| `App _... in *` | `let result = ...` | Module body |

### 3.2 Descending into definitions

| Selector | Expected | Principle |
|----------|----------|-----------|
| `App/update \... case *` | `msg` | Chain + descend + case scrutinee |
| `App/update \... \| Increment => *` | `model + 1` | Chain + descend + arm |
| `App/update \... \| _ => *` | all 3 arm bodies | Chain + descend + wildcard arm |
| `App/update \... \| _... Reset => *` | `0` | Chain + descend + ellipsis + named arm |
| `App/view \... let label = *` | `int_to_string(model)` | Chain + descend + nested let |

### 3.3 Compositional sub-patterns

These demonstrate that the same pattern works regardless of context:

| Selector | Context | Expected |
|----------|---------|----------|
| `\... if _... else *` | anywhere | Every else branch in program |
| `\... case *` | anywhere | Every scrutinee in program |
| `\... \| _ => *` | anywhere | Every case arm body in program |
| `\... fun _ -> *` | anywhere | Every function body in program |
| `\... test *` | anywhere | Every test body in program |
| `\... let _ = *` | anywhere | Every let definition in program |

### 3.4 Nested descent (chained `\...`)

| Selector | Expected | Principle |
|----------|----------|-----------|
| `App/view \... \... fun _ -> *` | nested lambdas inside view | Double descent |
| `\... \... \| _ => *` | arm bodies at any depth | Nested descent is idempotent-ish |

### 3.5 Tests (within module)

| Selector | Expected | Principle |
|----------|----------|-----------|
| `\... test *` | all 3 test bodies | Descend finds all tests |

## 4. Architectural Assessment

### 4.1 What's working well

**Descent + keyword + spine is genuinely compositional.** The pattern
`context \... keyword spine-pattern` composes cleanly. You can:
1. Navigate to a context (via chain or descent)
2. Descend into it
3. Match a keyword to pick the construct type
4. Walk the spine with `_`, `_...`, and `*`

This three-layer structure (context → descent → spine) covers the vast
majority of use cases.

**Binder chains are effective shorthand.** `A/B/x` is much more concise
than `module A = \... module B = \... let x` and resolves unambiguously
through the binder hierarchy.

### 4.2 Architectural concerns

**1. The `walk` function has two modes mixed together.**

When `walk` encounters a name like `x`, it has multiple possible interpretations:
- Is `x` a binder chain segment? (look up in `find_binder_in_exp`)
- Is `x` followed by `=`? (enter definition)
- Is `x` followed by `_... in`? (enter body)
- Is `x` just a name for `find_let_node`? (find the Let/ModuleExp wrapping `x`)

These are handled by pattern-matching on the full remaining selector, which means
the resolution of `x` depends on what comes *after* it. This is fine for a fixed
set of patterns but doesn't generalize well.

**Suggested principle**: A name after a keyword (`let x`, `module M`, `fun x`)
is resolved by the keyword's spine walker. A bare name at the start of a selector
or after `\...` is resolved by `find_let_node` (enters the binding). This
distinction should be made more explicit.

**2. Spine walkers are not uniform in their operator support.**

The audit revealed that `walk_after_module_kw`, `walk_after_type_kw`, and
`walk_test_spine` have minimal `_`/`_...` support compared to `walk_let_spine`,
`walk_if_spine`, and `walk_seq_spine`.

**Suggested principle**: Every spine walker should handle `MatchFocus` (return
current slot), `MatchSlot` (skip slot), and `MatchEllipsis` (skip to landmark)
at minimum. The spine walkers for `module`, `type`, and `test` should be brought
up to parity.

**3. The resolution of `|` depends on being inside a `Match`.**

Currently, `| Foo => *` at the top level calls `walk_pipe(current, ...)` which
extracts rules from the current node if it's a `Match`. But what if you want to
write `case _... | Foo => *`? That works because `walk_case_spine` delegates to
`walk_pipe_in_rules`. But `\... | Foo => *` also works because `descend_all` finds
the `Match` node and `walk` processes `| Foo => *` against it.

This is actually **correct compositional behavior**: `|` starts a new "spine level"
(the arms sequence) and `\...` finds the containing `case`. The key insight is that
`|` is implicitly scoped to its enclosing `case`.

**4. `_` and `_...` at the top-level `walk` are no-ops.**

When `_` or `_...` appear at the top level (not inside a spine walker), `walk`
just forwards them: `walk(rest, current)`. This means `_ let x = *` is the same
as `let x = *`. This is arguably correct (there's nothing to skip at the expression
level), but it means `_` doesn't "skip one let binding" in a chain. To skip
bindings, you need the `let` keyword: `let _ = _ in let x = *` or similar.

**This is actually the right design**: `_` operates within a spine, and the
top-level expression isn't really a spine (it's a tree). The explicit keywords
create spine contexts.

### 4.3 The compositionality principle

The selector language has a clear compositional grammar:

```
selector := context-nav* spine-pattern
context-nav := chain | descent | keyword
spine-pattern := (slot | ellipsis | focus | keyword | name | delimiter)*
```

Where:
- **context-nav** gets you to the right node in the tree
- **spine-pattern** navigates within that node's structure

The composition rule is: **context navigation produces a node; spine patterns
consume that node's structure.**

## 5. Specific Gaps to Address

### 5.1 `module` spine: slot/ellipsis support

`module _ = *` should work (wildcard module name). Currently `walk_after_module_kw`
requires an explicit name.

### 5.2 `type` spine: full spine support

`type _ = *` should work similarly. `walk_after_type_kw` is minimal.

### 5.3 Double descent deduplication

`\... \... let x = *` performs descent twice. Each descent searches all
descendants, so the second descent from each first-descent match could find
the same `let x` at different depths. The dedup in `descend_all` handles
this within a single descent, but dedup across chained descents may produce
duplicates. This should be tested and possibly addressed.

### 5.4 `let` within modules

When you write `\... let x = *` inside a module context, the selector
descends into the module's items. But `find_all_lets` deliberately skips
`ModuleExp` to avoid double-counting with `descend_all`. This means
`let x = *` (without `\...`) on a module body won't find `ModLet` items
directly. The current workaround is binder chains (`M/x = *`).

This is acceptable because modules and expression lets are different syntactic
categories — you shouldn't expect `let` keyword to find module items. Module
items are addressed via chains or explicit `module M = \... let x = *`.

### 5.5 The `* let x` pattern (whole-binding focus)

`* let x` focuses on the entire `let x = ... in ...` expression. This is
implemented as `[MatchFocus, MatchKeyword("let"), ...]` where `MatchFocus`
at the start of `walk` catches the first step and wraps the subsequent resolution.

Currently `* let x` works for let bindings. Does `* module M` work? Does
`* type T` work? These should be tested for consistency.

## 6. Implementation Recommendations

### 6.1 Short-term (improve current architecture)

1. Bring `walk_after_module_kw` and `walk_after_type_kw` to parity with
   `walk_let_spine` for `_`/`_...`/`*` handling
2. Add `MatchSlot` support to `walk_after_module_kw`:
   `module _ = *` should match any module definition
3. Test chained descent deduplication
4. Test `* module M` and `* type T` patterns

### 6.2 Medium-term (generalize)

Consider refactoring spine walkers to share more structure. Each spine is
essentially a **sequence of (landmark, slot) pairs**. A generic spine walker
could be parameterized by the landmarks and slots:

```
walk_generic_spine(
  landmarks: [(string, Exp.t)],  /* (delimiter, slot_value) pairs */
  steps: sem_selector
)
```

This would make `_`, `_...`, and `*` work uniformly across all forms by
construction, rather than needing to add support case-by-case.

However, this is a significant refactor and the current approach works.
The pragmatic path is to keep the per-form walkers but ensure they all
handle the three core operators.

### 6.3 Long-term (the vision)

The selector language should feel like "abbreviated Hazel syntax with wildcards."
The user writes something that looks like the code they're targeting, with `_` for
"don't care" and `*` for "I want this." The system resolves it against the actual
program.

This means new syntactic forms added to Hazel (records, labeled tuples, etc.)
should automatically get selector support by following the same spine-walker
pattern.

## 7. Test Matrix

The following tests would validate compositionality claims. Tests marked with
[!] are currently known to not work or to be untested.

```
# Context navigation
App/init = *                    → 0
App/update = *                  → fun msg -> ...
App/update \... case *          → msg
App/update \... | Increment => * → model + 1

# Wildcard in arms (recently fixed)
App/update \... | _ => *        → [model + 1, model - 1, 0]  (3 matches)

# Spine uniformity
module App = *                  → { ... }
[!] module _ = *                → { ... }  (needs slot support)
App _... in *                   → let result = ...

# Nested descent
App/view \... let label = *     → int_to_string(model)
\... test *                     → [all test bodies]  (3 matches)
\... fun _ -> *                 → [all function bodies]

# Double descent (should work, needs dedup testing)
\... \... | _ => *              → [all arm bodies at any depth]

# Whole-binding focus
* let result                    → let result = ... in result
[!] * module App                → module App = ... in ...  (needs testing)
```
