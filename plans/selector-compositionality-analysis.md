# Selector Language: Compositionality Analysis

This document examines the selector language as a **compositional system**, working
through a realistic program and identifying the underlying principles, gaps, and
potential generalizations.

## 0. Selectors as Partial Patterns

The deepest way to understand the selector language is as **partial pattern matching**
against Hazel's syntax tree. Like destructuring patterns in a functional language, a
selector describes the *shape* of the syntax it targets. But unlike full patterns:

- **Partial**: You only need to spell out the forward/opening spine — the keywords,
  delimiters, and structural landmarks that distinguish the form. Trailing delimiters
  and closing syntax are optional.
- **Focused**: `*` marks which sub-expression to extract, like a "capture group" in a regex.
- **Wildcarded**: `_` and `_...` skip parts you don't care about.
- **Descendable**: `\...` lets you jump into the interior of any matched subtree.

The compositionality of selectors should mirror the compositionality of pattern matching:
just as you can nest patterns (`(x, (y, z))` matching nested tuples), you should be
able to nest selectors (`let x = \... if _ then * else _` matching into a nested if).
The structure of the selector follows the structure of the syntax, and you can go
as shallow or as deep as you need.

### CSS selectors analogy

Like CSS selectors, these selectors navigate a tree (the AST instead of the DOM):
- **Descendant** (`\...`) is like CSS's space combinator (any descendant)
- **Direct nesting** (keyword spine) is like CSS's `>` child combinator
- **Wildcards** (`_`, `_...`) are like CSS's `*` universal selector
- **Names** are like CSS class/ID selectors

But unlike CSS, our selectors are *linearized patterns*: they read left-to-right as
the opening delimiters of the syntax form they target, rather than as a tree of
nesting combinators.

### The key insight: opening delimiters form a linear prefix

Every Hazel form has a sequence of opening delimiters:
- `let <pat> = <def> in <body>` → prefix: `let`, `=`, `in`
- `if <cond> then <then> else <else>` → prefix: `if`, `then`, `else`
- `case <scrut> | <pat> => <body> ... end` → prefix: `case`, `|`, `=>`, `end`
- `fun <pat> -> <body>` → prefix: `fun`, `->`
- `module <name> = <def> in <body>` → prefix: `module`, `=`, `in`

A selector reads as a **prefix of this delimiter sequence**, with `_` and `_...`
filling in the slots between delimiters, and `*` marking which slot to extract.
You never need to write the closing delimiters because the opening delimiters
already uniquely identify the form and which slot you're targeting.

This is why `if _... else *` works: `if` identifies the form, `_...` skips past
the condition and then-branch, `else` lands on the else-delimiter, and `*` selects
the following slot.

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

This is exactly the compositionality of pattern matching: `(_, x)` matches the
second element of any pair, regardless of where that pair appears. Similarly,
`if _... else *` matches the else branch of any if, regardless of nesting context.

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

### 6.2 Medium-term: pattern-matching perspective

The selector language is best understood as **partial pattern matching** against
syntax. This suggests a more principled architecture than the current accretion
of spine walkers.

**What pattern matching gives us:**

In a pattern like `Let(Var("x"), _, body)`, the compositionality is obvious:
- `Let(...)` discriminates the form
- `Var("x")` constrains the pattern slot
- `_` wildcards the definition
- `body` captures the body

Our selectors do the same thing but with a different notation:
- `let x` discriminates the form and constrains the name
- `_` wildcards a slot
- `= *` says "enter the = slot, capture it"

**The principle: each keyword starts a pattern, each slot is a pattern hole.**

This suggests that rather than having per-form spine walkers with hand-coded
patterns, we could represent each form's structure as a **spine schema**:

```
let_spine = [KW("let"), Slot(Pat), Delim("="), Slot(Def), KW("in"), Slot(Body)]
if_spine  = [KW("if"), Slot(Cond), KW("then"), Slot(Then), KW("else"), Slot(Else)]
case_spine = [KW("case"), Slot(Scrut), Repeat(KW("|"), Slot(ArmPat), Delim("=>"), Slot(ArmBody)), KW("end")]
fun_spine = [KW("fun"), Slot(Pat), Delim("->"), Slot(Body)]
```

Then the resolver would be a **single generic function** that walks a spine schema
against the selector tokens:
- When it sees a `Slot` in the schema and `MatchSlot` in the selector: skip
- When it sees a `Slot` and `MatchFocus`: capture
- When it sees a `Slot` and `MatchName(n)`: match the slot's content against `n`
- When it sees a `KW(k)` in the schema and `MatchKeyword(k)` in the selector: consume both
- `MatchEllipsis`: skip forward to the next matching keyword in the schema

This is essentially **spine unification** between the form's structure and the selector.

**Why this matters**: New forms added to Hazel would get selector support by
defining their spine schema. `_`, `_...`, `*` would work uniformly by construction.
The current per-form walkers work but require adding MatchSlot/MatchEllipsis/MatchFocus
handling to each one individually, which is where compositionality breaks down.

**Practical path**: Keep the current per-form walkers for now (they work, they're
tested), but ensure each one handles all three core operators. The spine-schema
refactor is a clean architectural goal but not blocking.

### 6.3 Long-term (the vision)

The selector language should feel like "abbreviated Hazel syntax with wildcards."
The user writes something that looks like the code they're targeting, with `_` for
"don't care" and `*` for "I want this." The system resolves it against the actual
program.

This means new syntactic forms added to Hazel (records, labeled tuples, etc.)
should automatically get selector support — ideally by defining a spine schema
(or at minimum by following the same per-form walker pattern with full operator
coverage).

**Future: nested patterns.** Currently selectors are linear (a flat sequence of
tokens). But just as patterns can nest (`(x, (y, z))`), selectors could potentially
nest: `let (_, x) = *` to match a tuple-pattern let binding. This is not needed
for v0 but is the natural direction given the pattern-matching analogy.

**Future: pattern variables.** Pattern matching has binding — `Let(pat, def, body)`
binds `pat`, `def`, `body`. Selectors could potentially bind too: `let $name = $def`
where `$name` and `$def` are meta-variables returned in the match result. Again,
not v0, but the architecture should not preclude it.

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
