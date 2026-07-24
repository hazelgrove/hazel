# Selector Calculus v2

Selectors are **pattern combinators over syntax**. A selector navigates from
the root of a Hazel program to a specific subterm (the *focus*) by
pattern-matching against the syntactic structure along the way.

The core insight: every Hazel syntactic form has a *spine* — an alternating
sequence of tokens (keywords, delimiters, operators) and children (subterms).
A selector walks spines, matching tokens literally and children by name or
wildcard, with one distinguished child marked as the focus (`%`). For
**function calls**, the **`name(`** token matches the callee and enters the
argument spine (same slot rules as tuples).

Think of selectors as destructuring patterns where:

- **`name(`** — call pattern: match a function head and enter its argument spine
- Literal tokens (keywords, delimiters) are exact match constraints
- `_` is a wildcard matching any slot in a sequence spine
- `%` is a distinguished capture variable (the focus)
- `_...` skips arbitrary spine positions
- `\...` searches all descendants

Selectors complement **paths** in the high-level node map (`App/update/`,
`#0`, …). Paths are stable structural addresses; selectors read like code
fragments (`let x = %`, `authenticate(`, `| Inc => %`) and are the
primary input to agent tools (`select`, `selector_update`, `selector_delete`, …).

### Architectural principle: sort- and form-generic

The resolver is designed to work **homogeneously across sorts and forms** in
the Hazel grammar — Exp, Pat, Typ, Mod, and related sorts. Each form decomposes
into a spine of tokens and children; matching is generic rather than ad hoc per use site.

This means:

- **Sequence forms** (lists, tuples, module items, case arms, **call argument lists**)
  share one sequence-spine mechanism (`%`, `_`, `_...`).
- **Cross-sort navigation** (e.g., from Exp through Pat into Typ) follows
  `focus_target` (`FocusExp`, `FocusPat`, `FocusTyp`, `FocusMod`).
- **Function application** (`Ap`, `DeferredAp`) uses the **`name(`** call
  pattern to enter a call's argument spine — the same slot rules as tuples
  (`%`, `_`, `_...`), but only after the function head matches.

---

## 1. Meta-Level Syntax

### 1.1 Combinators and wildcards

These tokens have meta-level meaning (not Hazel object syntax reused literally):

| Token | Role |
|-------|------|
| `name(` | Call pattern — match callee `name`, enter argument spine (§1.2) |
| `%` | Focus — capture this position for edits |
| `_` | Wildcard — skip one slot in a sequence spine |
| `_...` | Ellipsis — skip zero or more spine positions |
| `\...` | Descent — search all descendants |
| `#N` | Child index — enter Nth structural child (0-based) |
| `name/` | Chain step — enter binder's definition |
| `name#N` | Indexed name — Nth binder with this name (0-based) |

### 1.2 Call patterns (`name(`)

**`name(`** — match a **function application** (or partial application) whose
function head is `name`, then walk **argument slots** with the sequence-spine
rules (`%`, `_`, `_...`).

| Form | Role |
|------|------|
| `name(` | Enter call to `name`; prefix form focuses first argument slot |

Examples: `authenticate(`, `App.update(`. Not the same as a **tuple
literal** spine `( … )` at a `Tuple` node — `name(` requires an `Ap` or
`DeferredAp` node whose head matches `name`. A space before `(` (`name (`)
is optional.

| Written | Elaborated | Focuses on |
|---------|------------|------------|
| `authenticate(` | `authenticate ( %` | first argument |
| `authenticate ( _ %` | (unchanged) | second argument |

Bare **`name`** without `(` matches **references** to `name`, not call arguments.

### 1.3 Hazel object syntax

Everything else — keywords (`let`, `if`, `fun`, `case`, `module`, `type`, …),
delimiters (`=`, `->`, `=>`, `|`, `:`, `(`, `)`, …), operators (`+`, `-`,
`&&`, `::`, …), and names / literals (`x`, `42`) — is Hazel syntax reused
literally as match targets when not part of a `name(` call pattern.

**Chain vs name:** `foo/` (trailing slash) is a chain step into a binder's
definition. `foo` without slash is a name or atom match, not a chain step.

Selectors are **space-separated** token sequences. The tokenizer splits on
whitespace.

---

## 2. Processing

1. **Tokenize** the selector string → `list(token)`.
2. **Elaborate** → `list(sem_step)`: insert `%` if omitted; collapse `\... \...`
   to `\...`; expand chains to `EnterBinderDef` steps.
3. **Resolve** against the program AST → `list(match_result)` (focused term, id, breadcrumb).

**Search** (`Selector.query`) may return multiple matches. **Edits**
(`Selector.query_unique`, `selector_update`, `selector_delete`, …) require
exactly one match and exactly one `%`.

---

## 3. Sugars and Conventions

### 3.1 Completion via trailing ellipsis

A spine pattern matches as a **prefix** — unmatched trailing positions are
implicitly wild, equivalent to appending `_...`:

| Abbreviated | Equivalent |
|-------------|------------|
| `let x = %` | `let x = % _...` |
| `if %` | `if % _...` |
| `[ _ %` | `[ _ % _...` |
| `{ _... let b = %` | `{ _... let b = % _...` |
| `authenticate(` | `authenticate ( %` |

### 3.2 Implicit focus

If no `%` appears in the selector:

- If the last token is a **name** or **literal**, insert `%` **before** it.
- Otherwise, append `%` at the end.

| Written | Elaborated | Focuses on |
|---------|------------|------------|
| `let x` | `let % x` | pattern `x` (FocusPat) |
| `fun x` | `fun % x` | parameter pattern (FocusPat) |
| `type T` | `type % T` | type name (FocusTPat) |
| `module M` | `module % M` | module name (FocusPat) |
| `\| A` | `\| % A` | arm pattern (FocusPat) |
| `let x =` | `let x = %` | definition (FocusExp) |
| `a/b/` | `a/b/ %` | `b`'s definition |
| `a/b` | `a/ % b` | whole binding of `b` |
| `authenticate` | `% authenticate` | references to name (not call args) |

To focus a **call argument**, use **`name(`** — not bare `name` alone.

### 3.3 Chain expansion

Chains (`A/B/C/`) navigate through named binder definitions:

```
A/B/C/  →  EnterBinder(A), EnterBinder(B), EnterBinder(C), focus
        ≈  _ A = (\... _ B = (\... _ C = %))
```

| Chain form | Meaning |
|------------|---------|
| `A/B/C` | Last name matched; focus on whole binding of `C` |
| `A/B/C/` | All names enter definitions; focus on `C`'s definition |

Works across `let`, `module`, and `type` (any definition keyword at each level).

**Chain through shadows:** `a/a = %` finds inner binders named `a` inside outer `a`'s definition.

### 3.4 Implicit separator skipping

In sequence spines, separator tokens between children may be omitted:

| Selector | Equivalent |
|----------|------------|
| `[ _ %` | `[ _ , %` |
| `( _ _ %` | `( _ , _ , %` |
| `{ _ %` | `{ _ ; %` |

---

## 4. The `%` Focus Marker

The `%` marker determines what gets focused. Two roles:

1. **Inside a spine** (`let x = %`, `authenticate ( %`): `%` marks which
   child/slot is the focus — navigation enters that position.
2. **Before a spine** (`% let x`): focus on the **current node itself**; the
   following pattern is a shape filter. The result is the whole form, not a child.

```
let x = 42 in x + 1
```

| Selector | Result | Focus type |
|----------|--------|------------|
| `% let x` | whole `let x = 42 in x + 1` | FocusExp |
| `let % x` | pattern `x` | FocusPat |
| `let x = %` | `42` | FocusExp |
| `let x` | pattern `x` | FocusPat (implicit `%`) |

```
case x | A => 1 | B => 2 end
```

| Selector | Result | Focus type |
|----------|--------|------------|
| `case %` | scrutinee `x` | FocusExp |
| `\| % A =>` | pattern `A` | FocusPat |
| `\| A => %` | `1` | FocusExp |
| `\| % =>` | patterns `A`, `B` | FocusPat (multi-match) |

### Focus disambiguation for calls

| Form | Meaning |
|------|---------|
| `name(` | First **argument slot** of calls to `name` |
| `% name …` at expression start | **Whole** form headed by `name` (e.g. entire call) |
| `name` alone | **References** to `name` (variables, constructors) |

---

## 5. Composition Operators

### 5.1 Descent (`\...`)

`\...` searches the subtree rooted at the current position for a node where
the inner selector can proceed.

```
let a = (let b = 42 in b) in a
```

| Selector | Result |
|----------|--------|
| `let b = %` | no match (b not at top level) |
| `\... let b = %` | `42` |
| `a = \... let b = %` | `42` |

**Multiple matches:**

| Selector | Result |
|----------|--------|
| `\... \| _ => %` | all case arm bodies in program |
| `f/ \... \| _ => %` | arm bodies only inside `f` |
| `\... fun _ -> %` | all function bodies |
| `\... authenticate(` | argument at matching call sites |

Double descent `\... \...` collapses to `\...` during elaboration.

### 5.2 Chains (`A/B/C`)

```
let a = { let x = 1; let b = { let y = 42 } } in a.b.y
```

| Selector | Result |
|----------|--------|
| `a/b/y = %` | `42` |
| `a/x = %` | `1` |
| `a/b/ \... let y = %` | `42` |

---

## 6. Child Index (`#N`)

`#N` descends into the Nth structural child, numbered left-to-right.

| Form | Children |
|------|----------|
| `Let(pat, def, body)` | `#0` pat, `#1` def, `#2` body |
| `Fun(pat, body, …)` | `#0` pat, `#1` body |
| `If(cond, then, else)` | `#0` cond, `#1` then, `#2` else |
| `BinOp(op, e1, e2)` | `#0` left, `#1` right |
| `Ap(fn, arg)` | `#0` fn, `#1` arg |
| `DeferredAp(fn, args)` | `#0` fn, `#1…#n` each argument |
| `Match(scrut, rules)` | `#0` scrut; `#k` (k≥1) rule k−1 pair |
| `Tuple` / `ListLit` | `#0`, `#1`, … elements |
| `Module(items)` | `#0`, `#1`, … items |

**Match rules:** for `case …`, `#N` (N≥1) enters rule N−1; `#N #0` is the arm
pattern, `#N #1` is the arm body.

**Cross-sort:** `#0 #1` on `let x : Int = 42` reaches type `Int` (FocusTyp);
`#0 #0` reaches inner pattern.

```
let x = (1 + 2) + 3 in x
```

| Selector | Result |
|----------|--------|
| `x = #0 #0 #0` | `1` |
| `x = #0 #0 #1` | `2` |
| `x = #1` | `3` |

Named selectors and `#N` compose: `x = #0` after `x =` enters the definition.

---

## 7. Let Bindings

```
let x = 42 in x + 1
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let x = %` | `42` | definition |
| `let x` | pattern `x` | implicit focus |
| `let x _... in %` | `x + 1` | body |
| `let x : %` | type annotation | FocusTyp |
| `let x : _ = %` | `42` | skip annotation |
| `let _ = %` | all let defs | multi-match |
| `let _ = _ in %` | all let bodies | multi-match |
| `x = %` | all defs named `x` | multi-match if shadowed |
| `% let x` | whole let form | predicate focus |

#### Let chains

```
let a = 1 in let b = 2 in a + b
```

| Selector | Result |
|----------|--------|
| `let a = %` | `1` |
| `let b = %` | `2` |
| `let _ = %` | `1`, `2` |
| `a _... in %` | body of `a` |
| `b _... in %` | `a + b` |

---

## 8. Functions

```
let f = fun x -> x + 1 in f 5
```

| Selector | Result |
|----------|--------|
| `let f = %` | whole `fun …` |
| `fun x -> %` | body |
| `fun _ -> %` | body (wildcard param) |
| `f/ fun _ -> %` | body inside `f` |
| `f/ fun x` | parameter pattern |

---

## 9. If, Then, Else

```
if true then 1 else 0
```

| Selector | Result |
|----------|--------|
| `if %` | condition |
| `if _ then %` | then branch |
| `if _ then _ else %` | else branch |
| `if _... else %` | else branch |

---

## 10. Test Expressions

```
let x = 1 in test x == 1 end; x
```

| Selector | Result |
|----------|--------|
| `\... test %` | test body |
| `\... test _... end` | whole test |

---

## 11. Type and Module Definitions

```
type T = Int in let x : T = 42 in x
```

| Selector | Result |
|----------|--------|
| `type T = %` | `Int` |
| `type T _... in %` | body after alias |
| `type T#0 = %` | first `type T` if shadowed |

```
module M = { let x = 1 } in M.x
```

| Selector | Result |
|----------|--------|
| `module M = %` | module definition |
| `module M _... in %` | body after module |
| `module M` | module name pattern |
| `module _ = %` | any module name's def |
| `M/x = %` | `1` via chain |

---

## 12. Binary Operators

```
let x = 1 + 2 in x
```

| Selector | Result |
|----------|--------|
| `x = _ + %` | right operand `2` |
| `x = % + _` | left operand `1` |
| `x = #0` | `1` |
| `x = #1` | `2` |
| `% + _` / `_ + %` | focus operand via operator spine |

Supported: `+`, `-`, `**`, `<`, `<=`, `>`, `>=`, `==`, `!=`, `&&`, `||`,
`++`, `::`, and float variants (`+.`, `-.`, …).

**Limitation:** `/` (division) conflicts with chain syntax — use `#0` / `#1`.

Nested: `x = #0 _ + %` addresses inner binops inside parens.

---

## 13. Atom Matching

Atoms match leaf nodes by **printed string** (`42`, `true`, `x`). Use `\...`
to search anywhere.

```
let x = 42 in let y = true in x + 1
```

| Selector | Result |
|----------|--------|
| `\... 42` | `42` |
| `\... true` | `true` |
| `x = \... 42` | `42` inside `x`'s def |

```
let a = 42 in let b = 42 in a + b
```

| Selector | Result |
|----------|--------|
| `\... 42` | `42`, `42` (multi-match) |

**Bare names:** without keyword context, `x` matches variable/constructor
references (multi-match). `x/` is a chain step.

**Limitation:** string literals with spaces cannot be matched (tokenizer).
Floats match with numeric normalization in the resolver.

Prefer `authenticate( _... % 3` over `\... 3` when scoping to one call.

---

## 14. Sequence Forms

Lists, tuples, module blocks, and case arms use the same sequence-spine
machinery (`%`, `_`, `_...`).

| Form | Separator |
|------|-------------|
| List `[…]` | `,` |
| Tuple `(…)` | `,` |
| Module `{…}` | `;` |
| Case arms | `\|` |

### 14.1 Lists

```
let xs = [1, 2, 3] in xs
```

| Selector | Result |
|----------|--------|
| `xs/ [ %` | `1` |
| `xs/ [ _ %` | `2` |
| `xs/ [ _... %` | `3` |

### 14.2 Tuple literals

A **tuple literal** is a `Tuple` node (often `Parens(Tuple …)`). The `(`
spine walks **elements** when the current node is that tuple — not when the
current node is an `Ap` call.

```
let t = (10, 20, 30) in t
```

| Selector | Result |
|----------|--------|
| `t/ ( %` | `10` |
| `t/ ( _ %` | `20` |
| `t/ ( _ _ %` | `30` |
| `t/ ( _... %` | `30` |

### 14.3 Case arms

```
case msg | Inc => count + 1 | Dec => count - 1 | Reset => 0 end
```

| Selector | Result |
|----------|--------|
| `case %` | scrutinee |
| `| Inc => %` | body of `Inc` arm |
| `| Dec => %` | body of `Dec` arm |
| `| _ => %` | all arm bodies |
| `| % Inc` | pattern `Inc` |
| `case _... | Dec => %` | body of `Dec` (anchors optional but clearer with `\|`) |

Constructor names in arm selectors match **pattern heads**, including applied
constructors (pragmatic shorthand).

### 14.4 Module items

```
module M = { let a = 1; let b = 2; let c = 3 } in M.a
```

| Selector | Result |
|----------|--------|
| `M = { %` | first item |
| `M = { _ %` | second item |
| `M = { _... %` | last item |
| `M = { _... let b = %` | `2` |
| `M = #1 #1` | def of second item |
| `M/b = %` | chain equivalent to named item |

---

## 15. Function Application

A call `f(e1, e2, …)` is **`Ap(function, argument)`**. The argument is often
one child (tuple or parenthesized tuple):

```text
Ap
├── f
└── (e1, e2, …)
```

**Partial application** with program deferrals — `f(_, e2)` — is
**`DeferredAp(function, [arg₀, arg₁, …])`**, a flat argument list:

```text
DeferredAp
├── f
├── _     ← program deferral (not selector `_`)
├── e2
└── …
```

Selectors use **`name(`** to match calls to `name`, then sequence-spine
rules on argument slots. Tuple literal `( … )` and call **`name( … )`** share
slot syntax but attach at different AST nodes (`Tuple` vs `Ap` / `DeferredAp`).

> Selector `_` skips a slot; program `_` is a deferral hole.

### 15.1 Complete calls (`Ap`)

```
let ok = authenticate(3, 4, 5) in ok
```

| Selector | Result |
|----------|--------|
| `authenticate(` | first argument `3` |
| `authenticate ( _ %` | `4` |
| `authenticate ( _ _ %` | `5` |
| `authenticate ( _... %` | `5` (last) |
| `authenticate ( _... % 3` | argument equal to `3` |
| `ok = authenticate(` | `3` under binding |
| `\... authenticate(` | `3` at call site |
| `% authenticate ( _... )` | whole call |
| `ok = %` | whole RHS |

Unary:

```
let r = f(3) in r
```

| Selector | Result |
|----------|--------|
| `f(` | `3` |

### 15.2 Partial calls (`DeferredAp`)

```
let ok = authenticate(_, 4, 5) in ok
```

| Selector | Result |
|----------|--------|
| `authenticate(` | first slot (deferral) |
| `authenticate ( _ %` | `4` |
| `authenticate ( _ _ %` | `5` |
| `authenticate ( _... %` | `5` |
| `authenticate ( _... % 4` | `4` |
| `ok = authenticate(` | deferral under binding |
| `% authenticate ( _... )` | whole partial call |

```
let h = f(_) in h
```

| Selector | Result |
|----------|--------|
| `f(` | deferral |

### 15.3 Chains and calls

```
module App = { let update = fun msg -> case msg | Inc => … end } in …
```

| Selector | Result |
|----------|--------|
| `App/update/ \... | Inc => %` | arm body in `update` |
| `App/update \... case %` | scrutinee in `update` |

---

## 16. Shadowed Names and Indexing

```
let x = 1 in let x = 2 in x
```

| Selector | Result |
|----------|--------|
| `x#0 = %` | `1` |
| `x#1 = %` | `2` |
| `x#0 _... in %` | body of first `x` |
| `x#5 = %` | error (out of range) |
| `x = %` | `1`, `2` (multi-match) |

Edits require a unique match — use `x#0 = %` when shadowed.

Indexing applies to `module` and `type` as well (`module M#1 = %`, `type T#0 = %`).

```
let a = 4 in let a = 4 in let a = 4 in a
```

| Selector | Result |
|----------|--------|
| `a = %` | all three defs |

---

## 17. Canonical Selectors

Two functions generate selectors that identify a node uniquely:

- **`canonical_numeric`** — pure `#N` path from root (`#1 #0 #0 %`).
- **`canonical_named`** — prefers names and keywords; falls back to `#N`
  (`x = % + _`, `if _ then %`, `x#0 = %` when shadowed).

**Roundtrip:** resolving the canonical selector returns the same node id.

**`deparse`** converts the elaborated selector back to a surface string.

---

## 18. Edit Actions

| Action | Behavior |
|--------|----------|
| `SelectorUpdate(sel, code)` | Replace focused node; requires unique match |
| `SelectorDelete(sel)` | Remove focused node |
| `SelectorInsertBefore` / `After` | Insert relative to focus; context-dependent |

Focus sort determines replacement: **FocusExp**, **FocusPat**, **FocusTyp**, **FocusMod**.

**Insert semantics** (first match wins):

1. Case arm body → insert new rule
2. List element → insert element
3. Tuple element → insert element
4. Module item → insert item
5. Fallback → wrap in `let` (permissive; agent should use sensible selectors)

Examples:

```
module M = { let x = 1 } in M.x
SelectorInsertAfter("M/x = %", "let y = 2")
→ module M = { let x = 1; let y = 2 } in M.x
```

```
let f = fun x -> case x | A => 1 | B => 2 end in f
SelectorInsertAfter("| B => %", "C => 3")
→ … | B => 2 | C => 3 end …
```

---

## 19. Error Diagnostics

When a selector doesn't match, diagnostics include:

- How far matching progressed
- Which step failed
- Available names at the failure point
- "Did you mean?" for close misspellings

When prefix elaboration runs, errors may show the elaborated selector
(e.g. `authenticate(` → `authenticate ( %`).

```
let foo = 1 in let bar = 2 in foo + bar
```

| Selector | Error hint |
|----------|------------|
| `let baz = %` | Failed at: baz; Did you mean: bar |
| `if %` | Failed at first step: if |
| `M/z = %` | Failed at: z; Available: x, y |

---

## 20. Known Limitations

1. **`/` operator** — conflicts with chain syntax; use `#N`.
2. **Permissive insert** — binding fallback may wrap nonsensical positions.
3. **String literals with spaces** — tokenizer splits on whitespace.
4. **Single focus** — one `%`; no `%a`, `%b` yet.
5. **Post-focus trailing tokens** — e.g. `fun _ -> % +` not supported.
6. **Derivation quotes** — not supported in selector resolution.

---

## Implementation Notes

### Pipeline (current)

```
selector string  →  tokenize  →  elaborate  →  resolve_sem  →  match_result list
```

Implementation uses a flat `list(sem_step)` resolver in `Selector.re` (not yet
the full recursive spine AST in `plans/selector-calculus.md`). Application
spine matching (`walk_ap_spine` for `Ap` and `DeferredAp`) extends the same
step-list architecture.

### Key files

- `src/haz3lcore/CompositionCore/Selector.re` — tokenizer, elaboration, resolver, diagnostics, canonical, deparse
- `src/haz3lcore/CompositionCore/CompositionGo.re` — edit/read dispatch
- `src/haz3lcore/CompositionCore/CompositionActions.re` — action types
- `src/haz3lcore/CompositionCore/HighLevelNodeMap.re` — paths (parallel addressing)
- `src/haz3lcore/CompositionCore/TermEdit.re` — term transformations for edits
- `src/haz3lcore/CompositionCore/ToolJsonDefinitions/` — agent tool JSON
- `test/Test_AgentTools.re` — selector tests (`AgentTools.Selectors`, `AgentTools.Gaps`)

### Related specs

- `plans/selector-calculus.md` — formal spine-calculus target (rewrite plan)
- `plans/selector-rewrite-plan.md` — resolver rewrite toward that AST
- `plans/coding-agent-actions.md` — agent tools and path vs selector roles
