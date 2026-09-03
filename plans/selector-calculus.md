# Selector Calculus: Formal Specification & Reference

Selectors are **pattern combinators over syntax**. A selector navigates from
the root of a Hazel program to a specific subterm (the *focus*) by
pattern-matching against the syntactic structure along the way.

The core insight: every Hazel syntactic form has a *spine* — an alternating
sequence of tokens (keywords, delimiters, operators) and children (subterms).
A selector walks spines, matching tokens literally and children by name or
wildcard, with one distinguished child marked as the focus (`%`).

Think of selectors as destructuring patterns where:
- Literal tokens (keywords, delimiters) are exact match constraints
- `_` is a wildcard matching any child
- `%` is a distinguished capture variable (the focus)
- `_...` skips arbitrary spine positions
- `\...` searches all descendants

### Architectural principle: sort- and form-generic

The resolver is designed to work **homogeneously across all sorts and forms**
in the Hazel grammar. Every syntactic form — across Exp, Pat, Typ, Mod, Sig,
TPat, MPat — has a spine structure that can be decomposed into an alternating
sequence of tokens and children. The resolver operates on this uniform
decomposition rather than having per-form or per-sort logic.

This means:
- **Adding a new form** requires only adding a case to `decompose` (mapping
  the term variant to its spine positions). The resolver, spine matcher,
  descent, and all combinators work automatically.
- **Cross-sort navigation** (e.g., from Exp through Pat into Typ) is handled
  by the `focus_target` sum type. Spine matching transparently crosses sort
  boundaries when a child has a different sort than its parent.
- **Sequence forms** (lists, tuples, modules, case arms) are decomposed into
  flat spines with separator tokens, handled uniformly by the same matcher.
- The initial implementation covers the forms exercised by the test suite and
  spec examples, but the architecture supports all forms in `Form.re` and
  can be extended incrementally.

---

## 1. Meta-Level Syntax

These characters have meta-level meaning in the selector language
(as opposed to being Hazel object syntax reused literally):

| Character | Role |
|-----------|------|
| `%` | Focus — capture this position |
| `_` | Wildcard — match any child |
| `_...` | Ellipsis — skip spine positions |
| `\...` | Descent — search all descendants |
| `#N` | Child index — enter Nth structural child |
| `name/` | Chain step — enter binder's definition |
| `name#N` | Indexed name — Nth binder with this name |

Everything else — keywords (`let`, `if`, `fun`, ...), delimiters (`=`, `->`,
`=>`, `|`, `:`, ...), operators (`+`, `-`, `&&`, ...), brackets (`[`, `]`,
`(`, `)`), and names (`x`, `foo`, `MyModule`) — is Hazel object syntax
reused literally as match targets.

---

## 2. Core Data Types

```ocaml
(** A selector navigates from the current position to a focus.
    Every constructor except Focus(None) carries a continuation. *)
type selector =
  | Focus of selector option
      (** % — focus on the current node.
          Focus(None) is terminal: we've arrived.
          Focus(Some(k)) is a predicate: focus on the current node,
          but only if k successfully matches when applied to it.
          k's own focus results are discarded — only its success/failure
          matters. The focused result is the current node itself.

          Examples:
            %             →  Focus(None)
            % let x       →  Focus(Some(Spine([Token("let"), ChildNamed("x")])))
            \... % let x  →  Descend(Focus(Some(Spine([...]))))
      *)
  | Spine of spine
      (** Match the current node against a form's spine pattern.
          Exactly one child position in the spine is "active" (ChildSel) —
          it carries the continuation selector for further navigation.
          All other children are matchers only (ChildWild, ChildNamed). *)
  | Descend of selector
      (** \... — search all descendants of current position for a
          node where the inner selector can proceed. *)
  | ChildIdx of int * selector
      (** #N — enter the Nth structural child, continue with selector. *)
  | EnterBinder of binder_ref * selector
      (** name/ — enter a named binder's definition, continue.
          Sugar: elaborates to Spine with TokenWild + Descend. *)

(** A spine pattern matches one syntactic form's token/child structure.

    Forms alternate tokens and children. Key constraint: children are
    never adjacent — at least one token separates any two children.

    Prefix forms start with a token:  let C = C in C
    Infix forms start with a child:   C + C

    Unmatched trailing positions are implicitly wild (prefix matching).
    Equivalently, every spine has an implicit trailing Ellipsis. *)
type spine = spine_elem list

type spine_elem =
  | Token of string
      (** Match a specific keyword, delimiter, or operator *)
  | TokenWild
      (** Match any token *)
  | ChildWild
      (** _ — match any child, don't enter *)
  | ChildNamed of binder_ref
      (** Match child by binder name, don't enter *)
  | ChildSel of binder_ref option * selector
      (** The active child — optionally constrain by name,
          then continue with the sub-selector inside this child.
          % alone is ChildSel(None, Focus(None)).
          Exactly one ChildSel per spine. *)
  | Ellipsis
      (** _... — skip forward to the next matching anchor *)
  | Atom of string
      (** Match an atomic/leaf node by its printed representation.
          e.g., "42", "true", "hello" *)

type binder_ref =
  | Name of string
  | NameIdx of string * int   (** x#N — Nth binder with this name *)
```

### Design notes

**Sequencing is structural.** Each constructor carries its continuation
explicitly: `Descend(k)` means "search descendants, then apply k."
`ChildIdx(n, k)` means "enter child n, then apply k." Nesting comes
from `ChildSel(_, k)` inside a spine — one child is designated as
the continuation point.

**Focus has two roles.** `Focus(None)` is terminal — we've arrived.
`Focus(Some(k))` is a predicate focus: "focus on this node itself,
but only if `k` matches it." The inner selector `k` is applied to the
*same* node; its own focus results are discarded, only success/failure
matters. This is what makes `% let x` work — it focuses on the whole
let expression (not a child), using `let x` as a shape filter.
Compare with `Spine`, which always navigates INTO a child via ChildSel.

**ChildSel combines name matching with continuation.** In `let x`,
the child "x" is both matched by name AND focused. This is represented
as `ChildSel(Some(Name("x")), Focus(None))` — the `Some(name)` is a
filter, the `selector` is the continuation.

**Atom is a string, not a typed variant.** Matching `42`, `3.14`, or
`"hello"` compares the printed representation. No need to parse into
typed values — avoids float representation issues and automatically
supports new atom kinds.

**Form-generic decomposition.** The resolver does not contain per-form
matching logic. Instead, each term variant is *decomposed* into a flat
spine of `PosToken(string)` and `PosChild(focus_target)` entries. The
spine matcher operates on this uniform representation. For example:

```
Let(pat, def, body)  →  [Token("let"), Child(pat), Token("="), Child(def), Token("in"), Child(body)]
BinOp(Plus, e1, e2)  →  [Child(e1), Token("+"), Child(e2)]
Module([m1, m2])     →  [Token("{"), Child(m1), Token(";"), Child(m2), Token("}")]
Var("x")             →  atom "x"
```

This decomposition is the *only* place that needs per-form knowledge.
Everything else — spine matching, ellipsis handling, descent, child
indexing, binder search — is generic.

---

## 3. Sugars and Conventions

### 3.1 Completion via trailing ellipsis

A spine pattern matches as a prefix — unmatched trailing positions
are implicitly wild. This is equivalent to appending `_...` at the end.

This works uniformly for both fixed-arity and variable-arity forms:

| Abbreviated | Equivalent to |
|---|---|
| `let x = %` | `let x = % _...` (absorbs `in body`) |
| `if %` | `if % _...` (absorbs `then ... else ...`) |
| `[ _ %` | `[ _ % _...` (absorbs remaining elements) |
| `{ _... let b = %` | `{ _... let b = % _...` (absorbs remaining items) |

### 3.2 Implicit focus

If no `%` appears in the selector:
- If the last token is a **name**, `%` is inserted **before** it
  (focuses on the named term, typically a pattern).
- Otherwise, `%` is **appended** at the end
  (focuses on the subexpression after the last delimiter).

| Written | Elaborated | Focuses on |
|---------|-----------|------------|
| `let x` | `let %x` | pattern x (FocusPat) |
| `fun x` | `fun %x` | parameter pattern (FocusPat) |
| `type T` | `type %T` | type name (FocusTPat) |
| `module M` | `module %M` | module name (FocusPat) |
| `\| A` | `\| %A` | arm pattern (FocusPat) |
| `let x =` | `let x = %` | definition (FocusExp) |
| `a/b/` | `a/b/ %` | b's definition |
| `a/b` | `a/ %b` | whole binding of b |

### 3.3 Chain expansion

Chains (`A/B/C/`) are sugar for navigating through binder definitions.
`EnterBinder` elaborates to `Spine` with `TokenWild` (matching any
definition keyword — `let`, `module`, `type`) plus `Descend`:

```
A/B/C/  →  EnterBinder(A, EnterBinder(B, EnterBinder(C, Focus(None))))
        ≈  _ A = (\... _ B = (\... _ C = %))
```

The `TokenWild` is what makes chains work uniformly across `let`,
`module`, and `type` definitions.

### 3.4 Implicit separator skipping

In sequence forms (lists, tuples, modules, case arms), separator tokens
(`,`, `;`, `|`) between children are implicitly matched when two child
patterns are adjacent. Writing them is optional:

| Selector | Equivalent |
|----------|------------|
| `[ _ %` | `[ _ , %` |
| `( _ _ %` | `( _ , _ , %` |
| `{ _ %` | `{ _ ; %` |

The separator is the only token that can appear between children in that
form, so it's unambiguous to omit.

---

## 4. Prefix Forms

### 4.1 Let Bindings

```
let x = 42 in x + 1
```

| Abbreviated | Expanded | Parsed |
|---|---|---|
| `let x = %` | `let x = % in _` | `Spine([Token("let"), ChildNamed(Name("x")), Token("="), ChildSel(None, Focus(None))])` |
| `let x` | `let %x in _` | `Spine([Token("let"), ChildSel(Some(Name("x")), Focus(None))])` |
| `let x _... in %` | `let x _... in %` | `Spine([Token("let"), ChildNamed(Name("x")), Ellipsis, Token("in"), ChildSel(None, Focus(None))])` |
| `x = %` | `_ x = % _...` | `Spine([TokenWild, ChildNamed(Name("x")), Token("="), ChildSel(None, Focus(None))])` |
| `% let x` | `% let x _...` | `Focus(Some(Spine([Token("let"), ChildNamed(Name("x"))])))` — focus on whole form |

Note: `let x = %` naively expands to `let x = % in _` with explicit
trailing delimiters. Equivalently, `let x = % _...` where the ellipsis
absorbs `in body`. Both perspectives are correct — use whichever is
clearer in context.

#### Let chains

```
let a = 1 in let b = 2 in a + b
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let a = %` | `1` | first let |
| `let b = %` | `2` | second let |
| `let _ = %` | `1`, `2` | wildcard: matches BOTH lets |
| `a _... in %` | `let b = 2 in a + b` | body of a |
| `b _... in %` | `a + b` | body of b |

### 4.2 Type Annotations and Definitions

#### Annotated let

```
let x : Int = 42 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let x = %` | `42` | definition (annotation is transparent) |
| `let x : %` | `Int` | **FocusTyp** — the type annotation |
| `let x : _ = %` | `42` | skip annotation, focus on def |

#### Type alias

```
type T = Int in let x : T = 42 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `type T = %` | `Int` | **FocusTyp** — the type definition |
| `type T _... in %` | `let x = 42 in x` | body after type alias |
| `type T` | `T` | type pattern (FocusTPat, implicit focus) |

### 4.3 Module Definitions

```
module M = { let x = 1 } in M.x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `module M = %` | `{ let x = 1 }` | module def |
| `module M _... in %` | `M.x` | body after module |
| `module M` | `M` | module pattern (FocusPat, implicit focus) |
| `module _ = %` | `{ let x = 1 }` | wildcard module name |
| `_ M = %` | `{ let x = 1 }` | TokenWild: any definition keyword |

### 4.4 Functions

```
let f = fun x -> x + 1 in f 5
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let f = %` | `fun x -> x + 1` | f's definition |
| `f/ fun _ -> %` | `x + 1` | enter f, fun body |
| `f/ fun x` | `x` | parameter pattern (implicit focus) |

### 4.5 If/Then/Else

```
if true then 1 else 0
```

| Abbreviated | Expanded | Parsed |
|---|---|---|
| `if %` | `if % then _ else _` | `Spine([Token("if"), ChildSel(None, Focus(None))])` |
| `if _ then %` | `if _ then % else _` | `Spine([Token("if"), ChildWild, Token("then"), ChildSel(None, Focus(None))])` |
| `if _... else %` | `if _ then _ else %` | `Spine([Token("if"), Ellipsis, Token("else"), ChildSel(None, Focus(None))])` |

### 4.6 Test Expressions

```
let x = 1 in test x == 1 end; x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... test %` | `x == 1` | test body |
| `\... test _... end` | `test x == 1 end` | whole test (implicit focus) |

---

## 5. Infix Forms

### Binary operators

Operators act as tokens in the spine, with children on either side:

```
let x = 1 + 2 in x
```

| Abbreviated | Expanded | Parsed |
|---|---|---|
| `x = _ + %` | `_ x = (_ + %) _...` | `Spine([..., ChildSel(None, Spine([ChildWild, Token("+"), ChildSel(None, Focus(None))]))])` |
| `x = % + _` | `_ x = (% + _) _...` | `Spine([..., ChildSel(None, Spine([ChildSel(None, Focus(None)), Token("+"), ChildWild]))])` |

#### Other operators

```
y = _ && %     →  right of &&
y = % && _     →  left of &&
z = _ :: %     →  cons tail
z = % :: _     →  cons head
```

#### Nested operators

```
let x = (1 + 2) + 3 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `x = _ + %` | `3` | right of outer + |
| `x = #0 _ + %` | `2` | right of inner + (via child index into Parens) |
| `x = #0 % + _` | `1` | left of inner + |

Supported operators: `+`, `-`, `**`, `<`, `<=`, `>`, `>=`, `==`, `!=`,
`&&`, `||`, `++`, `::`, and float variants (`+.`, `-.`, etc.).

**Limitation**: `/` (division) conflicts with chain syntax.
Use `#0`/`#1` to address operands of `/`.

---

## 6. Atom Matching

Atoms are leaf nodes — literals, variable references, constructors.
They have no children or delimiters, so they match by their printed
representation as a string. Use `\...` to search for an atom anywhere
in the program.

```
let x = 42 in let y = true in x + 1
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... 42` | `42` | integer literal |
| `\... true` | `true` | boolean literal |
| `x = \... 42` | `42` | find literal inside x's def |

```
let a = 42 in let b = 42 in a + b
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... 42` | `42`, `42` | multi-match: finds all occurrences |

Atoms match against the printed form of the node — `42` matches
`Int(42)`, `true` matches `Bool(true)`, etc. No parsing into typed
values; the comparison is string-based.

#### Bare names as atoms

A bare name without keyword context matches as an atom — it finds
variable references, constructor names, etc. Names are distinguished
from chain steps by the absence of a trailing `/`.

```
let x = 1 in x + x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `x` | `x`, `x` | matches both variable references |
| `\... x` | `x`, `x` | equivalent with explicit descent |
| `x/` | enters `x`'s def | chain step (trailing `/`) |

When a name appears after a keyword (`let x`, `type T`), it matches
as a binder name — this takes priority. Without a keyword, the name
falls through to atom matching against all leaf nodes.

**Limitation**: String literals with spaces can't be matched (tokenizer
splits on whitespace). Adjacent tokens without spaces (e.g., `_,`)
are parsed as a single token — always separate with spaces.

---

## 7. Sequence Forms

Lists, tuples, case arms, and module items are all variable-length
sequences of children separated by tokens. The same spine mechanism
handles all of them — the difference is the separator token and the
complexity of each "unit."

| Form | Unit | Separator | Children per unit |
|------|------|-----------|-------------------|
| List `[...]` | single element | `,` | 1 |
| Tuple `(...)` | single element | `,` | 1 |
| Case `case..end` | `pat => body` | `\|` | 2 (compound) |
| Module `{...}` | `let x = def` etc. | `;` | compound (sub-spine) |

For single-child units (list, tuple), each `_` skips one element.
For compound units (case, module), use `_...` with token anchors
(`|`, `=>`, `let`, etc.) to navigate ergonomically.

### 6.1 Lists

```
let xs = [1, 2, 3] in xs
```

| Selector | Result | Notes |
|----------|--------|-------|
| `xs/ [ %` | `1` | first element |
| `xs/ [ _ %` | `2` | second element (comma implicit) |
| `xs/ [ _... %` | `3` | last element (ellipsis skips all) |

### 6.2 Tuples

```
let t = (10, 20, 30) in t
```

| Selector | Result | Notes |
|----------|--------|-------|
| `t/ ( %` | `10` | first element |
| `t/ ( _ %` | `20` | second (comma implicit) |
| `t/ ( _ _ %` | `30` | third by counting |
| `t/ ( _... %` | `30` | last via ellipsis |

### 6.3 Case Arms

```
case msg | Inc => count + 1 | Dec => count - 1 | Reset => 0 end
```

| Selector | Result | Notes |
|----------|--------|-------|
| `case %` | `msg` | scrutinee |
| `\| Inc => %` | `count + 1` | arm body by constructor name |
| `\| Dec => %` | `count - 1` | another named arm |
| `\| _ => %` | 3 matches | ALL arm bodies (wildcard) |
| `\| % Inc` | `Inc` | arm pattern (FocusPat) |

**Bars and arrows**: The `|` and `=>` tokens serve as structural
anchors in the case spine. They're recommended but not always required:
`_... Dec => %` also works if "Dec" is unambiguous. The `|` confirms
you're at the start of a rule (not matching "Dec" elsewhere), while
`=>` separates pattern from body. For maximum clarity, include both.

### 6.4 Module Items

```
module M = { let a = 1; let b = 2; let c = 3 } in M.a
```

| Selector | Result | Notes |
|----------|--------|-------|
| `M = { %` | `let a = 1` | first item (FocusMod) |
| `M = { _ %` | `let b = 2` | second item (semicolon implicit) |
| `M = { _... %` | `let c = 3` | last item |
| `M = { _... let b = %` | `2` | item by name (enters let, FocusExp) |

The `{ }` brackets are the module form's leading/closing tokens.
`_...` skips items and their semicolons. Nested spines let you match
structure within items: `{ _... let b = % _... }` finds b's definition
regardless of position.

Compare with chain syntax and child index for the same module:

| Selector | Result | Notes |
|----------|--------|-------|
| `M/b = %` | `2` | chain (sugar for above) |
| `M = #0` | `let a = 1` | first item by index (FocusMod) |
| `M = #1` | `let b = 2` | second item by index |
| `M = #1 #1` | `2` | second item's def (FocusExp) |

---

## 8. Composition Operators

### 8.1 Descent (`\...`)

`\...` searches the entire subtree rooted at the current position
for a node where the inner selector can match.

```
let a = (let b = 42 in b) in a
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let b = %` | ERROR | b is not at the top level |
| `\... let b = %` | `42` | descend finds b inside a's def |
| `a = \... let b = %` | `42` | enter a, then descend |

#### Multiple matches

```
let f = fun x -> case x | A => 1 | B => 2 end in
let g = fun y -> case y | C => 3 | D => 4 end in f(g(0))
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... \| _ => %` | `1`, `2`, `3`, `4` | all arm bodies everywhere |
| `f/ \... \| _ => %` | `1`, `2` | only inside f |
| `g/ \... \| _ => %` | `3`, `4` | only inside g |
| `\... fun _ -> %` | 2 matches | all function bodies |

Double descent `\... \...` collapses to `\...` during elaboration.

### 8.2 Chains (`A/B/C`)

Chains are sugar for navigating through named binder definitions:

```
let a = { let x = 1; let b = { let y = 42 } } in a.b.y
```

| Selector | Result | Notes |
|----------|--------|-------|
| `a/b/y = %` | `42` | three levels deep |
| `a/x = %` | `1` | sibling member |
| `a/b/ \... let y = %` | `42` | chain + explicit descent |

**Chain syntax**:
- `A/B/C` — last name is a MatchName (focuses on whole binding of C)
- `A/B/C/` — all names are EnterBinder (focuses on C's definition)
- Spaces allowed: `A/ B/ C/` is the same as `A/B/C/`

**Chain through shadows**: `a/a` tries all binders named `a`:

```
let a = 4 in let a = (let a = 0 in 4) in let a = 4 in a
```

| Selector | Result |
|----------|--------|
| `a/a = %` | `0` (finds inner `a` inside second `a`'s def) |

### 8.3 Child Index (`#N`)

`#N` descends into the Nth structural child, numbered left-to-right.

| Form | Children |
|------|----------|
| `Let(pat, def, body)` | `#0`=pat, `#1`=def, `#2`=body |
| `Fun(pat, body, _, _)` | `#0`=pat, `#1`=body |
| `If(cond, then, else)` | `#0`=cond, `#1`=then, `#2`=else |
| `BinOp(op, e1, e2)` | `#0`=e1, `#1`=e2 |
| `Tuple(items)` / `ListLit(items)` | `#0`, `#1`, `#2`, ... |
| `Match(scrut, rules)` | `#0`=scrut, `#1`=rule0, `#2`=rule1, ... |
| `Module(items)` | `#0`=item0, `#1`=item1, ... |
| `ModLet(pat, def)` | `#0`=pat, `#1`=def |

Child index crosses sort boundaries seamlessly:

```
let x : Int = 42 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `#0` | `x : Int` | pattern (FocusPat) |
| `#0 #0` | `x` | inner pat (FocusPat) |
| `#0 #1` | `Int` | type annotation (FocusTyp) |

Named selectors and `#N` compose — navigate by name, then drill
into anonymous structure:

```
let x = 10 + 20 in x
```

| Selector | Result |
|----------|--------|
| `x = #0` | `10` (left operand) |
| `x = #1` | `20` (right operand) |

---

## 9. The % Focus Marker: Full Position Spectrum

The `%` marker determines what gets focused. It has two forms:

1. **Inside a spine** (`let % x`, `let x = %`): `%` marks which child
   position is the focus target. It becomes `ChildSel` in the spine,
   navigating INTO a child.

2. **Before a spine** (`% let x`): `%` wraps the entire selector as
   `Focus(Some(k))`, focusing on the **current node itself**. The
   following spine pattern is a filter — it verifies the node's shape
   but the focused result is the whole node, not a child.

```
let x = 42 in x + 1
```

| Selector | Result | Focus type | Notes |
|----------|--------|------------|-------|
| `% let x` | `let x = 42 in x + 1` | FocusExp | `Focus(Some(Spine(...)))` — whole form |
| `let % x` | `x` | FocusPat | `ChildSel` at pattern position |
| `let x = %` | `42` | FocusExp | `ChildSel` at def position |
| `let x` | `x` | FocusPat | implicit % before last name |

```
case x | A => 1 | B => 2 end
```

| Selector | Result | Focus type |
|----------|--------|------------|
| `case %` | `x` | FocusExp (scrutinee) |
| `\| % A =>` | `A` | FocusPat (arm pattern) |
| `\| A => %` | `1` | FocusExp (arm body) |
| `\| % =>` | `A`, `B` | FocusPat (all arm patterns) |

---

## 10. Shadowed Names and Indexing

When multiple bindings share the same name, use `name#N` (0-based):

```
let x = 1 in let x = 2 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `x#0 = %` | `1` | first binding |
| `x#1 = %` | `2` | second binding |
| `x#0 _... in %` | `let x = 2 in x` | body of first x |
| `x#5 = %` | ERROR: "2 binding(s) named 'x'" | out-of-range |

Without indexing, selectors match **all** shadowed bindings (multi-match):

```
let a = 4 in let a = 4 in let a = 4 in a
```

| Selector | Result |
|----------|--------|
| `a = %` | `4`, `4`, `4` (all 3 bindings) |

For edit actions (`SelectorUpdate`, `SelectorDelete`), use `a#0 = %`
to target a specific binding (single-match required).

Indexing works for `module` and `type` keywords too:

```
module M = { let x = 1 } in module M = { let y = 2 } in M.y
```

| Selector | Result |
|----------|--------|
| `module M#0 = %` | `{ let x = 1 }` |
| `module M#1 = %` | `{ let y = 2 }` |

---

## 11. Worked Examples

### 11.1 MVU Application

```
module App = {
  let init = 0;
  let update = fun msg -> case msg
    | Inc => msg + 1
    | Dec => msg - 1
    | Reset => 0
    end;
  let view = fun model -> let label = model + 1 in label
} in
let result = App.update(App.init) in result
```

#### Named selectors

| Selector | Result | Technique |
|----------|--------|-----------|
| `App/init = %` | `0` | chain |
| `App/update \... case %` | `msg` | chain + descend + scrutinee |
| `App/update \... \| Inc => %` | `msg + 1` | chain + descend + named arm |
| `App/update \... \| Dec => %` | `msg - 1` | named arm |
| `App/update \... \| Reset => %` | `0` | named arm |
| `App/update \... \| _ => %` | 3 matches | all arms |
| `App/view \... let label = %` | `model + 1` | chain + descend + nested let |
| `module App = %` | `{ let init = 0; ... }` | whole module def |
| `App _... in %` | `let result = ...` | body after module |
| `result = %` | `App.update(App.init)` | top-level let |
| `\... fun _ -> %` | 2+ matches | all function bodies |

#### Structural selectors

| Selector | Result | Technique |
|----------|--------|-----------|
| `App = { %` | `let init = 0` | first module item |
| `App = { _ %` | `let update = ...` | second module item |
| `App = { _ _ %` | `let view = ...` | third module item |
| `App = #0` | `let init = 0` | first item via child index |
| `App = #0 #1` | `0` | init's definition |
| `App = #1 #1 #1 #0` | `msg` | update → fun body → case scrutinee |

### 11.2 Multi-Module Project

A program structured like a multi-file project, with top-level modules
acting as "files" and nested modules as "directories":

```
module Types = {
  type point = (Int, Int);
  type color = (Int, Int, Int)
} in
module Geom = {
  let origin = (0, 0);
  let translate = fun p -> fun dx ->
    let x = p.0 + dx in
    let y = p.1 in
    (x, y);
  module Shapes = {
    let circle = fun center -> fun radius -> (center, radius);
    let rect = fun tl -> fun br -> (tl, br)
  }
} in
module Render = {
  let draw = fun shape -> fun color ->
    if shape.1 > 0 then color else (0, 0, 0)
} in
let canvas = Render.draw(Geom.Shapes.circle(Geom.origin)(5))((255, 0, 0)) in
canvas
```

#### Cross-module navigation

| Selector | Result | Technique |
|----------|--------|-----------|
| `Types/point = %` | `(Int, Int)` | chain into type def (FocusTyp) |
| `Types/color = %` | `(Int, Int, Int)` | another type def |
| `Geom/origin = %` | `(0, 0)` | chain into let def |
| `Geom/Shapes/circle = %` | `fun center -> fun radius -> ...` | nested module chain |
| `Geom/Shapes/rect = %` | `fun tl -> fun br -> ...` | sibling in nested module |
| `Render/draw = %` | `fun shape -> fun color -> ...` | different top-level module |

#### Deep access within definitions

| Selector | Result | Technique |
|----------|--------|-----------|
| `Geom/translate \... fun _ -> %` | `let x = ... in ...` | chain + descend + fun body |
| `Geom/translate \... let x = %` | `p.0 + dx` | chain + descend + nested let |
| `Geom/translate \... let y = %` | `p.1` | another nested let |
| `Render/draw \... if %` | `shape.1 > 0` | chain + descend + condition |
| `Render/draw \... if _ then %` | `color` | then branch |
| `Render/draw \... if _... else %` | `(0, 0, 0)` | else branch |

#### Module-level operations

| Selector | Result | Technique |
|----------|--------|-----------|
| `module Types` | `Types` | module name (FocusPat) |
| `module Geom = %` | `{ let origin = ...; ... }` | whole module body |
| `Geom = { %` | `let origin = (0, 0)` | first item in Geom |
| `Geom = { _... %` | `module Shapes = ...` | last item in Geom |
| `Geom = { _... let translate = %` | `fun p -> fun dx -> ...` | item by name |
| `\... module Shapes = %` | `{ let circle = ...; ... }` | descend to nested module |
| `Geom/Shapes = { %` | `let circle = ...` | first item in nested module |
| `Geom/Shapes = { _ %` | `let rect = ...` | second item |

#### Wildcard queries

| Selector | Result | Technique |
|----------|--------|-----------|
| `\... fun _ -> %` | 5+ matches | all function bodies in program |
| `\... type _ = %` | 2 matches | all type definitions |
| `\... let _ = %` | all let defs | every definition everywhere |
| `_ = % _...` | multiple | all top-level definition RHSes |

### 11.3 Data Processing Pipeline

A program exercising lists, tuples, case expressions, and operators:

```
type status = +Active +Inactive +Pending in
let users = [
  ("Alice", Active),
  ("Bob", Inactive),
  ("Carol", Pending)
] in
let is_active = fun user -> case user.1
  | Active => true
  | Inactive => false
  | Pending => false
  end in
let count = fun xs -> case xs
  | [] => 0
  | _ :: tl => 1 + count(tl)
  end in
let result = count(users) in result
```

#### List and tuple access

| Selector | Result | Technique |
|----------|--------|-----------|
| `users/ [ %` | `("Alice", Active)` | first list element |
| `users/ [ _ %` | `("Bob", Inactive)` | second element |
| `users/ [ _... %` | `("Carol", Pending)` | last element |

#### Case arm access

| Selector | Result | Technique |
|----------|--------|-----------|
| `is_active/ \... case %` | `user.1` | scrutinee |
| `is_active/ \... \| Active => %` | `true` | arm by constructor |
| `is_active/ \... \| Pending => %` | `false` | another arm |
| `is_active/ \... \| _ => %` | 3 matches | all arm bodies |
| `count/ \... \| [] => %` | `0` | base case |
| `count/ \... \| _ :: tl => %` | `1 + count(tl)` | recursive case |

#### Cross-cutting queries

| Selector | Result | Technique |
|----------|--------|-----------|
| `\... case %` | 2 matches | all scrutinees |
| `\... \| _ => %` | 5 matches | all case arm bodies |
| `\... fun _ -> %` | 2 matches | all function bodies |

---

## 12. Canonical Selectors

Two functions generate selectors that uniquely identify any node:

### Numeric canonical (`canonical_numeric`)

Pure `#N` path from root. Universal, stable, terse.

```
let x = (1 + 2) + 3 in x

Node "1":     #1 #0 #0 #0 %
Node "2":     #1 #0 #0 #1 %
Node "1 + 2": #1 #0 #0 %
Node "3":     #1 #1 %
Node "x":     #2 %
```

### Named canonical (`canonical_named`)

Prefers names and keywords over indices. Falls back to `#N` for
anonymous subexpressions:

```
Node "1":      x = #0 #0 %
Node "2":      x = #0 #1 %
Node "42":     x = %          (when def is just 42)
```

```
if true then 1 else 0

Named "true":  if %
Named "1":     if _ then %
Named "0":     if _... else %
```

For BinOp operands, named canonical uses operator syntax:

```
canonical_named(id_of(1), "let x = 1 + 2 in x")  →  "x = % + _"
canonical_named(id_of(2), "let x = 1 + 2 in x")  →  "x = _ + %"
```

Shadowed names get `#N`: `x#0 = %`, `x#1 = %`.

### Roundtrip guarantee

For any node: `canonical(id, root)` produces a selector that, when
resolved, returns the same node ID. Holds for both numeric and named.

### Deparse

Converts `selector` back to surface syntax string:

```
deparse(ChildIdx(1, ChildIdx(0, Focus(None))))  →  "#1 #0 %"
deparse(Spine([Token("let"), ChildNamed(Name("x")), Token("="), ChildSel(None, Focus(None))]))  →  "let x = %"
deparse(Descend(Spine([Token("let"), ...])))  →  "\\... let ..."
```

---

## 13. Edit Actions

### SelectorUpdate / SelectorDelete

`SelectorUpdate(selector, code)` replaces the focused node with parsed `code`.
`SelectorDelete(selector)` removes the focused node. Both require exactly
one match (`query_unique`).

Focus type determines behavior:
- **FocusExp**: replace/delete expression
- **FocusPat**: replace pattern
- **FocusTyp**: replace type
- **FocusMod**: replace/delete whole module item

### SelectorInsert

`SelectorInsertBefore(selector, code)` and `SelectorInsertAfter(selector, code)`
insert new code relative to the focused node. The insert logic checks the
structural position to determine semantics:

1. **Case arm**: target is an arm body → insert new rule (`"D => 4"`)
2. **List element**: target is inside ListLit → insert element (`"42"`)
3. **Tuple element**: target is inside Tuple → insert element
4. **Module item**: target is inside Module → insert item (`"let y = 2"`)
5. **Binding (fallback)**: wrap in Let (`"let y = 2"`)

#### Examples

```
module M = { let x = 1 } in M.x

SelectorInsertAfter("M/x = %", "let y = 2")
→ module M = { let x = 1; let y = 2 } in M.x

SelectorInsertBefore("M/x = %", "let y = 0")
→ module M = { let y = 0; let x = 1 } in M.x
```

```
let f = fun x -> case x | A => 1 | B => 2 end in f

SelectorInsertAfter("| B => %", "C => 3")
→ let f = fun x -> case x | A => 1 | B => 2 | C => 3 end in f
```

**Note**: The binding fallback (step 5) is permissive — it will wrap
any expression in a Let, even in nonsensical positions. The agent is
responsible for using sensible selectors.

---

## 14. Error Diagnostics

When a selector doesn't match, the error includes:
- How far matching got before failing
- Which step failed
- Available names at the failure point
- "Did you mean?" suggestions for close misspellings

```
let foo = 1 in let bar = 2 in foo + bar
```

| Selector | Error |
|----------|-------|
| `let baz = %` | Matched up to: let / Failed at: baz / Did you mean: bar / Available: foo, bar |
| `if %` | Failed at first step: if |
| `M/z = %` | Failed at: z / Available names: x, y |

---

## 15. Known Limitations

1. **`/` operator**: `/` (division) conflicts with chain syntax.
   Use `#0`/`#1` to address operands of `/`.

2. **Permissive insert**: Binding fallback wraps any expression in Let,
   even in nonsensical positions. Consider adding strict mode.

3. **String literals with spaces**: The tokenizer splits on whitespace,
   so `\... "hello world"` doesn't work. Single-word strings are fine.

4. **No pattern variables**: Only one capture (`%`). Multi-variable
   selectors (`%a`, `%b`) are a future direction.

---

## 16. Future Directions

- **Multi-variable selectors**: `%a`, `%b`, `%c` for extracting
  several subexpressions in one pass.
- **Dynamics/probe integration**: `GetDynamics(path)` for runtime values.
- **Semantic filters**: `@refs(x)`, `@type(Int)`, `@errors` for
  query-based filtering. Multi-cursor edits via `UpdateAll`.
- **Nested selector patterns**: `let (_, x) = %` for tuple-pattern bindings.
- **Spine-schema unification**: Generic resolver parameterized by form
  structure — new forms get selector support by defining a spine schema.

---

## Implementation Notes

### Key files

- `Selector.re` — tokenizer, parser, resolver, diagnostics, canonical, deparse
- `CompositionGo.re` — edit_dispatch (path+selector), read_dispatch
- `CompositionActions.re` — action type definitions
- `HighLevelNodeMap.re` — node map construction, path resolution
- `TermEdit.re` — term-level transformations for all edit operations
- `ExpToSegment.re` — pretty-printing
- `ToolJsonDefinitions/` — JSON tool defs for agent API
- `Test_AgentTools.re` — tests
