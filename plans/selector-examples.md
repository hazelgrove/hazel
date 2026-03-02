# Selector Language: Examples & Reference

This document provides a progressive introduction to the Hazel selector
language through concrete examples. For the formal specification, see
`Hazel-Agent-Path-Selector-Language.md`.

## Grammar Summary

```
Operators:
  _       slot — matches one syntactic position
  _...    ellipsis — matches zero or more positions along a spine
  *       focus — "this is the thing I'm pointing at"
  \...    descend — search all descendants recursively

Keywords:     let  fun  if  then  else  case  end  module  type  in  test
Delimiters:   =  :  ->  =>  |  [  ]  (  )

Names:        x  foo  MyModule  (bare identifiers)
Indexing:     x#0  x#1  (nth binder named x, 0-based)
Child index:  #0  #1  #2  (nth structural child of current node)
Chains:       A/B/C  A/B/C/  (binder navigation sugar)

Implicit star: if no * appears, one is appended at the end.
```

## Semantics at a Glance

| Selector | Elaboration | Points at |
|----------|-------------|-----------|
| `let x = *` | MatchKeyword("let"), MatchName("x"), MatchDelimiter("="), MatchFocus | x's definition (RHS of =) |
| `let x` | MatchKeyword("let"), MatchName("x"), [implicit *] | whole `let x = ... in ...` |
| `x = *` | MatchName("x"), MatchDelimiter("="), MatchFocus | x's definition |
| `x` | MatchName("x"), [implicit *] | whole binding containing x |
| `x/` | EnterBinderDef("x"), [implicit *] | x's definition |
| `A/B/C` | EnterBinderDef(A), EnterBinderDef(B), MatchName(C) , [implicit *] | whole binding of C inside B inside A |
| `A/B/C/` | EnterBinderDef(A), EnterBinderDef(B), EnterBinderDef(C), [implicit *] | C's definition |

---

## 1. Let Bindings — The Foundation

### Program

```
let x = 42 in x + 1
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let x = *` | `42` | definition of x |
| `let x _... in *` | `x + 1` | body after x |
| `let x` | `let x = 42 in x + 1` | whole let expression (implicit *) |
| `x = *` | `42` | same as `let x = *` but without keyword |
| `x` | `let x = 42 in x + 1` | bare name → whole binding |
| `x/` | `42` | trailing slash → enter def |
| `* let x` | `let x = 42 in x + 1` | * prefix → focus on what follows |

### Let chains

```
let a = 1 in let b = 2 in a + b
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let a = *` | `1` | first let |
| `let b = *` | `2` | second let |
| `let _ = *` | `1`, `2` | wildcard: matches BOTH lets |
| `a _... in *` | `let b = 2 in a + b` | body of a |
| `b _... in *` | `a + b` | body of b |

---

## 2. Type Annotations and Type Definitions

### Annotated let

```
let x : Int = 42 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let x = *` | `42` | definition (annotation is transparent) |
| `let x : *` | `Int` | **FocusTyp** — the type annotation |
| `let x : _ = *` | `42` | skip annotation, focus on def |

### Type alias

```
type T = Int in let x : T = 42 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `type T = *` | `Int` | **FocusTyp** — the type definition |
| `type T _... in *` | `let x = 42 in x` | body after type alias |
| `type T` | `type T = Int in ...` | whole type alias expression |

---

## 3. Functions

### Program

```
let f = fun x -> x + 1 in f 5
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let f = *` | `fun x -> x + 1` | f's definition |
| `f/ \... fun _ -> *` | `x + 1` | enter f, descend, match fun body |
| `f/ \... fun x -> *` | `x + 1` | same, naming the parameter |
| `f/ \... fun _... -> *` | `x + 1` | ellipsis skips multi-param patterns |

---

## 4. If/Then/Else

### Program

```
if true then 1 else 0
```

| Selector | Result | Notes |
|----------|--------|-------|
| `if *` | `true` | scrutinee |
| `if _ then *` | `1` | then branch |
| `if _... else *` | `0` | else branch |

### Nested in a function

```
let f = fun x -> if x > 0 then x else 0 in f 5
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let f = \... if *` | `x > 0` | descend into f's def, find if condition |
| `let f = \... if _ then *` | `x` | then branch |
| `let f = \... if _... else *` | `0` | else branch |

---

## 5. Case Expressions

### Program

```
case msg | Increment => count + 1 | Decrement => count - 1 end
```

| Selector | Result | Notes |
|----------|--------|-------|
| `case *` | `msg` | scrutinee |
| `\| Increment => *` | `count + 1` | named arm body |
| `\| Decrement => *` | `count - 1` | named arm body |
| `\| _ => *` | `count + 1`, `count - 1` | wildcard: ALL arm bodies |
| `case _... \| Decrement => *` | `count - 1` | ellipsis skips to named arm |

Note: `|` is written as `\|` in shell contexts to avoid pipe interpretation,
but in the selector language itself it's just `|`.

---

## 6. Lists and Tuples

### List

```
let xs = [1, 2, 3] in xs
```

| Selector | Result | Notes |
|----------|--------|-------|
| `xs/ \... [ *` | `1` | first element |
| `xs/ \... [ _ *` | `2` | second element |
| `xs/ \... [ _... *` | `3` | last element |

### Tuple

```
let t = (1, 2, 3) in t
```

| Selector | Result | Notes |
|----------|--------|-------|
| `t/ \... ( *` | `1` | first element |
| `t/ \... ( _ *` | `2` | second element |
| `t/ \... ( _ _ *` | `3` | third element |
| `t/ \... ( _... *` | `3` | last element (ellipsis) |

---

## 7. Test Expressions

### Program

```
let x = 1 in test x == 1 end; x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... test *` | `x == 1` | test body |
| `\... test _... end` | `test x == 1 end` | whole test expression |

---

## 8. Binder Chains — Module Navigation

Chains are **sugar** for navigating through named bindings.

### Simple chain

```
let m = { let x = 42; let y = 99 } in m.x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `m/x = *` | `42` | enter m, focus on x's def |
| `m/y = *` | `99` | enter m, focus on y's def |
| `m/x` | `42` | bare name in module context → def |
| `m/x/` | `42` | trailing slash → also def (same in this case) |

### Chain as sugar

The chain `A/B/C` is sugar for navigating through definitions:

```
A/B/C   ≈  let A = \... let B = \... * let C
A/B/C/  ≈  let A = \... let B = \... let C = *
```

- No trailing slash: last name is a **MatchName** → whole binding
- Trailing slash: last name is an **EnterBinderDef** → enters the def

At top level, `MatchName(C)` returns the whole `let C = ... in ...`.
Inside `Module(items)`, there's no standalone let-expression node,
so it falls back to the definition.

### Nested chain

```
let a = { let x = 1; let b = { let y = 42 } } in a.b.y
```

| Selector | Result | Notes |
|----------|--------|-------|
| `a/b/y = *` | `42` | three levels deep |
| `a/x = *` | `1` | sibling member |
| `a/b/ \... let y = *` | `42` | enter a, enter b, descend for let y |

### Spaced chains

```
A/ B/ C/  is equivalent to  A/B/C/
A/ B/ C   is equivalent to  A/B/C
```

Spaces between chain segments are allowed. Each `name/` token
elaborates to `EnterBinderDef(name)` independently.

---

## 9. Module Expressions

### ModuleExp (top-level)

```
module M = { let x = 1 } in M.x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `module M = *` | `{ let x = 1 }` | module def |
| `module M _... in *` | `M.x` | body after module |
| `module M` | `module M = { let x = 1 } in M.x` | whole module expression |
| `module _ = *` | `{ let x = 1 }` | wildcard module name |
| `M/x = *` | `1` | chain into module member |

### Module-internal keywords

Inside a module body, `module` and `type` keywords match items:

```
module A = { let z = 0; module B = { let x = 42 }; type T = Int } in A.B.x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `A/ \... module B = *` | `{ let x = 42 }` | ModuleMod inside Module items |
| `A/ \... type T = *` | `Int` | ModType inside Module items |
| `A/ \... module B` | `module B = { let x = 42 }` | **FocusMod** — the whole Mod.t item |
| `A/ \... type T` | `type T = Int` | **FocusMod** — the whole Mod.t item |
| `A/B/x = *` | `42` | chain through nested module |
| `\... let z = *` | `0` | descend finds ModLet items |
| `\... let x = *` | `42` | descend finds ModLet inside nested module |

### FocusMod: Pointing at Module Items

When a selector points at a `ModLet`, `ModuleMod`, or `ModType` inside
a `Module(items)`, the result is a **FocusMod(Mod.t)** — a module item
term, not an expression. This lets you point at `let x = 42` or
`type T = Int` as standalone items, which is important for edit
operations like replacing a let with a type definition.

---

## 10. Descendant Search

`\...` searches the entire subtree rooted at the current position.

### Simple descent

```
let a = (let b = 42 in b) in a
```

| Selector | Result | Notes |
|----------|--------|-------|
| `let b = *` | ERROR | b is not at the top level |
| `\... let b = *` | `42` | descend finds b inside a's def |
| `a = \... let b = *` | `42` | enter a, then descend |

### Multiple matches

```
let f = fun x -> case x | A => 1 | B => 2 end in
let g = fun y -> case y | C => 3 | D => 4 end in f(g(0))
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... \| _ => *` | `1`, `2`, `3`, `4` | all arm bodies everywhere |
| `f/ \... \| _ => *` | `1`, `2` | only inside f |
| `g/ \... \| _ => *` | `3`, `4` | only inside g |
| `\... fun _ -> *` | two results | all function bodies |

### Double descent is idempotent

`\... \...` collapses to `\...` during elaboration.

---

## 11. Shadowed Names and Indexing

When multiple bindings share the same name, use `#N` (0-based) to
disambiguate:

### Program

```
let x = 1 in let x = 2 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `x#0 = *` | `1` | first binding |
| `x#1 = *` | `2` | second binding |
| `x#0 _... in *` | `let x = 2 in x` | body of first x |
| `x#1 _... in *` | `x` | body of second x |
| `let x#1 = *` | `2` | with let keyword |
| `x#5 = *` | ERROR: "2 binding(s) named 'x'" | out-of-range diagnostic |

### Module and type indexing

Indexing works for `module` and `type` keywords too:

```
module M = { let x = 1 } in module M = { let y = 2 } in M.y
```

| Selector | Result | Notes |
|----------|--------|-------|
| `module M#0 = *` | `{ let x = 1 }` | first module named M |
| `module M#1 = *` | `{ let y = 2 }` | second module named M |

```
type T = Int in type T = Bool in 42
```

| Selector | Result | Notes |
|----------|--------|-------|
| `type T#0 = *` | `Int` | first type named T |
| `type T#1 = *` | `Bool` | second type named T |

---

## 12. The Implicit Star Rule

If no `*` appears in the selector, one is appended at the end. This means
selectors that "end" at a binding naturally focus on it:

| Selector | With implicit star | Result |
|----------|-------------------|--------|
| `let x` | `let x *` | whole let expression |
| `let x =` | `let x = *` | x's definition |
| `a/b/` | `a/b/ *` | b's definition |
| `a/b` | `a/b *` | whole binding of b |
| `type T =` | `type T = *` | type definition |
| `\... fun _ ->` | `\... fun _ -> *` | function body |

The implicit star does **not** apply when a `*` is already present:

```
* let x = ...        ← * focuses on whole let, = and ... continue
let f = \... * fun   ← * focuses on the fun expression
```

---

## 13. The * Prefix: Focus-Before-Keyword

Placing `*` before a keyword focuses on the **whole matched form**
rather than a subpart:

### Program

```
let x = 42 in x + 1
```

| Selector | Result | Notes |
|----------|--------|-------|
| `* let x` | `let x = 42 in x + 1` | focus on whole let |
| `let x = *` | `42` | focus on def |

### With descent

```
let x = (let y = 99 in y) in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `\... * let y` | `let y = 99 in y` | descend, focus on whole inner let |
| `\... let y = *` | `99` | descend, focus on def |

---

## 14. Composition Patterns

These examples show how operators compose for realistic programs.

### MVU Application

```
module App = {
  let init = 0;
  let update = fun msg -> case msg | Inc => msg + 1 | Dec => msg - 1 | Reset => 0 end;
  let view = fun model -> let label = model + 1 in label
} in
let result = App.update(App.init) in result
```

| Selector | Result | Notes |
|----------|--------|-------|
| `App/init = *` | `0` | chain into module member |
| `App/update \... case *` | `msg` | chain + descend + case scrutinee |
| `App/update \... \| Inc => *` | `msg + 1` | chain + descend + named arm |
| `App/update \... \| _ => *` | 3 matches | chain + descend + all arms |
| `App/view \... let label = *` | `model + 1` | chain + descend + nested let |
| `module App = *` | `{ let init = 0; ... }` | whole module def |
| `App _... in *` | `let result = ...` | body after module |
| `\... * let result` | `let result = ... in result` | descend + focus whole let |
| `\... fun _ -> *` | 2+ matches | all function bodies |

### Chain + Descend + Spine

```
module M = {
  let f = fun x -> if x > 0 then x + 1 else x - 1
} in M.f(5)
```

| Selector | Result | Notes |
|----------|--------|-------|
| `M/f \... if *` | `x > 0` | chain + descend + if condition |
| `M/f \... if _ then *` | `x + 1` | chain + descend + then branch |
| `M/f \... if _... else *` | `x - 1` | chain + descend + else branch |
| `M/f \... fun _ -> *` | `if x > 0 then ...` | chain + descend + fun body |

---

## 15. Error Diagnostics

When a selector doesn't match, the error message includes:
- How far matching got before failing
- Which step failed
- Available names at the failure point
- "Did you mean?" suggestions for close misspellings

### Examples

```
let foo = 1 in let bar = 2 in foo + bar
```

| Selector | Error |
|----------|-------|
| `let baz = *` | Matched up to: let / Failed at: baz / Did you mean: bar / Available names: foo, bar |
| `if *` | Failed at first step: if |
| `let zzzzz = *` | Failed at: zzzzz / Available names: foo, bar (no suggestion — too different) |

```
module M = { let x = 1; let y = 2 } in M.x
```

| Selector | Error |
|----------|-------|
| `M/z = *` | Failed at: z / Available names: x, y |

---

## 16. Child Index — Structural Addressing

`#N` descends into the Nth structural child of the current node.
Children are numbered left-to-right as they appear in source syntax.
Metadata fields (env, direction, provenance) are skipped.

### Child ordering

| Form | Children |
|------|----------|
| `Let(pat, def, body)` | `#0`=pat, `#1`=def, `#2`=body |
| `Fun(pat, body, _, _)` | `#0`=pat, `#1`=body |
| `If(cond, then, else)` | `#0`=cond, `#1`=then, `#2`=else |
| `BinOp(op, e1, e2)` | `#0`=e1, `#1`=e2 |
| `Asc(e, typ)` | `#0`=e, `#1`=typ |
| `Tuple(items)` / `ListLit(items)` | `#0`=item0, `#1`=item1, ... |
| `Match(scrut, rules)` | `#0`=scrut, `#1`=rule0, `#2`=rule1, ... |
| `Module(items)` | `#0`=item0, `#1`=item1, ... |
| `Parens(e)` | `#0`=e |
| `ModLet(pat, def)` | `#0`=pat, `#1`=def |

Match rules are virtual pairs: `#1` enters rule 0, then `#0`=pat, `#1`=body.

### Examples

```
let x = (1 + 2) + 3 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `#0` | `x` | pat (cross-sort: FocusPat) |
| `#1` | `(1 + 2) + 3` | def |
| `#2` | `x` | body |
| `x = #0` | `1 + 2` | left operand (enters Parens) |
| `x = #1` | `3` | right operand |
| `x = #0 #0 #0` | `1` | deep: left of inner + |
| `x = #0 #0 #1` | `2` | deep: right of inner + |

### Cross-sort navigation

`#N` can cross sort boundaries. A Pat or Typ child is entered seamlessly:

```
let x : Int = 42 in x
```

| Selector | Result | Notes |
|----------|--------|-------|
| `#0` | `x : Int` | pat (FocusPat, includes Asc) |
| `#0 #0` | `x` | inner pat before annotation (FocusPat) |
| `#0 #1` | `Int` | type annotation (FocusTyp) |

### Match rule pairs

```
case x | A => 1 | B => 2 end
```

| Selector | Result | Notes |
|----------|--------|-------|
| `#0` | `x` | scrutinee |
| `#1 #0` | `A` | rule 0 pat (FocusPat) |
| `#1 #1` | `1` | rule 0 body |
| `#2 #0` | `B` | rule 1 pat (FocusPat) |
| `#2 #1` | `2` | rule 1 body |

### Module items

```
let m = { let x = 42; let y = 99 } in m
```

| Selector | Result | Notes |
|----------|--------|-------|
| `m = #0` | `let x = 42` | first item (FocusMod) |
| `m = #1` | `let y = 99` | second item (FocusMod) |
| `m = #0 #0` | `x` | first item's pat (FocusPat) |
| `m = #0 #1` | `42` | first item's def |

### Mixing named + index

Named selectors navigate to a binding, then `#N` drills into anonymous
substructure:

```
let x = 10 + 20 in let y = 30 in x + y
```

| Selector | Result | Notes |
|----------|--------|-------|
| `x = #0` | `10` | left operand of x's def |
| `x = #1` | `20` | right operand of x's def |

---

## 17. Canonical Selectors — Addressing Any Node

Two functions generate selectors that uniquely identify any node in the AST,
given its ID:

### Numeric canonical (`canonical_numeric`)

Pure `#N` path from root. Universal, stable, terse. Every step is a
`ChildIndex`, terminated by `MatchFocus`.

```
let x = (1 + 2) + 3 in x

Node "1":     #1 #0 #0 #0 *
Node "2":     #1 #0 #0 #1 *
Node "1 + 2": #1 #0 #0 *
Node "3":     #1 #1 *
Node "x":     #2 *
```

### Named canonical (`canonical_named`)

Prefers names and keywords over indices. Falls back to `#N` only for
anonymous subexpressions.

```
let x = (1 + 2) + 3 in x

Node "1":      x = #0 #0 *     (name + index inside BinOp)
Node "2":      x = #0 #1 *
Node "1 + 2":  x = #0 *        (name + index inside Parens)
Node "42" in "let x = 42":  x = *   (just the name!)
```

### If / Match / Fun examples

```
if true then 1 else 0

Numeric "true":   #0 *
Named "true":     if *
Numeric "1":      #1 *
Named "1":        if _ then *
Numeric "0":      #2 *
Named "0":        if _... else *
```

```
case x | A => 1 | B => 2 end

Numeric "A":  #1 #0 *
Named "A":    | _... A *       (constructor name)
Numeric "2":  #2 #1 *
Named "2":    | _... B => *
```

### Shadowed names

Named canonical uses `x#N` when names are shadowed:

```
let x = 1 in let x = 2 in x

Named "1":  x#0 = *
Named "2":  x#1 = *
```

### Deparse (`deparse`)

Converts any `sem_selector` back to surface syntax:

```reason
deparse([ChildIndex(1), ChildIndex(0), MatchFocus]) → "#1 #0 *"
deparse([MatchKeyword("let"), MatchName("x"), MatchDelimiter("="), MatchFocus]) → "let x = *"
deparse([DescendInto, MatchKeyword("let"), MatchName("y"), MatchDelimiter("="), MatchFocus]) → "\\... let y = *"
deparse([MatchKeyword("let"), MatchNameIndex("x", 1), MatchDelimiter("="), MatchFocus]) → "let x#1 = *"
```

### Roundtrip guarantee

For any node in a well-formed program:
1. `canonical_numeric(id, root)` produces a selector
2. Resolving that selector returns the same node ID
3. Same guarantee holds for `canonical_named`

---

## 18. Composition: The MVU Application

A realistic program that exercises most selector features:

```
module App = {
  let init = 0;
  let update = fun msg -> case msg | Inc => msg + 1 | Dec => msg - 1 | Reset => 0 end;
  let view = fun model -> let label = model + 1 in label
} in
let result = App.update(App.init) in result
```

### Named selectors

| Selector | Result | Notes |
|----------|--------|-------|
| `App/init = *` | `0` | chain into module member |
| `App/update \... case *` | `msg` | chain + descend + case scrutinee |
| `App/update \... \| Inc => *` | `msg + 1` | chain + descend + named arm |
| `App/update \... \| Dec => *` | `msg - 1` | named arm |
| `App/update \... \| Reset => *` | `0` | named arm |
| `App/update \... \| _ => *` | 3 matches | all arms |
| `App/view \... let label = *` | `model + 1` | chain + descend + nested let |
| `module App = *` | `{ let init = 0; ... }` | whole module def |
| `App _... in *` | `let result = ...` | body after module |
| `\... * let result` | `let result = ... in result` | descend + focus whole let |
| `result = *` | `App.update(App.init)` | top-level let |
| `\... fun _ -> *` | 2+ matches | all function bodies |

### Numeric child-index selectors

| Selector | Result | Notes |
|----------|--------|-------|
| `App = #0` | `let init = 0` | first module item (FocusMod) |
| `App = #1` | `let update = fun msg -> ...` | second module item |
| `App = #2` | `let view = fun model -> ...` | third module item |
| `App = #0 #1` | `0` | init's def (ModLet child #1) |
| `App = #1 #1 #1 #0` | `msg` | update's fun body → case scrutinee |

---

## 19. Selector Insert Actions — Semantics & Edge Cases

`SelectorInsertBefore(selector, code)` and `SelectorInsertAfter(selector, code)`
insert new code relative to the node matched by `selector`. The selector resolves
to a target node, and the insert logic checks what *kind* of structural position
that node occupies to determine how to insert.

### The cascade (checked in order)

1. **Case arm**: If the target is a case arm body, insert a new rule.
   The `code` is a rule like `"D => 4"` (optional leading `|`).

2. **List element**: If the target is inside a `ListLit`, insert a new element.
   The `code` is an expression like `"42"`.

3. **Tuple element**: If the target is inside a `Tuple`, insert a new element.
   The `code` is an expression like `"42"`.

4. **Module item**: If the target is (or is inside) a `Module(items)` item,
   insert a new item. The `code` is a binding like `"let y = 2"`.

5. **Binding (fallback)**: Otherwise, treat the target as a Let/TyAlias chain
   node. The `code` is a binding like `"let y = 2"`.

### Examples

#### Let bindings

```
Program: let x = 1 in x + 1

SelectorInsertAfter("* let x", "let y = 2")
→ let x = 1 in let y = 2 in x + 1
  (new let is spliced into x's body)

SelectorInsertBefore("* let x", "let y = 2")
→ let y = 2 in let x = 1 in x + 1
  (new let wraps the target)
```

#### Module items

```
Program: module M = { let x = 1 } in M.x

SelectorInsertAfter("M/x = *", "let y = 2")
→ module M = { let x = 1; let y = 2 } in M.x

SelectorInsertBefore("M/x = *", "let y = 0")
→ module M = { let y = 0; let x = 1 } in M.x
```

#### Case arms

```
Program: let f = fun x -> case x | A => 1 | B => 2 end in f

(via path system, not yet wired through SelectorInsert directly)
Insert(After, "f/|B", "C => 3")
→ let f = fun x -> case x | A => 1 | B => 2 | C => 3 end in f

Insert(Before, "f/|A", "Z => 0")
→ let f = fun x -> case x | Z => 0 | A => 1 | B => 2 end in f
```

The code string for case arms is `"Pat => Body"` with an optional leading `|`.

### Edge cases and open questions

**What happens when the target isn't in a "sequence-like" position?**

The binding fallback (step 5) will try to wrap *any* expression in a Let,
even if the selector pointed at, say, an operand of `+`. The result would
be structurally valid but semantically surprising:

```
Program: let x = 1 + 2 in x
SelectorInsertAfter("x = #0", "let y = 99")
→ wraps the "1" in: let y = 99 in 1 (inside the BinOp)
```

This is permissive — an agent never gets blocked — but may produce
nonsensical results. Options:

1. **Current (permissive)**: Always succeed. Agent is responsible for
   using sensible selectors.
2. **Strict**: Refuse to insert unless the target is in a recognized
   "sequence-like" position (case arm, list, tuple, module item, or
   let/type chain). Would require an explicit check before the fallback.
3. **Diagnostic**: Succeed but return a warning when the target position
   is unusual.

Currently using option 1. May want to revisit.

**SelectorInsert for case arms via selector syntax:**

The current test suite uses the path system (`Insert(After, "f/|B", "C => 3")`)
for case arm insertion. Wiring `SelectorInsertAfter("| B => *", "C => 3")`
through the selector system would require the `is_case_arm` check to work
with selector-resolved IDs. This should already work since both resolve to
the same `focused_id`, but needs testing.

---

## Future Directions

### Wildcard keyword `_` in keyword position

Currently chains are the only way to be keyword-agnostic. The expansion:

```
A/B/C  ≈  _ A = \... _ B = \... * _ C
```

where `_` in keyword position matches `let`, `module`, `type`, etc.

### BinOp spine walkers

Named canonicals for binary operators (e.g., `x = * + _` for the left
operand of `+`). Requires BinOp-aware spine walkers.

### Inline `*` in BinOp context

Currently `*` as focus is always terminal. Inline use like `x = * + _`
would capture the left operand — disambiguated from multiplication by
syntactic context (bare `*` vs `* + _`).

### Multi-variable selectors

Multiple focus variables (`*a`, `*b`, `*c`) for extracting several
subexpressions in one pass.

### Indexing for non-let binders

Currently `x#N` only works for let bindings. Could extend to
`fun x#1 -> *` (nth fun with parameter x) or `module M#1 = *`.
