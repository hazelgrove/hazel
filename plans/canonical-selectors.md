# Canonical Unique Selectors — Design

## Goal

Given any node ID in the AST, produce a selector that uniquely resolves
back to that node. Two modes:

1. **Numeric**: Pure child-index path. Universal, stable, terse.
2. **Named**: Prefer names/keywords over indices. More readable.

Both should address *every* node — expressions, patterns, types, module items.

## Layer 1: Numeric Canonical Paths

### Surface syntax

`#N` tokens, space-separated:

```
#1 #0 #0    →  child 1, child 0, child 0
#0          →  first child
```

A standalone `#0` is a single child descent. Multiple `#N` tokens chain.

### Semantic step

```reason
| ChildIndex(int)   /* descend into nth child */
```

### Child ordering convention

Children are numbered left-to-right as they appear in source syntax.
Only *structural* children count — metadata fields (env, provenance,
direction, etc.) are skipped.

| Form | Children |
|------|----------|
| `Let(pat, def, body)` | `#0`=pat, `#1`=def, `#2`=body |
| `Fun(pat, body, _, _)` | `#0`=pat, `#1`=body |
| `If(cond, then, else)` | `#0`=cond, `#1`=then, `#2`=else |
| `BinOp(op, e1, e2)` | `#0`=e1, `#1`=e2 |
| `UnOp(op, e)` | `#0`=e |
| `Ap(_, fn, arg)` | `#0`=fn, `#1`=arg |
| `Match(scrut, rules)` | `#0`=scrut, `#1`=rule0_pat, `#2`=rule0_body, ... |
| `Tuple(items)` | `#0`=item0, `#1`=item1, ... |
| `ListLit(items)` | `#0`=item0, `#1`=item1, ... |
| `Seq(e1, e2)` | `#0`=e1, `#1`=e2 |
| `Parens(e)` | `#0`=e |
| `Asc(e, typ)` | `#0`=e, `#1`=typ |
| `Test(body)` | `#0`=body |
| `Module(items)` | `#0`=item0, `#1`=item1, ... |
| `TyAlias(tpat, typ, body)` | `#0`=tpat, `#1`=typ, `#2`=body |
| `ModuleExp(mpat, def, body)` | `#0`=mpat, `#1`=def, `#2`=body |
| `Cons(hd, tl)` | `#0`=hd, `#1`=tl |
| `Dot(base, field)` | `#0`=base, `#1`=field |
| `TupLabel(lbl, e)` | `#0`=lbl, `#1`=e |

For Pat and Typ, same convention (left-to-right structural children).

For Mod items:
| `ModLet(pat, def)` | `#0`=pat, `#1`=def |
| `ModType(tpat, typ)` | `#0`=tpat, `#1`=typ |
| `ModuleMod(mpat, def)` | `#0`=mpat, `#1`=def |
| `ModExp(e)` | `#0`=e |

### Resolution

`ChildIndex(n)` in walk():
```reason
| [ChildIndex(n), ...rest] =>
  switch (nth_child(n, current)) {
  | Some(FocusExp(e)) => walk(rest, e)
  | Some(FocusPat(p)) => walk_pat(rest, p)  /* new */
  | Some(FocusTyp(t)) => walk_typ(rest, t)  /* new */
  | Some(FocusMod(m)) => walk_mod(rest, m)  /* new */
  | None => []
  }
```

This means we need `walk_pat`, `walk_typ`, `walk_mod` — minimal
versions that handle `MatchFocus` and `ChildIndex` at least.

### Generation

```reason
let canonical_numeric = (target_id: Id.t, root: Exp.t): option(sem_selector)
```

DFS from root. At each node, try each child. If the child's ID matches
target_id, return `[ChildIndex(i), MatchFocus]`. If the target is deeper,
recurse into the child and prepend `ChildIndex(i)`.

Need to search across sorts: a child might be Pat or Typ, so the
DFS needs `find_in_exp`, `find_in_pat`, `find_in_typ`, `find_in_mod`.

### Cross-sort focus

A numeric path can cross sort boundaries:

```
let x : Int = 42 in x

#0          → the pattern (Pat)
#0 #0      → the inner pattern before Asc (Pat)
#0 #1      → the type annotation Int (Typ)
#1          → the def 42 (Exp)
#2          → the body x (Exp)
```

This requires `nth_child` to return `focus_target` (already a variant
over Exp/Pat/Typ/Mod) and the walker to dispatch into sort-specific
walkers.

## Layer 2: Named Canonical Paths

### Strategy

Same DFS, but at each node, try named addressing first:
- If the current node is `Let(pat, def, body)` and we're entering `def`,
  emit `MatchName(pat_name) MatchDelimiter("=")` instead of `#1`.
- If named addressing is ambiguous (shadowed), use `MatchNameIndex`.
- If no name applies (anonymous subexpression), fall back to `ChildIndex`.

### Example

```
let x = (1 + 2) + 3 in x

Named canonical for `1`:        x = #0 #0
Named canonical for `1 + 2`:    x = #0
Named canonical for `3`:        x = #1
Named canonical for `x` (body): x _... in *
Named canonical for `x` (let):  let x
Named canonical for `Int`:      x : *     (type annotation)
```

### Algorithm sketch

```reason
let canonical_named = (target_id: Id.t, root: Exp.t): option(sem_selector)
```

At each node:
1. If node ID = target_id, emit `[MatchFocus]`
2. If node is a named binder and target is in def/body, emit name-based steps
3. If node is a keyword form and target is in a named slot, emit keyword steps
4. Otherwise, emit `ChildIndex(n)` for the child containing the target

### Deparse (sem_selector → string)

Inverse of elaborate. Render semantic steps back to surface tokens:

```reason
let deparse = (steps: sem_selector): string
```

Rules:
- `MatchFocus` → `*`
- `MatchSlot` → `_`
- `MatchEllipsis` → `_...`
- `MatchKeyword(kw)` → `kw`
- `MatchDelimiter(d)` → `d`
- `MatchName(n)` → `n`
- `MatchNameIndex(n, i)` → `n#i`
- `ChildIndex(n)` → `#n`
- `EnterBinderDef(n)` → (part of chain, needs context)
- `DescendInto` → `\...`

Chains: consecutive `EnterBinderDef` steps can be collapsed:
`EnterBinderDef("a") EnterBinderDef("b") MatchName("c")` → `a/b/c`

## Implementation Plan

### Phase 1: ChildIndex resolution (~40-50 lines)
1. Add `ChildIndex(int)` to `sem_step`, `Index(int)` to `token`
2. Add tokenizer: `#N` pattern → `Index(N)`
3. Add elaboration: `Index(N)` → `ChildIndex(N)`
4. Add `nth_child(n: int, e: Exp.t): option(focus_target)` — big switch
5. Add `ChildIndex` case to `walk()` — dispatch into child
6. For cross-sort: minimal `walk_pat`, `walk_typ`, `walk_mod` (just
   handle `MatchFocus` and `ChildIndex`)
7. Tests: `#0`, `#1`, `#0 #0`, cross-sort `#0 #1` for type annotation

### Phase 2: Numeric canonical generation (~60-80 lines)
1. `find_in_exp(target_id, e)` → `option(list(int))`
2. `find_in_pat(target_id, p)` → `option(list(int))`
3. `find_in_typ(target_id, t)` → `option(list(int))`
4. `find_in_mod(target_id, m)` → `option(list(int))`
5. `canonical_numeric(target_id, root)` → `option(sem_selector)`
   converts index list to `ChildIndex` steps + `MatchFocus`
6. Tests: roundtrip — for each node in a program, generate canonical
   path, resolve it, verify we get the same node ID back

### Phase 3: Named canonical generation (~80-100 lines)
1. `canonical_named(target_id, root)` → `option(sem_selector)`
2. At each level, prefer name steps over index steps
3. Tests: compare named vs numeric for same targets, verify both resolve

### Phase 4: Deparse (~20-30 lines)
1. `deparse(sem_selector)` → `string`
2. Chain collapse: consecutive EnterBinderDef → chain syntax
3. Implicit star elision: omit trailing `*` if it would be auto-appended
4. Tests: roundtrip — deparse then parse, verify same sem_selector

## Resolved Questions

1. **Parens transparency**: RESOLVED — `#0` on `Parens(e)` returns `e`
   (enters parens). Parens has exactly one child. Named canonical skips
   Parens transparently when building paths.

2. **Match rule grouping**: RESOLVED — pairs, not flattening.
   `#1 #0`=rule0_pat, `#1 #1`=rule0_body. Match rules are virtual
   paired nodes at indices 1, 2, 3, ... (index 0 is scrutinee).

3. **BinOp operator matching**: DEFERRED — named canonical uses
   `#0`/`#1` for BinOp operands. BinOp spine walkers not yet built.

4. **Star placement**: DEFERRED — canonical generation uses terminal `*`
   exclusively. Inline `*` (e.g., `x = * + _`) is a separate ergonomic
   feature for human-authored selectors.

## Implementation Status

- **Phase 1**: ChildIndex resolution — DONE (nth_child_exp/pat/typ/mod, cross-sort walkers)
- **Phase 2**: Numeric canonical generation — DONE (find_in_exp/pat/typ/mod DFS, canonical_numeric)
- **Phase 3**: Named canonical generation — DONE (named_in_exp with keyword addressing)
- **Phase 4**: Deparse — DONE (sem_selector → surface string)
- **Tests**: 49 canonical tests (32 numeric, 5 deparse, 12 named)
