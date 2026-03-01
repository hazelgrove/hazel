# Coding Agent Actions: Structural Editing & Addressing Plan

**Branch**: `coding-agent-actions` (off `coding-agent`, merged with `dev`)
**Worktree**: `/Users/andrewblinn/.claude-worktrees/hazel/coding-agent-actions/`

## Status

- [x] Fetch `coding-agent` and `dev`
- [x] Create worktree + branch `coding-agent-actions`
- [x] Merge `dev` into branch (resolves cleanly after two minor conflicts)
- [x] Fix `Module`/`ModuleExp` exhaustiveness in `HighLevelNodeMap.re`
- [x] Build passes, all 11 AgentTools tests pass
- [ ] Investigate & critique current structural action system
- [ ] Design addressing/path improvements
- [ ] Add module edit actions
- [ ] Expand test suite
- [ ] Design read/query improvements

---

## 1. Current System Overview

### Architecture

```
Action.Structural.t  ──>  Perform.go  ──>  CompositionGo.go  ──>  edit_dispatch
                                                                        │
                          HighLevelNodeMap.build ◄──────────────────────┘
                                │
                          path_to_node(path) ──> get_inner_term_id ──> Select.term ──> overwrite/destruct/insert
```

### Current Action Set (`Action.re:86-116`)

```
Structural.t =
  | Update(target, path, code)
  | Delete(target, path)
  | Insert(insert_target, path, code)

target = Definition | Body | Pattern | BindingClause
insert_target = After | Before
path = string  (* "/" delimited name path *)
```

### Current Path System (`HighLevelNodeMap.re`)

- **Format**: Slash-delimited names, e.g. `"a"`, `"a/inner"`
- **Resolution**: Linear scan of all nodes comparing name paths (O(n))
- **Naming**: Extracted from patterns via `Namer.mk_name` (Var, Constructor, Tuple, etc.)
- **Fuzzy matching**: Levenshtein distance for error messages
- **Scope**: Only `Let` and `TyAlias` expressions are addressable

### Current Read/Query Actions

- `expand(paths)` / `collapse(paths)` — toggle definition visibility
- `view_entire_definition` — show full def without folds
- `view_context` — show typing context at cursor
- `show_references` — show all references to current node (defined but unimplemented)
- `ShowUseSites` / `ShowReferences` — defined in `CompositionActions.re`, marked TODO

---

## 2. Critique: What Can't Be Addressed

### A. Bare expressions (non-binding program lines)

The current system assumes every interesting thing is a `Let` or `TyAlias`. But programs can have:
```
let x = 1 in
test 5 == x + 4 end;    (* <-- bare expression, no binding name *)
let y = x + 1 in
y
```
The `test` expression is structurally the *body* of `x`, but it's awkward to access. You'd have to `Update(Body, "x", ...)` and include the entire rest of the program. There's no way to target just the test expression.

The final expression `y` (after all lets) is similarly unreachable except as the body of the last `let`.

### B. Expressions inside definitions

Within a definition like `let f = fun x -> if x > 0 then x else 0 in ...`, there's no way to address the `if` branch, the condition, etc. You can only replace the entire definition.

### C. Module items

`Module([ModLet(...), ModType(...), ModExp(...)])` has a flat list of items, none of which are in the node map. The `Module` form returns `[]` children in `child_expressions_of_exp`.

### D. Match arms / case branches

No way to address individual branches of a `match`/`case`.

### E. Function parameters and type annotations

Can't target just the type annotation on a let, or individual function parameters.

### F. Pattern internals

`Update(Pattern, ...)` replaces the whole pattern. Can't target e.g. just the type annotation part of `x : Int`.

---

## 3. Proposed Path System Improvements

### 3.1 Path as a structured type (not raw string)

Replace `type path = string` with a structured type that supports multiple addressing modes:

```reason
type segment =
  | Name(string)           /* existing: "x", "inner" */
  | Index(int)             /* positional: 0, 1, 2 */
  | Form(string, int)      /* form + index: ("let", 0) for first let */
  | ModItem(int)           /* module item by position */
  | ModItemName(string)    /* module item by name */
  | Branch(int)            /* match/case branch index */
  | FinalExpr              /* the trailing expression after all bindings */

type path = list(segment)

/* Legacy string paths still work via parsing */
let of_string: string => path
```

**Key insight**: A path is a list of "steps" that walk into the term structure. Each step disambiguates using the most natural scheme for that level:
- Names for let/type bindings (the common case, backward compatible)
- Indexes for positional things (match arms, module items, tuple elements)
- `Form` for selecting among same-level things by syntactic form
- `FinalExpr` as a special case for "the body after all the bindings"

### 3.2 Path resolution strategy

```
"x"           → [Name("x")]                    (* backward compat *)
"x/inner"     → [Name("x"), Name("inner")]     (* backward compat *)
"#0"          → [Index(0)]                      (* top-level item 0 *)
"x/#0"        → [Name("x"), Index(0)]           (* first child of x *)
"$"           → [FinalExpr]                     (* trailing expression *)
"x/$"         → [Name("x"), FinalExpr]          (* body after x's inner lets *)
"{M}/x"       → [ModItemName("M"), Name("x")]   (* item x in module M *)
"?0"          → [Branch(0)]                     (* match branch 0 *)
```

The `/`-delimited string format is preserved but extended. A segment starting with `#` is an index, `$` is final-expr, `{...}` is a module name, `?` is a branch.

### 3.3 Minimal extension: just add `$` (FinalExpr) and `#n` (Index)

If we want to be conservative, just these two additions get us a lot:
- `$` lets us address the trailing expression after all bindings
- `#n` lets us address positional items (module items, match arms, expressions-as-statements)

This is probably the right first step.

---

## 4. Proposed Target Extensions

### 4.1 New targets for existing actions

```reason
type target =
  | Definition
  | Body
  | Pattern
  | BindingClause
  /* New: */
  | TypeAnnotation   /* just the type part of `x : Int` in a pattern */
  | Expression       /* a bare expression (non-binding) at a given path */
```

### 4.2 Expression-line actions

For the "test expression as statement" case, we need an action that targets a non-binding expression in a let chain. This is equivalent to targeting the body of the preceding let, but only the part before the next let.

**Approach**: Model this as `Update(Expression, path, code)` where the path uses an index to identify the expression among the sequential items at that scope level.

Actually, a simpler approach: these are always inside a `Seq(e1, e2)`. The expression is `e1` and `e2` is the continuation. So:
- `Seq` acts like an unnamed binding where `e1` is the "definition" (side-effecting expression) and `e2` is the "body"
- We can include `Seq` in the node map with synthetic names like `#0`, `#1` etc.

### 4.3 Module-specific actions

```reason
/* Module item operations */
type mod_target =
  | ModDefinition(int)    /* definition of item i */
  | ModPattern(int)       /* pattern of item i (for ModLet/ModuleMod) */
  | ModTypeAnnotation(int) /* type of item i (for ModType) */
  | ModItem(int)          /* entire item i */

type mod_action =
  | UpdateModItem(mod_target, path, code)
  | InsertModItem(int, path, code)    /* insert at position i */
  | DeleteModItem(int, path)          /* delete item i */
  | AppendModItem(path, code)         /* add item at end */
```

Or, more unified: extend the existing system so that once a path resolves to a `Module(items)`, the next path segment can address individual items, and then the existing `target` (Definition, Pattern, etc.) works on each item.

---

## 5. Module Edit Actions (Implementation Plan)

### 5.1 Extend HighLevelNodeMap to handle Module items

Currently `Module(_)` returns `[]` children. We should:

1. When encountering `Module(items)` in `build_children`, iterate the items and add nodes for each `ModLet`/`ModType`/`ModuleMod`
2. Give each item a name derived from its pattern (same as `Namer`)
3. `ModExp` items get index-based names (`#0`, `#1`, etc.)
4. These become children of the enclosing `ModuleExp` or the expression containing the `Module`

### 5.2 Module-aware dispatch in `edit_dispatch`

The existing `Utils.get_inner_term_id` only handles `Let` and `TyAlias`. Extend it to handle module items:

```reason
| Module(items) =>
  /* Path segment resolves to an item index */
  switch (inner_term) {
  | Pat => /* pattern of ModLet/ModuleMod at index */
  | Def => /* definition of ModLet/ModuleMod at index */
  | Body => /* not applicable — modules don't have a body per item */
  }
```

Actually, since modules expand to nested lets, we might be able to leverage the expansion. After elaboration, `Module([ModLet(x,1), ModLet(y,2)])` becomes `Let(x, 1, Let(y, 2, (x=x, y=y)))`. So the existing let-targeting machinery could work if we address the expanded form. But this loses the connection to the surface syntax.

**Better approach**: Work at the surface level. When the path resolves to a module-containing binding (e.g. `module M = {...}`), the next path segment names an item inside the module. The dispatch then:
1. Finds the module expression
2. Locates the item by name/index
3. Uses `Select.term` to select the relevant piece
4. Overwrites/inserts/deletes

### 5.3 What actions make sense for modules

| Action | Module equivalent |
|--------|------------------|
| `Update(Definition, "M/x", code)` | Change the definition of item `x` in module `M` |
| `Update(Pattern, "M/x", code)` | Rename item `x` in module `M` |
| `Insert(After, "M/x", code)` | Add new item after `x` in module `M` |
| `Delete(BindingClause, "M/x")` | Remove item `x` from module `M` |
| `Update(Expression, "M/#0", code)` | Change a bare expression in module |

**Key insight**: Modules are *more natural* for this system because they're genuinely flat. The path system maps cleanly: `"M/x"` means "item named x in module M". No awkward body-nesting semantics.

---

## 6. Test Suite Plan

### 6.1 New tests for existing actions (edge cases)

- [ ] Nested lets: `let a = let b = let c = 1 in c in b in a` — deep path `"a/b/c"`
- [ ] Type alias editing: `type T = Int in let x : T = 1 in x` — update type alias def
- [ ] Pattern with annotation: `let x : Int = 1 in x` — update just the pattern
- [ ] Tuple patterns: `let (a, b) = (1, 2) in a + b` — update/rename tuple bindings
- [ ] Duplicate names at different scopes: `let a = 1 in let b = let a = 2 in a in b`
- [ ] Empty body (hole): `let a = 1 in ?` — insert/delete on minimal program
- [ ] Deeply nested insert: multiple lets, insert at various positions
- [ ] Static error rejection: confirm bad edits are rejected
- [ ] Invalid path: confirm useful error message with suggestion
- [ ] Seq expressions (if supported): `let a = 1 in test a == 1 end; a`

### 6.2 Module action tests

- [ ] Build node map for module: `module M = { let x = 1; let y = 2 } in M`
- [ ] Update module item definition: change `x` from `1` to `42`
- [ ] Insert module item: add `let z = 3` after `y`
- [ ] Delete module item: remove `y`
- [ ] Rename module item: rename `x` to `a`
- [ ] Nested modules: `module M = { module N = { let x = 1 } } in M`
- [ ] Module with type aliases: `module M = { type T = Int; let x : T = 1 } in M`
- [ ] Module with bare expressions: `module M = { let x = 1; test x == 1 end } in M`
- [ ] ModuleExp node map: verify `module M = ... in body` creates proper nodes

### 6.3 Path resolution tests

- [ ] Name-based path resolution (existing)
- [ ] Index-based resolution (`#0`, `#1`)
- [ ] FinalExpr resolution (`$`)
- [ ] Mixed paths (`"a/#0"`, `"M/x"`)
- [ ] Error messages for bad paths

---

## 7. Read/Query Improvements (Design Sketch)

### Current state
- `expand`/`collapse` on name paths — works but coarse
- `view_entire_definition` — useful but limited
- `view_context` — typing context at cursor
- `show_references` — defined in JSON schema but unimplemented

### Desired state (CSS-selector-like queries)

```
query("*")                        → whole program, collapsed
query("x")                        → just binding x, expanded
query("x", "y")                   → union of x and y
query("x/*")                      → x and all descendants expanded
query("M/*")                      → all items in module M
query("@refs(x)")                 → all use sites of x
query("@type(Int)")               → all bindings with type Int
query("@errors")                  → all bindings with type errors
```

These would return a "view" — the program text with strategic folding, showing the queried parts expanded and everything else collapsed.

### Multi-cursor / multi-edit

For edits across multiple locations simultaneously:
```
UpdateAll("@refs(x)", new_name)   → rename all references
UpdateMatching(query, transform)  → structural find-and-replace
```

This is future work but the path/query system should be designed with it in mind.

---

## 8. Implementation Order

### Phase 1: Foundation (this session)
1. **Extend path type** — add `FinalExpr` and `Index` support to path parsing
2. **Module node map** — make `HighLevelNodeMap.build_children` handle `Module(items)`
3. **Module edit dispatch** — extend `edit_dispatch` and `get_inner_term_id` for module items
4. **Tests** — comprehensive test suite for existing + new actions

### Phase 2: Refinement
5. **Seq/expression-line support** — add bare expressions to node map
6. **TypeAnnotation target** — target just the type part of annotated patterns
7. **Read query implementation** — implement `show_references`, `show_use_sites`
8. **Path type formalization** — move from string to proper ADT

### Phase 3: Compositional queries
9. **Query language** — CSS-selector-like addressing
10. **Multi-cursor edits** — batch operations across multiple nodes
11. **Collapsed/expanded view composition** — union/intersection of query results

---

## 9. Open Questions

1. **Should paths be strings or structured types in the JSON API?**
   - Strings are simpler for the LLM to produce
   - But structured types are more precise and self-documenting
   - Proposal: keep string format for JSON API, parse into structured type internally

2. **How to handle name collisions in modules?**
   - Modules allow shadowing (last def wins)
   - Path `"M/x"` is ambiguous if there are two `let x = ...` items
   - Proposal: `"M/x"` targets the last (effective) one; `"M/#n"` for positional

3. **Should module items use the same `target` type?**
   - `Definition`/`Pattern`/`Body`/`BindingClause` map naturally to module items
   - But `Body` doesn't make sense for module items (they don't have a body)
   - Proposal: reuse the same type, make `Body` on a module item target "everything after this item in the module"

4. **How deep should sub-expression addressing go?**
   - Could eventually address any subexpression by structural path
   - But diminishing returns — at some point, just replace the whole definition
   - Proposal: for now, only go one level into modules. Future phases add deeper addressing.

5. **Expression lines in non-module context**
   - `test ... end; let x = ...` creates `Seq(test, Let(x, ...))`
   - Should the `Seq`'s first expression be a "node" in the map?
   - Proposal: yes, with synthetic name `#0`, `#1` based on position
