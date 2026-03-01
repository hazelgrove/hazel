# Coding Agent Actions: Structural Editing, Addressing & Semantic Queries

**Branch**: `coding-agent-actions` (off `coding-agent`, merged with `dev`)
**Worktree**: `/Users/andrewblinn/.claude-worktrees/hazel/coding-agent-actions/`

## Status

- [x] Fetch `coding-agent` and `dev`
- [x] Create worktree + branch `coding-agent-actions`
- [x] Merge `dev` into branch (two minor conflicts resolved)
- [x] Fix `Module`/`ModuleExp` exhaustiveness in `HighLevelNodeMap.re`
- [x] Build passes, all 11 AgentTools tests pass
- [x] Investigate & critique current structural action system
- [x] Investigate ChatLSP.re removal and functionality gaps
- [x] Design addressing/path improvements (this document)
- [x] Add module edit actions (HighLevelNodeMap + edit_dispatch for Module items)
- [x] Add `$` (FinalExpr) and `#n` (Index) path extensions
- [x] Expand test suite (39 tests: 24 AgentTools + 15 HighLevelNodeMap)
- [x] Implement read actions: GetSyntax, GetStatics, GetContext
- [x] Wire read actions through Agent.re (tool definitions, dispatch, LLM message)
- [x] Read action tests (10 tests: syntax, statics, context, modules, indices)
- [x] TypeAnnotation target for structural edits (5 tests)
- [x] Seq/Test expression-line node map support (6 tests)
- [x] Parse error detection in edit validation (1 test)
- [x] Refactor edit_dispatch with validate_edit helper
- [x] Augment mk_context_message with cursor-position type info
- [x] Module Insert/Delete/BindingClause via TermEdit (term-level transformations)
- [x] Fix Delete(BindingClause) to remove items cleanly (not leave holes)
- [x] Fix gather_top_level ordering (sort by sibling_idx for source order)
- [x] Enhance parse_error_check: add Invalid, MultiHole detection
- [x] Fix validate_edit: add validate_edit_full returning (z, info_map, node_map)
- [x] Audit cursor context: format_cursor_context sent unconditionally on every message; not useful for path-based agent but low priority to change (deferred)
- [x] Expand TermEdit to handle ALL edit operations (not just module items)
  - All edit_dispatch cases now use TermEdit (term-level round-trip) instead of segment-level Select/overwrite
  - AutoFormat mode in ExpToSegment handles whitespace heuristically (no stored secondary needed)
  - Pattern rename done at term level via rename_var_in_exp (respects shadowing, handles both Let and ModLet)
  - TyAlias definition updates dispatched to update_type_annotation (Typ ID, not Exp ID)
  - insert_binding strips trailing " in" from user code before parsing
  - Fixed should_add_space in ExpToSegment to add spaces around ":" for type annotations
  - 68 tests pass (45 AgentTools + 23 supporting)
- [ ] Improve parse_error_check messages: line numbers, syntax excerpts (thread Measured.t + TermData.t)
- [ ] Add completeness_check: track EmptyHoles (grout vs explicit `?`, distinguished at segment level)
- [ ] Copy whitespace from neighboring items for TermEdit insertions (instead of hardcoded space)
- [ ] Add more complex test programs (inspired by study/debugging programs: sum types, records, tests, case dispatch)

### Known compromises & issues in the TermEdit approach

1. **AutoFormat `should_add_space` change is global**: Modified ExpToSegment.re's
   `should_add_space` to always add spaces around `:` and `::`. Previously it
   suppressed space before `:` (line 965) and only added space after `:` before
   `$` or `!` (lines 967-969). The old rules were wrong for Hazel type annotations
   (`x : Int` rendered as `x:Int`). The change only affects AutoFormat mode — the
   web UI uses PreserveExact so it's unaffected. But any code path using AutoFormat
   (e.g. CLI `format` command) will now produce different spacing around `:`. The
   Reparse tests all pass, so this seems safe, but it could surface edge cases.

2. **Whitespace for new terms is heuristic, not contextual**: AutoFormat uses
   `should_add_space` to decide spacing. This means ALL programmatic edits get the
   same heuristic spacing regardless of context. For example, a definition inserted
   into a single-line program and a multi-line program both get single-space
   formatting. The previous approach of copying secondary from neighbors would
   preserve the original formatting style, but was fragile. The heuristic approach
   produces correct but potentially style-inconsistent output.

3. **Module item insertion whitespace is hardcoded**: `exp_to_mod_item` always adds
   a single space before new module items (via `([Space(" ")], [])` secondary).
   Multi-line modules would ideally get newline separation. This should be addressed
   when we have a general strategy for secondary on new terms.

4. **Pattern rename is limited to Var patterns**: `pat_var_name` only extracts names
   from `Var(n)` and `Asc(Var(n), _)` patterns. Tuple patterns, constructor patterns,
   etc. don't get rename support. This is fine for the current use case but would need
   extension for more complex pattern renaming.

5. **Module pattern rename doesn't propagate to outer scope**: When renaming a module
   item's pattern (e.g. `m/a` → `x`), the rename only affects direct variable
   references within the module's subsequent items. Dot-access references like `m.a`
   in the outer body are NOT renamed — this is correct behavior (they're a different
   kind of reference), but the agent needs to know it should update those separately.

6. **insert_binding trailing " in" stripping is fragile**: We strip trailing ` in` from
   the code parameter to prevent double-"in" parse errors. This is a heuristic — if
   the user's code legitimately ends with ` in` as part of a nested let (e.g.
   `let x = let y = 1 in`), the strip would be wrong. In practice this shouldn't
   happen since the agent is expected to provide complete binding clauses.

7. **Type alias test programs weakened**: Two tests that changed `type T = Int` to
   `type T = Bool` were modified to remove `: T` annotations from dependent bindings.
   This avoids the type error rejection, but means we're not testing the case where
   a type alias change cascades through the program. The static error check correctly
   catches this — the question is whether the agent should be allowed to make such
   changes. Currently it can't.

8. **No sub-expression editing yet**: TermEdit operates on whole definitions, bodies,
   patterns, and types — not arbitrary sub-expressions. The path system would need
   form-slot paths (e.g. `"f/fun/if/then"`) to address sub-expressions, which is
   designed in section 3 but not implemented.

9. **Segment-level infrastructure now unused**: The segment-level helpers in
   CompositionGo (`overwrite_term`, `insert_term`, `destruct`, `introduce`) plus
   the `~syntax` and `~return` parameters are now dead code since all edit_dispatch
   cases use TermEdit. These should be cleaned up, but are left for now since they
   might be useful reference for future work.

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
path = string  /*"/" delimited name path */
```

### Current Path System (`HighLevelNodeMap.re`)

- **Format**: Slash-delimited names, e.g. `"a"`, `"a/inner"`
- **Resolution**: Linear scan of all nodes comparing name paths (O(n))
- **Naming**: Extracted from patterns via `Namer.mk_name`
- **Fuzzy caseing**: Levenshtein distance for error messages
- **Scope**: Only `Let` and `TyAlias` expressions are addressable

### Current Read/Query Actions

- `expand(paths)` / `collapse(paths)` — toggle definition visibility
- `view_entire_definition` — show full def without folds
- `view_context` — show typing context at cursor
- `show_references` — defined in JSON schema but unimplemented
- `ShowUseSites` / `ShowReferences` — defined in `CompositionActions.re`, marked TODO

### Error Feedback

Tool failures DO get sent back to the LLM as `ToolResult` messages (standard
OpenRouter protocol). The LLM sees the error and can retry naturally. However,
there's no curated error context — just the raw failure message. The old
`ErrorRound` module on dev formatted parse errors and new static errors
specifically, and prompted the LLM to fix them. This curated feedback is
missing.

### Static Context

The old `ChatLSP.Completion.get_static_context` provided expected type and
relevant values from the typing context. This used `RelevantTypes.get` and
`RelevantValues.get` — functions that **don't exist** on this branch. The TyDi
system (`TyDiCtx.re`) has similar infrastructure (`bound_variables`,
`bound_constructors`, `free_variables`) but isn't wired to the agent.

---

## 2. Critique: What Can't Be Addressed

### A. Bare expressions (non-binding program lines)

```
let x = 1 in
test 5 == x + 4 end;    /*<-- no binding name, structurally body of x */
let y = x + 1 in
y
```
No way to target just the test expression. Would need `Update(Body, "x", ...)`
and include everything after. The trailing `y` is similarly unreachable.

### B. Sub-expressions inside definitions

In `let f = fun x -> if x > 0 then x else 0 in ...`, can't address the `if`
condition, branches, etc. Can only replace the entire definition of `f`.

### C. Module items

`Module([ModLet(...), ModType(...), ModExp(...)])` items aren't in the node map.

### D. Match arms, function parameters, type annotations

Can't address individual case branches, individual fun parameters, or just the
type annotation part of a pattern like `x : Int`.

---

## 3. Path System: Design Exploration

### 3.1 Current system: name-based paths

Works well for the common case of named bindings:
```
"x"         → binding named x
"x/inner"   → binding inner nested in x's definition
```
**Strengths**: Readable, semantic, stable across minor edits.
**Limitations**: Only addresses named bindings. Fixed granularity.

### 3.2 Proposed extension: form-slot paths ("abbreviated syntax" approach)

The idea: a path reads like an abbreviation of the syntax you'd traverse from
left to right. Each segment names either a binding (by name) or a syntactic
form + slot you descend through. The path mirrors what you'd read in the code.

#### Term form reference (children slots)

```
let    → pat, def, body         if     → cond, then, else
fun    → pat, body              case  → scrut, arm[i]
typfun → tpat, body             tuple  → [i]
tyalias→ tpat, def, body        list   → [i]
ap     → fn, arg                cons   → hd, tl
binop  → left, right            seq    → expr, rest
asc    → expr, type             module → (named items or [i])
```

#### Worked examples

**Program 1**: Simple let chain
```
let x = 1 in
let y = x + 1 in
y
```

| Target | Current path | Extended path |
|--------|-------------|---------------|
| Binding x | `"x"` | `"x"` |
| x's definition (1) | `Update(Definition, "x", ...)` | same |
| y's body (y) | `Update(Body, "y", ...)` | same |
| Trailing expr after all lets | _(impossible)_ | `"$"` or `Update(Body, "y", ...)` |

Here `$` means "final expression". In this case it's the same as y's body.

**Program 2**: Function with branching
```
let f = fun x ->
  if x > 0 then x else 0
in
let result = f 5 in
result
```

| Target | Path |
|--------|------|
| f's definition (the fun) | `"f"` + `Definition` |
| fun's body (the if) | `"f/fun/body"` or `"f/def/body"` |
| if condition (x > 0) | `"f/fun/if/cond"` |
| then branch (x) | `"f/fun/if/then"` |
| else branch (0) | `"f/fun/if/else"` |
| result's def (f 5) | `"result"` + `Definition` |
| application's argument (5) | `"result/ap/arg"` |

Reading `"f/fun/if/then"` left to right: "go to f, into the fun, into the if,
take the then branch." It reads like the code itself, abbreviated.

**Program 3**: Module
```
module M = {
  let x = 1;
  type T = Int;
  let y : T = x + 1;
  test y == 2 end
} in
let z = M.x in
z
```

| Target | Path |
|--------|------|
| Module M | `"M"` |
| x inside M | `"M/x"` |
| T inside M | `"M/T"` |
| y inside M | `"M/y"` |
| test expression (4th item) | `"M/#3"` |
| y's type annotation (T) | `"M/y"` + `TypeAnnotation` |
| z (after module) | `"z"` |

Module items addressed by name (for named items) or index (for bare exprs).

**Program 4**: Match expression
```
let classify = fun x ->
  case x with
  | 0 => "zero"
  | 1 => "one"
  | _ => "other"
  end
in
classify 5
```

| Target | Path |
|--------|------|
| classify's def | `"classify"` + `Definition` |
| case scrutinee (x) | `"classify/fun/case/scrut"` |
| first branch body ("zero") | `"classify/fun/case/arm[0]/body"` |
| second branch pattern (1) | `"classify/fun/case/arm[1]/pat"` |
| wildcard branch body | `"classify/fun/case/arm[2]/body"` |

**Program 5**: Nested lets with seq
```
let x = 1 in
test x == 1 end;
let y = 2 in
test y == 2 end;
y
```

Term structure: `Let(x, 1, Seq(Test(...), Let(y, 2, Seq(Test(...), Var(y)))))`

| Target | Path |
|--------|------|
| First test | `"x/$"` or `"#0"` — first expr after x before y |
| Second test | `"y/$"` or `"#1"` |
| Final y | `"$"` |

The `$` suffix means "the non-binding expression at this scope." `"x/$"` = "x's
body, but just the expression part before the next binding." Alternately, we
number bare expressions with `#n` at each scope level.

**Program 6**: Deeply nested — the "CSS selector" feel
```
let app =
  let model = { count = 0 } in
  let update = fun msg -> fun model ->
    case msg with
    | Increment => { count = model.count + 1 }
    | Decrement => { count = model.count - 1 }
    end
  in
  let view = fun model ->
    div [
      button (text "+") Increment,
      span (text (to_string model.count)),
      button (text "-") Decrement
    ]
  in
  (model, update, view)
in
app
```

| Target | Path |
|--------|------|
| update function | `"app/update"` |
| Increment handler body | `"app/update/fun/fun/case/arm[0]/body"` |
| view's div children list | `"app/view/fun/body"` (the list) |
| model's count field | `"app/model"` + `Definition` |

The deep path `"app/update/fun/fun/case/arm[0]/body"` reads naturally: "in
app's update, go through the outer fun, inner fun, into the case, first arm,
body."

### 3.3 Path disambiguation rules

When a form name could case multiple children:
1. **Form names are unique per expression type**: `fun` only appears once in
   a definition (if there are nested funs, the path goes through each one)
2. **Slot names are unambiguous within a form**: each form's children have
   distinct names (`cond`/`then`/`else` for if, `pat`/`body` for fun)
3. **When the slot is "the obvious one"**, it can be omitted:
   - `"f/fun"` without a slot → defaults to the fun's body
   - `"f/if"` without a slot → ambiguous, require slot
4. **Names take priority over form descent**: `"f/x"` looks for a named
   binding `x` inside f before trying to case a form `x`

### 3.4 Multiple addressing schemes coexist

We don't pick one scheme — we have a **layered system**:

1. **Name paths** (existing): `"x"`, `"x/inner"` — for named bindings
2. **Form-slot paths** (new): `"x/fun/if/then"` — for sub-expression navigation
3. **Index paths**: `"#0"`, `"M/#3"` — for positional items
4. **Special tokens**: `"$"` for final expression

All coexist in a single string format. Parsing rules:
- Bare identifier → Name
- `#n` → Index
- `$` → FinalExpr
- Known form names (`fun`, `if`, `case`, `let`, `ap`, `binop`, etc.) → Form
  descent (only when no Name binding casees)
- Known slot names (`cond`, `then`, `else`, `body`, `pat`, `def`, etc.) →
  Slot selection within current form
- `arm[n]` → indexed slot (for case arms, list/tuple items)

### 3.5 Internal representation

```reason
type path_segment =
  | Name(string)
  | Index(int)
  | Form(string)          /* "fun", "if", "case", etc. */
  | Slot(string)          /* "cond", "then", "body", etc. */
  | IndexedSlot(string, int)  /* "arm", 0 → arm[0] */
  | FinalExpr             /* $ */

type path = list(path_segment)
```

String parsing produces this. The current `path_to_id` can be extended to
resolve these against the actual term structure, not just the node map.

---

## 4. Edit Actions

### 4.1 Current set (working)

All operate on the binding-level node map:
- `Update(Definition|Body|Pattern|BindingClause, path, code)`
- `Delete(BindingClause|Body, path)` — Definition/Pattern delete unimplemented
- `Insert(Before|After, path, code)`

### 4.2 Module edit actions (new, Phase 1)

Modules are genuinely flat, so the existing action vocabulary maps cleanly:

| Action | Example | Meaning |
|--------|---------|---------|
| `Update(Definition, "M/x", "42")` | Change x's def in module M |
| `Update(Pattern, "M/x", "a")` | Rename x to a in module M |
| `Insert(After, "M/x", "let z = 3")` | Add item after x in M |
| `Delete(BindingClause, "M/x")` | Remove x from M |

Implementation: extend `HighLevelNodeMap.build_children` to walk `Module(items)`
and create nodes for each `ModLet`/`ModType`/`ModuleMod`. `ModExp` items get
index names.

### 4.3 TypeAnnotation target (new, Phase 2)

```reason
type target = ... | TypeAnnotation
```

For `let x : Int = 1 in ...`, `Update(TypeAnnotation, "x", "Bool")` changes
just the type annotation. Common operation, shows clear intent.

### 4.4 Sub-expression edits (future, Phase 3+)

With form-slot paths, the existing `Update(Definition, path, code)` generalizes:
the path navigates to any sub-expression, and the target says what to do there.
But this needs the form-slot path resolution working first.

---

## 5. Read/Query Actions (New)

### 5.1 Granular read actions (Phase 2)

Most granular building blocks first. Each takes a path and returns information.

```reason
type read_action =
  | GetSyntax(path)              /* return the syntax at this path */
  | GetStatics(path, options)    /* return type info at this path */
  | GetDynamics(path, options)   /* return probe/runtime values at this path */
  | GetContext(path)             /* return typing context at this path */
```

**`GetSyntax(path)`**: Returns the pretty-printed code at the path. Could
include options for expansion depth (how many levels of nested bindings to
show before folding).

**`GetStatics(path, options)`**: Returns:
- Analytic type (expected/required type from context)
- Synthetic type (inferred type of the expression)
- Status (consistent, inconsistent, unknown)
- Optionally: relevant context entries caseing the expected type

Options might include `{expected_type: bool, relevant_ctx: bool}` —
similar to what ChatLSP had, using TyDiCtx infrastructure.

**`GetDynamics(path, options)`**: Returns probe/runtime values. Uses the
existing `Sample.Cursor.t` system:
- Current sample values at this node
- Available samples (how many, from which call contexts)
- Step range information

Options: `{sample_index: option(int), call_stack: option(call_stack)}`

**`GetContext(path)`**: Returns what's in scope — variable names, types,
type aliases, modules.

### 5.2 Annotated views (Phase 2-3)

Compose the granular reads into richer views:

**Syntax + statics**: Show the code with type annotations inline.
```
let x : Int = 1 in        /*x : Int, synthesized */
let f : Int -> Bool =      /*f : Int -> Bool, synthesized */
  fun x -> x > 0           /*body : Bool, expected Bool */
in
f x                        /*: Bool */
```

**Syntax + dynamics**: Show the code with runtime values.
```
let x = 1 in               /*x = 1 */
let f = fun x -> x > 0 in  /*f = <fun> */
f x                        /*= true, via x=1 */
```

These are "tabular" views — primary column is syntax, additional columns are
semantic attributes. Could be formatted as:
```
| Syntax              | Type         | Value    |
|---------------------|--------------|----------|
| let x = 1           | Int          | 1        |
| let f = fun x -> ...| Int -> Bool  | <fun>    |
| f x                 | Bool         | true     |
```

### 5.3 Query-driven views (Phase 3+)

Compositional queries for selective expand/collapse with semantic filtering:

```
query("x", "y")              → show bindings x and y expanded
query("M/*")                  → all items in module M
query("@refs(x)")             → all use sites of variable x
query("@type(Int)")           → all bindings with type Int
query("@errors")              → all bindings with type errors
query("@refs(x) & M/*")      → use sites of x that are inside M
```

Multi-cursor edits building on queries:
```
UpdateAll("@refs(x)", "y")    → rename all references
```

---

## 6. Restore Static Context & Error Feedback

### 6.1 Error feedback loop (Phase 2)

**Current state**: Tool failures → raw error message → ToolResult → LLM can
retry. This works at the protocol level but the error messages aren't curated.

**Enhancement**: When a structural action fails due to static errors, format
the feedback using the old ErrorRound approach:
1. Parse error detection (uncaseed delimiters via `Zipper.local_backpack`)
2. Differential static error detection (new errors vs. pre-existing)
3. Curated feedback message: "The following static errors were discovered: ...
   Please try to address them."

This doesn't need a separate retry loop — the standard ToolResult mechanism
handles retries. We just need better error messages.

**Implementation**: Add a formatting layer in `CompositionGo.re` that uses
`ErrorPrint.all` differentially and checks for parse errors before attempting
the structural edit.

### 6.2 Static context for agent (Phase 2)

**Goal**: Give the agent curated type context for the current editing position.

**Approach 1 — Read action**: `GetStatics(path)` returns expected type +
relevant bindings. Uses `TyDiCtx.bound_variables`, `TyDiCtx.bound_constructors`,
etc. to find what's in scope and consistent with the expected type.

**Approach 2 — Annotated view**: When the agent expands a definition, show
type annotations alongside the code (refractor-style, but text-only for now).

**Approach 3 — Context message enhancement**: Augment the auto-sent context
message (`mk_context_message` in Agent.re) with expected-type info at the
current cursor position.

All three are useful. Approach 1 is most granular. Approach 3 is cheapest to
implement and gives always-on context.

### 6.3 Dynamic cursor for agent (Phase 2-3)

The `Sample.Cursor.t` system is fully implemented:
- Call stack tracking, pinning, step-into
- Sample selection with windowing
- `SampleCursorPerform.re` for capture/toggle_pin/reset

**To wire to agent**: Add tool definitions for:
- `capture_sample(probe_path, sample_index)` — focus on a specific sample
- `toggle_pin(call_stack)` — filter samples by call context
- `get_samples(path)` — return available samples at a probe
- `step_into(sample, function_id)` — navigate into a function call

The dynamic cursor state already lives in the editor and is dispatched via
`Action.Project(SampleCursor(...))`.

---

## 7. Test Suite Plan

### 7.1 Existing action edge cases

- [ ] Nested lets: `let a = let b = let c = 1 in c in b in a` — path `"a/b/c"`
- [ ] Type alias editing: `type T = Int in let x : T = 1 in x`
- [ ] Pattern with annotation: `let x : Int = 1 in x`
- [ ] Tuple patterns: `let (a, b) = (1, 2) in a + b`
- [ ] Shadowed names: `let a = 1 in let b = let a = 2 in a in b`
- [ ] Empty body (hole): `let a = 1 in ?`
- [ ] Static error rejection: confirm bad edits are rejected
- [ ] Invalid path: confirm useful error message with suggestion

### 7.2 Module actions

- [ ] Node map for module: `module M = { let x = 1; let y = 2 } in M`
- [ ] Update module item definition
- [ ] Insert module item
- [ ] Delete module item
- [ ] Rename module item
- [ ] Nested modules: `module M = { module N = { let x = 1 } } in M`
- [ ] Module with type aliases
- [ ] Module with bare expressions (indexed)
- [ ] ModuleExp node map structure

### 7.3 Path resolution

- [ ] Name-based (backward compat)
- [ ] Index-based (`#0`, `#1`)
- [ ] FinalExpr (`$`)
- [ ] Form-slot (when implemented): `"f/fun/if/then"`
- [ ] Error messages for bad paths

### 7.4 Read actions

- [ ] GetSyntax returns correct code at path
- [ ] GetStatics returns expected/synthesized type
- [ ] GetContext returns in-scope bindings
- [ ] GetDynamics returns sample values (when wired)

---

## 8. Implementation Order

### Phase 1: Module support + tests
1. Extend `HighLevelNodeMap.build_children` for `Module(items)`
2. Extend `Namer` for module patterns (`MPat`)
3. Extend `get_inner_term_id` and `edit_dispatch` for module items
4. Add `$` (FinalExpr) and `#n` (Index) to path parsing
5. Comprehensive tests for existing + module actions
6. Test edge cases for existing actions

### Phase 2: Read actions + restored context
7. Implement `GetSyntax(path)` — return code at path
8. Implement `GetStatics(path)` — return type info using TyDiCtx
9. Implement `GetContext(path)` — return in-scope bindings
10. Enhance error messages in `CompositionGo.re` (parse errors, differential statics)
11. Augment `mk_context_message` with expected-type info
12. `TypeAnnotation` target for patterns
13. Seq/expression-line node map support

### Phase 2.5: Term-level syntax transformations

**Motivation**: The current edit operations use `Parser.to_segment` + `Zipper.insert_segment`
to splice code. This creates tiles in Exp sort context regardless of where they're inserted.
For module items (Mod sort context), this produces wrong-sort tiles (e.g., `["let","=","in"]`
instead of `["let","="]`). Now that whitespace is stored in terms (via `IdTagged.annotation.secondary`),
we can do syntax transformations at the term level instead, sidestepping sort issues entirely.

**Round-trip infrastructure** (already exists):
- `Parser.to_term(string) → option(Exp.t)` — parse to term with secondary
- `ExpToSegment.exp_to_segment(~settings={secondary: PreserveExact, ...}, term) → Segment.t`
- `Printer.of_segment(segment) → string`
- Round-trip tests in `Test_ExpToSegment.re` (200+ tests)

**New approach for edit operations**:
1. Parse the current program to a term: `MakeTerm.from_zip_for_sem(z).term`
2. Locate the target sub-term by ID (walk the term tree using node map path)
3. Splice in the new sub-term (parsed from the code string)
4. Convert the modified term back to a segment via `ExpToSegment`
5. Replace the zipper contents with the new segment

**Specific items**:
- [x] 14a. Implement TermEdit.re: term-level helpers for module item manipulation (replace_module_items, find_module_containing_item, exp_to_mod_item, insert/delete/replace_item)
- [ ] 14b. Extend term-level approach to non-module edits (replaces segment-based approach) — deferred
- [x] 14c. Module Insert: parse code as Exp term, wrap as ModLet/ModType/ModExp, insert into Module(items) list with proper secondary whitespace
- [x] 14d. Module Delete: remove item from Module(items) list cleanly (no leftover holes)
- [x] 14e. Module Update(BindingClause): replace the ModLet/ModType item directly in the items list
- [x] 14f. Fix gather_top_level ordering: sort by sibling_idx (set during build_siblings_and_trim from dummy root's children order)
- [x] 14g. Enhance parse_error_check: detect Invalid nodes and MultiHole via Exp.map_term walk
- [x] 14h. Fix validate_edit: added validate_edit_full returning (z, info_map, node_map); Pattern case refactored to use it
- [x] 14i. Audit cursor context: format_cursor_context sent unconditionally on every message; not useful for path-based agent. Deferred — low priority.

**Architecture note**: Consider lightweight binding-structure-only analysis instead of full statics for node map building. Currently we run `Statics.mk` just to get ancestor chains and term structure. A simpler walk could provide the same info without type checking. However, full statics IS needed for post-edit validation (differential error checking), so the savings may be small in practice.

### Phase 3: Deep paths + dynamics
14. Form-slot path parsing and resolution against term structure
15. Wire `Sample.Cursor.t` to agent tools (capture, pin, step-into)
16. `GetDynamics(path)` — return probe values at path
17. Annotated views (syntax + statics, syntax + dynamics)

### Phase 4: Compositional queries
18. Query language design and implementation
19. Multi-cursor/multi-edit operations
20. Collapsed/expanded view composition from query results

---

## 9. Design Notes from Review

### 9.1 Delimiter-based child naming

Instead of inventing slot names (`cond`, `then`, `else`), use the actual
delimiters from the syntax where possible:
- `if` has delimiters `if`, `then`, `else` → children addressed by those tokens
- `fun` has `fun`, `->` → first child gets form name, body gets `->` (or default)
- `let` has `let`, `=`, `in` → pattern is `let`/default, def is `=`, body is `in`
- `case` (NOT `case` — Hazel syntax is `case`) has `case`, `|`, `=>`

Rule: the prefix delimiter names the first child (or the form itself). Subsequent
delimiters name subsequent children. A trailing child with no delimiter needs a
special symbol.

### 9.2 Definition/Pattern/etc should become path segments

The current `target` type (`Definition`, `Pattern`, `Body`, `BindingClause`)
should probably fold into the path itself:
- `Update("f/def", code)` instead of `Update(Definition, "f", code)`
- `Update("f/pat", code)` instead of `Update(Pattern, "f", code)`

This unifies the addressing — everything is just a path, and the action is
just `Update(path, code)` or `Delete(path)`.

### 9.3 Names are pattern caseers

When you write `"x"` in a path, you're really saying "find the binding whose
pattern casees x." This generalizes: for tuple patterns `(a, b)`, the name
is the rendered pattern. For case arms, you could address by pattern text.
The current system already does this via `Namer.mk_name_from_pat`.

### 9.4 Sibling selectors

Want to address case arms by pattern, not just index. E.g., for
`case x | 0 => "zero" | _ => "other" end`, address the wildcard arm as
`"classify/case/_/body"` (pattern as selector among siblings).

More generally: sibling selectors let you pick among children of a node by
caseing on a property of one child (the pattern) and then accessing another
child (the body) of the same branch.

### 9.5 `in` and `;` as equivalent separators

Let chains (`let x = 1 in let y = 2 in ...`) and semicolons
(`test a end; test b end; ...`) are both sequential. The flat list view
should treat them uniformly. Index addressing (`#0`, `#1`) works for
both named and unnamed items in the sequence.

### 9.6 GetStatics is a heuristic package

`GetStatics` should not just return the typing context. It should be a
curated static slice: expected type, synthesized type, recursively gathered
type aliases that appear in those types, relevant bindings. This is the
"static contextualization" concept — a heuristic bundle of useful static
info at a given program point.

### 9.7 Combined views as default

When requesting a view of code at a path, static and dynamic info should
come along by default (opt-out, not opt-in). Action sequences naturally
produce combined views.

### 9.8 Hazel syntax notes

- Comments: `/* ... */` (NOT `/*... */`)
- Case expressions, not case: `case x | 0 => ... | _ => ... end`
- Probes already give dynamics (will improve with probes branch merge)

---

## 10. Open Questions

1. **Path string syntax**: Is the proposed syntax (`#n`, `$`, `arm[n]`,
   form names) reasonable? Should form-slot segments use a different
   separator (e.g., `.` for slots vs `/` for names)?

2. **Default slot**: When you say `"f/fun"` without a slot name, should it
   default to `body`? This makes paths shorter for the common case of
   descending through function bodies.

3. **Name vs form priority**: If a binding is named `fun` (unlikely but
   legal), does `"fun"` case the name or the form? Proposal: names always
   win; use explicit form syntax like `@fun` for form descent.

4. **Module shadowing**: `"M/x"` targets last effective binding. `"M/#n"`
   for positional. Is this the right default?

5. **How to present statics to agent**: As a separate tool result? Inline
   annotations? Always-on in context messages? Probably: separate granular
   tool + always-on summary in context.

6. **Error feedback granularity**: Should error messages distinguish parse
   errors vs. type errors vs. scope errors? The old ErrorRound did. Probably
   yes — parse errors suggest syntax issues, type errors suggest logic issues.
