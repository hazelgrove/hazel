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
- [x] Clean up dead segment-level code (overwrite_term, insert_term, destruct, statics_map_new_ids, format_ctx_entry)
- [ ] Improve parse_error_check messages: line numbers, syntax excerpts (thread Measured.t + TermData.t)
- [x] Add completeness_check: track EmptyHoles via GetCompleteness read action
- [ ] Copy whitespace from neighboring items for TermEdit insertions (instead of hardcoded space)
- [x] Re-evaluate error gating strategy: warnings instead of rejection
  - Static errors now produce warnings, not hard failures
  - Parse errors (unmatched delimiters, Invalid/MultiHole) still block
  - Allows multi-step refactoring (e.g., change type alias, then fix dependents)
- [ ] Generalize pattern actions: split Update(Pattern) into "replace whole pattern" + separate "RenameVariable" action
- [x] Add more complex test programs (sum types, records, case dispatch, selectors)
- [x] **Path selector language** (see `plans/Hazel-Agent-Path-Selector-Language.md`)
  - [x] Parse concrete selector syntax into AST
  - [x] Implement resolver against Hazel term tree
  - [x] Start with read actions (Select via selector) — heavily unit-tested
  - [ ] Incrementally extend to edit actions (replacing current path system)
- [x] **Sequence element editing** (unified approach for all "spine" structures)
  - All sequence-type structures follow the module item pattern:
    Insert(Before/After), Delete, Update on individual elements
  - [x] **Case arms**: addressable as `"f/|A"`, `"f/|Some(x)"` (pipe-prefixed pattern names)
    - TermEdit: parse_case_arm, case_delete_arm, case_insert_arm, case_update_arm_body, case_update_arm_pattern
    - HighLevelNodeMap indexes arm bodies as children of enclosing node
    - 22 tests (TermEdit + node map + dispatch)
  - [x] **List elements**: addressable as `"xs/[0]"`, `"xs/[1]"`, `"xs/[2]"`
    - TermEdit: list_delete_element, list_insert_element, list_update_element
    - 8 tests (TermEdit + node map + dispatch)
  - [x] **Tuple elements**: addressable as `"p/(0)"`, `"p/(1)"` (positional) or `"p/x"`, `"p/y"` (labeled)
    - TermEdit: tuple_delete_element, tuple_insert_element, tuple_update_element
    - Labeled tuple elements named by label, unlabeled by index
    - 8 tests (TermEdit + node map + dispatch)
  - All wired through CompositionGo dispatch with is_case_arm/is_list_element/is_tuple_element detection
- [ ] **Improve dispatch error reporting**: distinguish failure reasons in edit_dispatch
  - Path resolution failures (path doesn't point to any node)
  - Action inapplicability (e.g., Update(Pattern) on a list element)
  - Code parsing failures (code string doesn't parse as valid syntax)
  - TermEdit internal failures (target ID not found in term tree)
  - Currently TermEdit returns `option(Zipper.t)` — consider `result(Zipper.t, string)` for failure reasons
  - Currently misleading messages like "Failed to parse" when the real issue is wrong node kind
  - Consider: explicit inapplicability checks before attempting the operation
- [ ] **Evaluate `target` vs path unification**: decide whether `Definition|Body|Pattern|...` target enum should be absorbed into the path
  - The extended path design (section 3.2/9.2) already proposes `"f/def"`, `"f/pat"`, `"f/body"` as path suffixes
  - Could simplify `Action.Structural.t` to just `Update(path, code)` / `Delete(path)` / `Insert(Before|After, path, code)`
  - Internal implementation question: should `Update(Definition, "f", code)` be sugar for `Update("f/def", code)`?
  - Benefits: simpler API surface, paths are self-describing, one resolution mechanism
  - Risks: target enum provides useful static guarantees (inapplicability checks at type level)
  - Step 1: ensure internal dispatch converges — target-based and path-based should use the same underlying resolution
  - Step 2: decide whether to keep targets as sugar, remove them, or keep them as the primary API
  - Related: case arm/list/tuple elements already use path-only addressing (no target beyond Body)
- [x] **Action Explorer UI**: developer toolbar for interactive path/action exploration
  - Toggle via Nut Menu > Developer Settings > "Action Explorer"
  - Bar below top bar with: action type selector, target/direction dropdowns, path input, code input, execute button
  - Live highlights: as user types in path field, resolve against HighLevelNodeMap and/or Selector,
    highlight matched nodes in the editor using decoration system (Highlight.re)
  - Read actions exposed: GetSyntax, GetStatics, GetContext, Select, GetCompleteness
  - Implementation: ActionExplorer.re (Model/Update/View), Settings.show_action_explorer flag,
    highlight integration via Highlight.color with `action-explorer-match` CSS class,
    Globals.action_highlights for passing IDs to CodeEditable deco
  - [ ] **Highlight caret interaction**: highlights change shape as caret enters/exits highlighted
    term (per-shard rendering means caret splits the shape). Expected behavior but could be
    improved with a separate overlay layer that doesn't depend on shard splitting.
  - [ ] **GetCompleteness with path**: currently whole-program only; could accept an optional
    path to scope hole-counting to a subtree
- [ ] **Selector language gaps** (from audit against spec):
  - Missing keyword resolvers: `fun`, `test`, `end` (tokenized but no walk handler)
  - Missing `:` (colon) delimiter resolver
  - Missing list spine matching: `[ * , ... ]` and `[ ... , * ]` patterns
  - Section 9 (selector-driven edits) not wired — edits use HighLevelNodeMap paths only
  - `query_unique` exists but not integrated into edit_dispatch
- [ ] **Strategic edit granularity audit**: survey what precise edits agents/programmers need
  - Current: whole definition/body/pattern/type-annotation/binding-clause
  - Missing: case arms, list/tuple/labeled-tuple elements, function parameters
  - Goal: identify the minimal set of granular edit operations for expressive intent
  - Consider: what edits express precise intent vs what can be done by replacing larger chunks?

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
   patterns, and types — not arbitrary sub-expressions. The selector language
   (see `plans/Hazel-Agent-Path-Selector-Language.md`) can address sub-expressions
   for reads but is not yet wired to edit actions.

9. **Segment-level infrastructure now unused**: The segment-level helpers in
   CompositionGo (`overwrite_term`, `insert_term`, `destruct`, `introduce`) plus
   the `~syntax` and `~return` parameters are now dead code since all edit_dispatch
   cases use TermEdit. These should be cleaned up, but are left for now since they
   might be useful reference for future work.

---

## 1. Systems Map: Addressing & Path Systems

We have **two addressing systems** that overlap in some areas. This section maps
what each does, where they converge, and where they should converge further.

### 1.1 HighLevelNodeMap paths (edit-oriented)

**Used by**: all edit actions (`edit_dispatch`), `GetSyntax`, `GetStatics`, `GetContext`

**Path format**: slash-delimited segments with multiple addressing modes:
```
path_segment = Name(string) | Index(int) | FinalExpr
```

| Syntax | Example | Meaning |
|--------|---------|---------|
| bare name | `"x"`, `"x/y"` | binding named x, nested y inside x |
| `#n` | `"#0"`, `"M/#2"` | nth item (0-indexed) at current scope |
| `$` | `"$"`, `"x/$"` | final expression at current scope |
| `\|pat` | `"f/\|A"`, `"f/\|Some(x)"` | case arm by pattern name |
| `[n]` | `"xs/[0]"`, `"xs/[1]"` | list element by index |
| `(n)` | `"p/(0)"`, `"p/(1)"` | tuple element by index |
| label | `"p/x"`, `"p/y"` | labeled tuple element by label |

**Resolution**: `parse_path` → `resolve_path` walks node tree level-by-level.
Fallback: original full-path-name comparison for backward compat.
Error: Levenshtein "did you mean?" suggestion.

**Node map construction**: `build(zipper, info_map)` walks the term tree and
creates nodes for Let, TyAlias, Module items, Seq/Test lines, case arms, list
elements, and tuple elements. Each node has: info, path (ancestor IDs),
children, siblings, sibling_idx, name.

**Combined with `target` enum**: edit actions use `Action.Structural.t`:
```
Update(target, path, code)  |  Delete(target, path)  |  Insert(Before|After, path, code)
target = Definition | Body | Pattern | BindingClause | TypeAnnotation
```
The `target` selects a sub-part of the node at `path` (via `get_inner_term_id`).
For sequence elements (case arms, list/tuple), the `target` is mostly ignored —
the path points directly to the element.

### 1.2 Selector language (query-oriented)

**Used by**: `Select` read action only (not yet wired to edits)

**Format**: whitespace-separated tokens matching Hazel syntax patterns:
```
let x = *         → x's definition
let x … in *      → x's body
if … else *       → else branch
A/B/f ⋱ if … else * → descend into nested binder chain
| Increment => *  → case arm body
[ … , * ]         → last list element
```

**Operators**: `_` (one slot), `_...`/`…` (zero+ slots), `⋱` (descendant search),
`*` (focus marker — what gets returned), `A/B/C` (binder chain sugar)

**Resolution**: pattern-match against `Exp.t` tree. Returns 0..N matches, each
with focused subtree, ID, and breadcrumb string.

**Disambiguation**: queries return all matches. `query_unique` requires exactly 1
match (for potential edit use).

### 1.3 Overlap and convergence

| Capability | NodeMap paths | Selectors |
|------------|--------------|-----------|
| Named bindings | `"x"`, `"x/y"` | `let x`, `A/B/C` chain |
| Sub-expressions | NOT YET | `⋱ if … else *` |
| Sequence elements | `"\|A"`, `"[0]"`, `"(0)"` | `\| A => *`, `[ … , * ]` |
| Target (def/body/pat) | `target` enum | `let x = *` vs `let x … in *` |
| Edit actions | YES | NOT YET (designed, section 9 of selector plan) |
| Read queries | YES (GetSyntax etc.) | YES (Select) |
| Multiple matches | No (single or error) | Yes (0..N) |
| Error messages | "did you mean?" | breadcrumbs |

**Key insight**: the selector language subsumes HighLevelNodeMap paths for
expressiveness. The `target` enum maps directly to selector patterns:
- `Definition` = `let x = *`
- `Body` = `let x … in *`
- `Pattern` = `* let x` (with focus on pattern extraction)
- `TypeAnnotation` = ascription focus

The HighLevelNodeMap path system remains useful as a simpler, more constrained
interface that's good for the common case. The selector language adds power for
sub-expression access and pattern-based matching.

**Convergence plan** (see to-do items above):
1. Wire selectors to edit actions (selector plan section 9) — requires `query_unique`
2. Consider making the `target` enum sugar over selector resolution internally
3. Ensure both systems produce consistent results for overlapping cases
4. Consider disambiguating annotations (e.g. `@1` for shadowed names) in both systems

### 1.4 Architecture

```
Agent.re
├── edit actions → CompositionGo.edit_dispatch
│   └── HighLevelNodeMap.path_to_node(path) → target → TermEdit.*
│       └── Parse code → modify term tree → ExpToSegment → new Zipper
└── read actions → CompositionGo.read_dispatch
    ├── Select(selector) → Selector.query(selector, term) → matches
    ├── GetSyntax(path) → HighLevelNodeMap → segment_of_term → Printer
    ├── GetStatics(path) → HighLevelNodeMap → Info.t → format
    ├── GetContext(path) → HighLevelNodeMap → Ctx → format
    └── GetCompleteness → count_holes(z)
```

---

## 2. What Can't Be Addressed (remaining gaps)

### RESOLVED:
- ~~Bare expressions~~ → Seq/Test node map support, `#n` and `$` paths
- ~~Module items~~ → Module node map + TermEdit module operations
- ~~Case arms~~ → case arm node map + TermEdit case operations
- ~~Type annotations~~ → TypeAnnotation target

### Remaining:
- **Sub-expressions inside definitions**: `"f/fun/if/then"` style paths.
  The selector language CAN address these for reads (`let f = ⋱ if … else *`).
  Not yet wired to edits. TermEdit would need per-form splice operations.
- **Function parameters**: can't address individual parameters of `fun x y z -> ...`.
  Could be a sequence element type (like case arms/list elements).
- **Shadowed names**: `let a = 1 in let a = 2 in a` — both bindings are named `a`.
  HighLevelNodeMap handles this by matching the first `a` found at each level.
  May need disambiguation syntax (e.g. `a@1` for second definition of `a`)
  both in path addresses and potentially in displayed output syntax.

---

## 3. Path System: Design Exploration

> **NOTE (March 2026):** The form-slot path design in sections 3.2-3.5 below has been
> **largely superseded** by the selector language (see `plans/Hazel-Agent-Path-Selector-Language.md`
> and `Selector.re`). The selector language provides the same sub-expression addressing
> capability with a more expressive pattern-matching approach. The worked examples below
> remain useful as motivation for what the selector language solves, and some ideas
> (like index notation `arm[n]`, disambiguation syntax `a@1`) may still be adopted
> into the selector language or the HighLevelNodeMap path system.

### 3.1 Current system: name-based paths (IMPLEMENTED)

Works well for the common case of named bindings, plus extensions:
```
"x"         → binding named x
"x/inner"   → binding inner nested in x's definition
"#0", "#1"  → item by index (IMPLEMENTED)
"$"         → final expression (IMPLEMENTED)
"f/|A"      → case arm by pattern (IMPLEMENTED)
"xs/[0]"    → list element by index (IMPLEMENTED)
"p/(0)"     → tuple element by index (IMPLEMENTED)
```
**Strengths**: Readable, semantic, stable across minor edits, covers common cases.
**Limitations**: Can't address sub-expressions (if branches, fun bodies, etc.).

### 3.2 Previously proposed extension: form-slot paths

> **STATUS: SUPERSEDED** by the selector language. Kept for historical reference.

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

### 3.3 Path disambiguation rules (SUPERSEDED — see selector language)

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

### 3.4 Multiple addressing schemes coexist (PARTIALLY IMPLEMENTED)

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

### 3.5 Internal representation (SUPERSEDED — see selector language)

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

### 4.2 Module edit actions (IMPLEMENTED)

All module edit actions work via TermEdit + HighLevelNodeMap:

| Action | Example | Meaning |
|--------|---------|---------|
| `Update(Definition, "M/x", "42")` | Change x's def in module M |
| `Update(Pattern, "M/x", "a")` | Rename x to a in module M |
| `Insert(After, "M/x", "let z = 3")` | Add item after x in M |
| `Delete(BindingClause, "M/x")` | Remove x from M |

### 4.3 TypeAnnotation target (IMPLEMENTED)

`Update(TypeAnnotation, "x", "Bool")` changes just the type annotation on `let x : Int = ...`.

### 4.4 Sequence element editing (IMPLEMENTED)

Case arms, list elements, and tuple elements all support Insert/Delete/Update:

| Action | Example | Meaning |
|--------|---------|---------|
| `Update(Body, "f/\|A", "0")` | Change arm A's body |
| `Update(Pattern, "f/\|A", "B")` | Change arm A's pattern |
| `Insert(After, "f/\|A", "\| C => 3")` | Insert arm after A |
| `Delete(BindingClause, "f/\|A")` | Delete arm A |
| `Update(Body, "xs/[0]", "42")` | Change first list element |
| `Insert(Before, "xs/[1]", "99")` | Insert before second element |
| `Delete(BindingClause, "p/(0)")` | Delete first tuple element |

### 4.5 Sub-expression edits (future)

The selector language can address sub-expressions (`let f = ⋱ if … else *`).
Wiring selectors to edit actions would enable replacing arbitrary sub-expressions.
This would use `Selector.query_unique` for single-match resolution, then TermEdit
for the actual replacement. Main design question: should this be a new action type
(`UpdateSelector(selector, code)`) or should we unify the path format?

---

## 5. Read/Query Actions

### 5.1 Granular read actions (MOSTLY IMPLEMENTED)

```reason
type read_action =
  | GetSyntax(path)         /* IMPLEMENTED — return code at this path */
  | GetStatics(path)        /* IMPLEMENTED — return type info at this path */
  | GetContext(path)         /* IMPLEMENTED — return typing context at this path */
  | Select(selector)         /* IMPLEMENTED — selector language queries */
  | GetCompleteness          /* IMPLEMENTED — count unfilled holes */
  | GetDynamics(path, options) /* NOT YET — return probe/runtime values */
```

**`GetSyntax(path)`** (IMPLEMENTED): Returns pretty-printed code at the path.

**`GetStatics(path)`** (IMPLEMENTED): Returns analytic type, synthesized type,
status (consistent/inconsistent/error), and errors in subtree.

**`GetContext(path)`** (IMPLEMENTED): Returns variables, type aliases, and
constructors in scope at the path. Uses `Ctx.filter_shadowed`.

**`Select(selector)`** (IMPLEMENTED): Uses the selector language for flexible
pattern-matching queries. Returns all matches with focused subtrees.

**`GetCompleteness`** (IMPLEMENTED): Counts expression, pattern, and type holes.

**`GetDynamics(path, options)`** (NOT YET): Would return probe/runtime values.
Uses existing `Sample.Cursor.t` system. Options would include sample index
and call stack filtering. See section 6.3.

### 5.2 Annotated views (NOT YET)

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

## 6. Static Context & Error Feedback

### 6.1 Error feedback loop (IMPLEMENTED)

Parse errors block edits (unmatched delimiters, Invalid tokens, MultiHole).
Static errors produce warnings but do NOT block (allows multi-step refactoring).
Error messages include node type, path, and code for debugging.

Implementation: `parse_error_check` + `static_error_warning` in `CompositionGo.re`.
`validate_edit` combines both checks.

### 6.2 Static context for agent (PARTIALLY IMPLEMENTED)

Three approaches, two implemented:

**Approach 1 — Read actions** (IMPLEMENTED): `GetStatics(path)` returns expected
type, synthesized type, status, and subtree errors. `GetContext(path)` returns
in-scope variables, type aliases, and constructors.

**Approach 2 — Annotated views** (NOT YET): Show code with inline type annotations.
See section 5.2 above.

**Approach 3 — Context message** (IMPLEMENTED): `mk_context_message` in Agent.re
sends cursor-position type info unconditionally on every message. Low priority
to optimize since it's informational.

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

## 7. Test Suite Status

**124 tests total** (all passing). Test groups:

### 7.1 AgentTools tests (comprehensive)
- Basic edit operations (update definition, body, pattern, binding clause)
- Module operations (insert, delete, update, rename, nested modules, bare exprs)
- Case arm operations (insert, delete, update body, update pattern)
- List element operations (insert, delete, update)
- Tuple element operations (insert, delete, update, labeled)
- TypeAnnotation updates
- Dispatch-level round-trip tests for all operation types
- Path resolution (names, `#n`, `$`, sequence element names)
- Read actions (GetSyntax, GetStatics, GetContext)
- Parse error detection
- Static error warnings

### 7.2 Selector language tests (in Test_AgentTools.re)
- Tokenization, elaboration, resolution
- Binder chains, descendant search, focus
- Complex programs (modules, case, nested lets)

### 7.3 HighLevelNodeMap tests
- Node map construction for various program structures
- Path resolution for all naming conventions
- Sibling ordering

### 7.4 Not yet tested
- [ ] Shadowed name disambiguation
- [ ] Selector-driven edits (when implemented)
- [ ] GetDynamics (when implemented)

---

## 8. Implementation Order

### Completed phases

**Phase 1** (DONE): Module support, `$`/`#n` paths, HighLevelNodeMap, tests.

**Phase 2** (DONE): Read actions (GetSyntax/GetStatics/GetContext/GetCompleteness),
TypeAnnotation target, Seq/Test node map, parse error detection, error gating
(warnings not rejection), context message enhancement.

**Phase 2.5** (DONE): TermEdit term-level transformations for ALL edit operations.
All edit_dispatch cases use TermEdit (term tree → splice → ExpToSegment → new Zipper).
Module, case arm, list, tuple operations all implemented.

**Selector language** (DONE for reads): tokenizer, elaborator, resolver implemented.
Select read action wired through Agent.re.

### Next phases

**Convergence & cleanup** (current):
- [ ] Evaluate `target` vs path unification (see to-do above)
- [ ] Wire selectors to edit actions (`query_unique` → TermEdit splice)
- [ ] Ensure HighLevelNodeMap and Selector produce consistent results
- [ ] Add disambiguation annotations for shadowed names
- [ ] Clean up dead segment-level code in CompositionGo
- [ ] Improve dispatch error reporting (see to-do above)

**Dynamics & annotated views** (future):
- [ ] Wire `Sample.Cursor.t` to agent tools (capture, pin, step-into)
- [ ] `GetDynamics(path)` — return probe values at path
- [ ] Annotated views (syntax + statics, syntax + dynamics)

**Compositional queries** (future):
- [ ] Multi-match edits (`UpdateAll`)
- [ ] Collapsed/expanded view composition
- [ ] Semantic filters (`@refs(x)`, `@type(Int)`, `@errors`)

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

### Resolved
- ~~Path string syntax~~ → `#n`, `$`, `|pat`, `[n]`, `(n)` all implemented in HighLevelNodeMap.
  Selector language provides the sub-expression addressing that form-slot paths would have.
- ~~How to present statics to agent~~ → GetStatics read action + always-on context message.
- ~~Error feedback granularity~~ → Parse errors block, static errors warn. Messages include
  node type, path, code, and "did you mean?" suggestions.

### Still open
1. **Target vs path unification**: Should `Definition|Body|Pattern|...` be folded into the
   path/selector? Or kept as sugar? Selector language can express `let x = *` (definition)
   vs `let x … in *` (body), so the target enum is technically redundant. But it constrains
   the agent's action space and provides useful inapplicability checks.

2. **Shadowed name disambiguation**: When multiple bindings share a name (`let a = 1 in let a = 2`),
   how to disambiguate? Options: `a@1` index suffix, or require selectors for ambiguous cases.
   Related: should *output* syntax include disambiguation annotations so round-tripping works?

3. **Selector-driven edits (concrete proposal)**: Add a new constructor to `Action.Structural.t`:
   ```
   | Selector(selector_action)

   type selector_action =
     | SelectorUpdate(selector_string, code)
     | SelectorDelete(selector_string)
     | SelectorInsert(direction, selector_string, code)
   ```
   This coexists with the existing target-based actions. Implementation plan:
   - Use `Selector.query_unique` for single-match resolution
   - For Update/Delete: selector resolves to an ID, TermEdit replaces/removes at that ID
   - For Insert: selector must resolve to a "spine element" (something with siblings) +
     direction is still needed since selectors don't express Before/After

   **Assessment of path/selector feature completeness (March 2026)**:
   - **Paths alone are NOT sufficient** to replace the `target` enum. Paths resolve to
     binding *nodes* (which contain pattern + definition + body + type annotation).
     The `target` parameter is needed to select which sub-expression to operate on.
   - **Selectors CAN distinguish sub-expressions**: `let x = *` (definition), `let x … in *`
     (body), `* = … in` (pattern). But binding clause and type annotation targeting would
     need new selector patterns.
   - **What works today with selectors**: Update definition, Update body, Delete whole binding
   - **What needs new selector patterns**: pattern targeting, type annotation targeting,
     binding clause targeting (whole let clause)
   - **Insert is the trickiest**: selectors don't express direction (Before/After), and the
     target must be a "spine element" — needs explicit direction parameter alongside selector
   - **Recommended approach**: implement SelectorUpdate and SelectorDelete first (body/definition
     targets only), then progressively add pattern/type-annotation selector patterns

4. **Combined views**: Should GetSyntax/GetStatics/GetDynamics be composable into a single
   annotated view? Or separate calls composed by the agent?

5. **Dead code cleanup**: Segment-level helpers (`introduce`, `~syntax`, `~return` params)
   in CompositionGo are unused. When to remove?
