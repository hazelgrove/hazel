# Coding Agent Actions: Structural Editing & Addressing

**Branch**: `coding-agent-actions` (off `coding-agent`, merged with `dev`)
**Worktree**: `/Users/andrewblinn/.claude-worktrees/hazel/coding-agent-actions/`

## Architecture: Two Addressing Systems

### 1. HighLevelNodeMap paths (edit-oriented)

Slash-delimited paths with multiple addressing modes:

| Syntax | Example | Meaning |
|--------|---------|---------|
| bare name | `"x"`, `"x/y"` | binding named x, nested y |
| `#n` | `"#0"`, `"M/#2"` | nth item (0-indexed) |
| `$` | `"$"`, `"x/$"` | final expression |
| `\|pat` | `"f/\|A"` | case arm by pattern |
| `[n]` | `"xs/[0]"` | list element by index |
| `(n)` | `"p/(0)"` | tuple element by index |

Resolution: `parse_path` -> `resolve_path` walks node tree.
Combined with `target` enum: `Update(Definition|Body|Pattern|BindingClause|TypeAnnotation, path, code)`

### 2. Selector language (query + edit)

See `plans/selector-calculus.md` for the spec and
`plans/selector-rewrite-plan.md` for the resolver rewrite plan.

Tools: `select`, `get_canonical`, `selector_update`, `selector_delete`,
`selector_insert_before`, `selector_insert_after`

All wired into agent API (JSON tool defs + `action_of` dispatch).

---

## Remaining Work

### Non-selector fixes

- **Pattern rename limited to Var patterns**: `pat_var_name` only extracts from
  `Var(n)` and `Asc(Var(n), _)`. Tuple/constructor patterns don't get rename.

- **Module pattern rename doesn't propagate to dot access**: Renaming a module
  item's pattern only affects variable references within the module. `m.a`
  references in outer scope are NOT renamed.

- **SelectorInsert permissiveness**: The binding fallback wraps any expression
  in a Let, even for nonsensical positions. Consider strict mode.

- **Module item insertion whitespace is hardcoded**: Always adds single space.

### Future features

- **Multi-variable selectors**: Multiple focus variables for extracting several
  subexpressions in one pass.
- **Dynamics/probe integration**: `GetDynamics(path)` to return probe/runtime values.
- **Semantic filters**: `@refs(x)`, `@type(Int)`, `@errors` for query-based filtering.

---

## Known Compromises

1. **AutoFormat `should_add_space` change is global**: Modified to always add
   spaces around `:` and `::`. Only affects AutoFormat mode.

2. **TermEdit uses PreserveExact for round-trips**: Resolved. Uses
   `leading_secondary_of`/`trailing_secondary_of` helpers.

3. **`insert_binding` trailing "in" stripping is fragile**: Strips trailing
   ` in` to prevent double-"in" parse errors.

4. **`find_all_lets` uses synthesized EmptyHole body for ModLet**: Contained
   smell — being addressed in selector rewrite.

---

## Key Files

- `CompositionGo.re` — edit_dispatch, read_dispatch, composition_dispatch
- `CompositionActions.re` — action type definitions
- `CompositionUtils.re` — tools list, action_of dispatch
- `Selector.re` — tokenizer, parser, resolver, diagnostics, canonical, deparse
- `HighLevelNodeMap.re` — node map construction, path resolution
- `TermEdit.re` — term-level transformations
- `ToolJsonDefinitions/EditTools.re` — JSON tool defs for edit tools
- `ToolJsonDefinitions/ReadTools.re` — JSON tool defs for read tools
- `Test_AgentTools.re` — comprehensive test suite
- `ActionExplorer.re` — developer UI for interactive action exploration

## Test Running

```bash
cd /Users/andrewblinn/.claude-worktrees/hazel/coding-agent-actions
dune build
node --stack-size=16000 _build/default/test/haz3ltest.bc.js test 'AgentTools' --show-errors
```
