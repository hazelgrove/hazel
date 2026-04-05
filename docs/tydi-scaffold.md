# TyDi Scaffold: Type-Directed Tuple Completion

## Overview

When typing a function application or tuple literal in Hazel, the scaffold  
system anticipates the tuple structure based on type infofrmation and shows  
ghost commas and hole placeholders. This eliminates misleading type errors  
during left-to-right typing and provides per-element type expectations.

**Example:** Given `let f : (Int, String) -> Bool`, typing `f(1` shows:

```
f(1¦⟨, ?⟩
```

The ghost `, ?` tells the user: there's one more argument expected (String).
Statics sees `Ap(f, Tuple([1, ?]))` instead of `Ap(f, 1)`, so `1` gets
`ana = Int` (correct) instead of `ana = (Int, String)` (wrong, produces error).

## Core Concepts

### 1. Scaffold = ghost commas + holes

The scaffold is a buffer of structural pieces — real Tile pieces for commas
and label operators, real Grout pieces for holes, and Whitespace secondaries
for formatting. It's stored in the zipper's selection as a `Buffer(Unparsed)`.

### 2. Reification = making statics see it

Before statics runs, `TyDiScaffold.reify` splices the scaffold into a scratch
copy of the zipper. The commas become real tuple separators, the holes become
proper empty expressions. MakeTerm and Statics then operate on this modified
term. The real zipper (what the user edits) is unchanged.

### 3. Completion + scaffold = one decision

The scaffold and text completion are computed together in `TyDi.suggest_assist`.
When typing `ar` inside `string_replace(`, the system considers both:

- `arg : String` matches the first element type → show `g` + scaffold `, ?, ?`
- `args : (String,String,String)` matches the full Prod → show `gs` alone

The best candidate wins. Element-type matches with scaffold are preferred over
full-Prod matches when the element completion is shorter (more specific).

## Worked Examples

### A. Basic two-argument function

```
let f : (Int, String) -> Int = fun x -> 0 in f(¦
```

1. Scaffold detects: inside parens, expected type `Prod([Int, String])`,
  0 existing commas, arity 2 → remaining = 1.
2. Left boundary is empty (just `(`) → `holes_first = true`.
3. Scaffold: `[Grout(?), Tile(,), Whitespace( )]` → display `?,` 
4. Statics reifies: sees `Ap(f, Tuple([?, ?]))` → per-element types Int, String.

After typing `1`:

```
f(1¦⟨, ?⟩
```

- `holes_first = false` (content to left), scaffold: `[Tile(,), Whitespace( ), Grout(?)]`
- Display: `, ?`

After Tab:

```
f(1, ¦?
```

- `scaffold_emit_text` extracts `,`  (comma + trailing space)
- `Parser.to_zipper` inserts it, regrout creates hole
- Scaffold regenerates: 1 existing comma, remaining = 0 → no scaffold

### B. Three-argument function with progressive Tab

```
let g : (Int, String, Bool) -> Int = fun x -> 0 in g(¦
```

Display: `?, ?,`  (3 elements, holes_first)

```
g(1¦    →  scaffold: ", ?, ?"
Tab     →  g(1, ¦?     scaffold: ", ?"
2       →  g(1, 2¦     scaffold: ", ?"
Tab     →  g(1, 2, ¦?  scaffold: None (all commas placed)
```

### C. Suppression: variable satisfies the full Prod

```
let f : (Int, String) -> Int = fun x -> 0 in
let p : (Int, String) = (1, "a") in
f(p¦
```

`should_suppress` checks: `p` is convex, its synthesized type is
`(Int, String)`, which is consistent with the expected `Prod([Int, String])`.
Result: no scaffold. This works even in multi-line contexts — `should_suppress`
skips whitespace/grout to find the nearest convex piece.

### D. Element-type completion

```
let arg : String = ? in string_replace(ar¦
```

1. Scaffold applies: expected `Prod([String, String, String])`, remaining = 2.
2. `element_context` computes: element index 0, element type = `String`.
3. `completion_suffix(~ci=element_ci)` finds `arg : String` → suffix `"g"`.
4. Combined: `"g"` + scaffold `, ?, ?` → display `g, ?, ?`.

### E. Completion that satisfies full Prod (no scaffold)

```
let blargs : (String, String, String) = ? in string_replace(bl¦
```

1. Element completion (`String`): no match for `bl` prefix.
2. Full-Prod completion (`(String,String,String)`): finds `blargs` → suffix `"args"`.
3. `full_would_suppress("args")`: checks if `blargs` satisfies the full Prod → yes.
4. Result: `"args"` only, no scaffold.

### F. Exact match suppression

```
let args : (String,String,String) = ? in
let arg : String = ? in
string_replace(arg¦
```

1. Element completion: `arg` is an exact match at `String` → suppressed.
2. Full-Prod completion: `args` matches → suffix `"s"`.
3. But `element_suppressed = true` (token `arg` is valid at element type `String`).
4. Result: scaffold only `, ?, ?`. The user has typed a complete valid element.

### G. Form completion combined with scaffold

```
let g : (Int, String, Bool) -> Int = fun x -> 0 in g(1111, tr¦
```

1. Element type = `String` (index 1). `true : Bool` doesn't match `String`.
2. Full-Prod: `true` found via `TyDiForms.suggest_operand`.
3. `full_would_suppress("ue")`: checks `true` against `Prod([Int,String,Bool])` → no.
4. Combined: `"ue"` + scaffold `, ?` → display `ue, ?`.

### H. Labeled tuples

```
let f : (x=Int, y=String) -> Bool = fun a -> true in f(1¦)
```

Scaffold: `, y=?` — the label `y=` appears before the hole.

```
f(¦    →  scaffold: "x=?, "  (holes_first with label)
Tab    →  f(x=¦?             (emits label prefix "x=")
1      →  f(x=1¦)            scaffold: ", y=?"
Tab    →  f(x=1, ¦?)         (emits ", ")
```

### I. Nested tuples

```
let h : ((Int, Bool), String) -> Float = fun x -> 0.0 in h((1¦
```

Inner paren context: expected `Prod([Int, Bool])`, scaffold `, ?`.

```
h((1, true)¦    →  outer scaffold: ", ?"  (outer Prod element)
Tab             →  h((1, true), ¦?
```

When inner parens close, scaffold regenerates for the outer context.
`expected_type` handles this via `find_next_paren`: if the inner `(`
has Unknown ana, it falls back to the outer `(` and indexes into its
Prod by counting commas between them.

### J. Multi-line function application

```
let f : (Int, String) -> Int = fun x -> 0 in
f(
  1¦
```

The `(` is on a different line. `inside_parens` finds it by scanning all
left siblings (not just immediate). `inner_left_siblings` walks left,
skipping whitespace, until it hits the `(` shard. Scaffold works normally:
`, ?`.

### K. Formatting: space after user-typed comma

```
string_replace("",¦
```

User typed comma without trailing space. `left_needs_space` detects bare
comma as immediate left neighbor → prepends `Whitespace(" ")` to scaffold.
Display: `?,` (space + hole + comma + space). On Tab, the space is
emitted first as a formatting fixup, then scaffold regenerates.

## Architecture

### Module structure

```
TyDi.re                    — Public API: suggest_assist + re-exports
  ├── TyDiComplete.re      — Text completion: suggest, set_buffer, suffix_of
  ├── TyDiScaffold.re      — Scaffold: display, reify, set, paren detection
  ├── TyDiCtx.re           — Context search: bound_variables, suggest_variable
  ├── TyDiForms.re         — Form/keyword suggestions
  └── TyDiSuggestion.re    — Suggestion data type
Buffer.re                  — Buffer mechanics: set_assist_buffer, accept, clear
CachedStatics.re           — init_with_assist (two-pass statics resolution)
CodeWithStatics.re         — Editor integration: statics refresh, debounce
```

### The two-pass statics resolution

`CachedStatics.init_with_assist` resolves the circular dependency between
buffer computation (needs info_map) and statics (needs buffer for reification):

1. Run statics on bare zipper → `info_map`
2. Compute buffer using `info_map` via `suggest_assist`
3. If buffer has scaffold → re-run statics with reification

The second pass only runs when scaffold is present. For text-only completions,
one pass suffices.

### Fast preview vs. authoritative result

`Editor.calculate` sets the buffer using old (previous-cycle) statics for
immediate visual feedback. `CodeWithStatics` then runs `init_with_assist`
with fresh statics to produce the authoritative buffer and statics. If the
authoritative result differs (e.g., suppression kicks in with fresh type info),
the buffer is updated.

### Progressive Tab acceptance

Each Tab press emits one "chunk" from the scaffold:

- A formatting space (when caret follows bare comma)
- A label prefix like `x=`
- A comma + trailing space like `,` 

The buffer is cleared after emission. Next cycle, scaffold regenerates
with fewer remaining elements. This continues until all commas are placed.

## Appendix: Decision Points

### Why structural pieces instead of text?

The original plan used text buffers (`", ○"`) with `○` as hole placeholder.
The implementation evolved to use real Piece values because:

- Grout pieces have proper convex shape, avoiding shape conflicts
- Tile pieces for commas have correct molds for Skel processing
- `reify` can splice structural pieces directly without re-parsing
- `insertable` can extract formatting-aware text for acceptance

### Why not reify text completions?

Currently only scaffold buffers (containing Tile/Grout) are reified. Text
completions (e.g., `"args"` for `bl→blargs`) are not virtually inserted
for statics. This means statics sees `bl` (with type errors) rather than
`blargs` (correct). Reifying all completions would eliminate these transient
errors but requires running `Parser.to_zipper` on the completion text for
the scratch zipper. This is a potential future improvement.

### Why element-type decomposition instead of reification for completion ranking?

When deciding between `arg` (element type) and `args` (full Prod type),
we decompose the Prod type locally (`List.nth(tys, element_index)`) rather
than reifying the scaffold and re-running statics. This avoids the cost of
a full statics pass just to determine which completion to show. The per-element
type is sufficient for ranking; the statics re-run only happens once, after
the winning candidate is chosen.

### Bare tuples excluded

Scaffold only triggers inside parentheses (function application or explicit
tuple parens). `let t : (Int, Bool) = 1¦` does NOT get scaffold — there's
no paren context to detect tuple position. This is a deliberate limitation:
bare tuples are syntactically ambiguous without parens.

## Appendix: Known Limitations

1. **Pattern ancestor case**: `let (¦) : (Int, Bool)` — caret is `Inner(0)`
  on the `(` delimiter, not `Outer` inside the paren child. Scaffold bails
   early. Fixing requires virtual-move or buffer-set-from-caret-info.
2. **token_to_left precision**: `token_to_left` only checks the immediate
  left piece. If whitespace intervenes (e.g., caret at start of line after
   linebreak), no completion is generated. Scaffold-only still works since
   it doesn't depend on `token_to_left`.

