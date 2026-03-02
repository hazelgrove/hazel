# Notes for Andrew — Clarifications on Compromises

## 1. `should_add_space` change (compromise #1)

The change: removed the rule `starts_with(s2, ~prefix=":") => false` from
`should_add_space` in ExpToSegment.re and replaced the conditional after-`:`
logic with unconditional `true` for both before and after `:`.

**Old rules (broken for Hazel):**
- No space before `:` in all cases → `x:Int` instead of `x : Int`
- After `:`, only add space before `$` or `!` → `x:Int` again

**New rules:**
- Always add space before and after `:` and `::` → `x : Int`, `a :: b`

This only affects AutoFormat mode. The web UI uses PreserveExact (respects
stored whitespace). All Reparse tests pass. The old rules were genuinely
incorrect for Hazel type annotations — they seem like they were written for a
different notation or an early version of the language.

## 2. Whitespace / formatting consistency (compromise #2)

**Is this newly induced or pre-existing?** Pre-existing. The old segment-level
edit path used `Select.term` + `Zipper.insert_segment` which pastes raw segments
— those also don't copy whitespace from neighbors. The previous code never did
contextual whitespace copying. The `copy_secondary` approach was something I
tried during this session and abandoned in favor of AutoFormat.

**What AutoFormat actually produces:** It adds single spaces between tokens based
on `should_add_space` heuristics. So `let a = 1 in let b = 2 in a + b` comes out
with consistent single-space formatting. It does NOT insert newlines — everything
is single-line. If the original program had:
```
let a = 1 in
let b = 2 in
a + b
```
After a TermEdit round-trip, it would become:
```
let a = 1 in let b = 2 in a + b
```
This is because AutoFormat's `mk_form` only adds spaces, never newlines. The
`inline: true` setting in roundtrip_settings reinforces this.

**UPDATE**: Option 1 was implemented. TermEdit now uses `PreserveExact` with
`leading_secondary_of`/`trailing_secondary_of` helpers that walk into compound
forms to extract effective whitespace from leaves. Line breaks are preserved
across edits. The `PreserveWithFallback` variant was removed from ExpToSegment
entirely. 14 whitespace preservation tests verify this behavior.

## 3. "Cascading type changes" / type alias test weakening (compromise #7)

**Specific example of what was happening:**

Original test program:
```hazel
type T = Int in let x : T = 1 in x
```

Edit action: `Update(Definition, "T", "Bool")` — change T from Int to Bool.

Expected result:
```hazel
type T = Bool in let x : T = 1 in x
```

What actually happened: The edit was **rejected** by `validate_edit`'s static
error check. Here's why:
- After changing T to Bool, `let x : T = 1` now means `let x : Bool = 1`
- But `1` has type `Int`, not `Bool` → type inconsistency error
- `static_error_check` sees new errors > old errors → rejects the edit

The test expected the edit to succeed and produce the new code. But the error
gating correctly identified that this introduces a type error.

**What I did:** Changed the test program to `type T = Int in let x = 1 in x`
(removed the `: T` annotation on x). Now changing T to Bool doesn't affect x
at all, so no type error.

**The real question:** Should the agent be allowed to make edits that introduce
type errors in other parts of the program? Currently the answer is "no" — any
edit that increases the error count is rejected. But this is overly conservative
for type alias changes, where you might want to:
1. Change `type T = Int` to `type T = Bool`
2. Then separately update `let x : T = 1` to `let x : T = true`

The current gating forces you to do this as a single atomic edit (change both T
and x in one action), which isn't always possible with the current action set.

## 4. `insert_binding` trailing " in" strip (compromise #6)

**Is this pre-existing?** No, this is new. The old segment-level insert path
(`insert_term` in PerformUtils) used `Select.term` + `Move.by_token` +
`Zipper.insert_segment` — it pasted the raw segment at a cursor position,
so the user's code string went through `Parser.to_segment` directly with no
manipulation.

The new TermEdit approach for `insert_binding` works differently:
1. It needs to parse the code as a full expression to extract `Let(pat, def, _)`
2. So it does `parse_exp(code ++ " in 0")` to make a complete `let ... in ...`
3. Then it extracts `pat` and `def` and builds a new `Let(pat, def, existing_body)`

The problem: if the user writes `"let x = a in"` (with trailing " in"), the parse
becomes `"let x = a in in 0"` which has a double "in" and fails to parse.

The fix: strip trailing ` in` before appending. This is a heuristic but reasonable
since the agent's convention is `Insert(Before, "b", "let x = a in")` — the
trailing " in" is syntactic sugar indicating "this is a binding" but isn't needed
for the TermEdit path since TermEdit constructs the full Let expression itself.

A cleaner alternative would be to change the action API so insert takes
`"let x = a"` (no trailing in) and TermEdit handles the rest. But that's a
breaking API change for the agent prompt.

## 5. Pattern rename generalization

Currently `update_pattern` does two things in one action:
1. Replaces the pattern itself (e.g., `a` → `x`)
2. Renames variable references in scope (e.g., all `a` → `x` in def and body)

This only works for simple `Var` patterns. For tuple patterns like `(a, b)`,
constructor patterns like `Some(x)`, etc., there's no clear "rename" semantics.

Suggestion from you: split into two actions:
- `Update(Pattern, path, code)` → just replaces the whole pattern, no rename
- A new `RenameVariable(path, old_name, new_name)` action → renames variable
  references in scope

This would be cleaner and more general. The pattern update would just be a
structural replacement, and variable renaming would be a separate semantic
operation that works regardless of pattern shape.

## 6. Error gating strategy — RESOLVED

**Resolution**: Implemented "warning instead of rejection" approach.

Static type errors now produce warnings (returned as `option(string)` alongside
the successful zipper) rather than blocking the edit. Parse errors (unmatched
delimiters, Invalid/MultiHole nodes) still block since they indicate genuinely
broken syntax. The warning is stored in `CompositionGo.Public.last_warning` and
included in the agent's tool call success message via Agent.re.

This allows multi-step refactoring: the agent can change `type T = Int` to
`type T = Bool` (which produces a warning about `let x : T = 5` now having a
type error), then fix `x` in a follow-up edit.

The scrutiny system (`edit_action_to_static_error_scrutiny`) is still used to
scope warnings — e.g., `Update(Definition)` only warns about errors in pat+def,
not body — but it no longer gates.
