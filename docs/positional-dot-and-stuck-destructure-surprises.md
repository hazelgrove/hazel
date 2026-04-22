# Development Surprises — Positional Dot + Stuck Destructure

Things that were not as anticipated during implementation. Roughly ordered
by when they surfaced.

## 1. `.0` and `.1` lex as floats when chained after a dot

**Expected**: `((1, 2), 3).0.1` would parse as `Dot(Dot(..., 0), 1)`,
giving 2 because `.` is max-munch-broken by the left operand.

**Actual**: Tile insertion is greedy. After typing `x.0`, typing another
`.` would check `sibling_appendability`: the left sibling is the int
tile `0`, and `Token.append("0", ".") == "0."` matches `is_potential_operand`
(via the `[0-9_]+\.[…]*` float-prefix alternative). The merge fires,
producing `0.`. Next char `1` merges again to `0.1`, the complete float.
The Dot op then sees a float on its RHS and marks `BadLabel`.

**Initial workarounds** (now no longer needed): space-separate (`x . 0 . 1`)
or outer-parenthesize (`(x.0).1`). The `x.(0).1` form does **not** work
— `.(0).1` reads as function application of `x.0` to `1`.

**Fix taken**: added `chained_dot_edge_case` in
`src/haz3lcore/zipper/action/Insert.re`, a context-aware guard on
`sibling_appendability`. When inserting `.`, if (a) the left sibling is
a pure-int tile and (b) the next non-secondary tile to its left is a `.`
operator tile, the merge is blocked and the new `.` becomes its own tile.

Typing `0.5` from scratch is unaffected — when the user types the first
`.`, there's no preceding `.` operator tile, so the guard doesn't fire
and the merge proceeds as before to form the float.

After the fix, `((1, 2), 3).0.1` parses directly as
`Dot(Dot(..., 0), 1)`. The two workarounds also still work. All three
forms have passing tests.

## 2. Parser wraps unexpected Dot RHS in `MultiHole`, not just Atom

**Expected**: A Dot with an Atom(Int) RHS would parse to `Dot(l, Int(…))`
with the Int sitting on the RHS directly.

**Actual**: `MakeTerm.re` wraps any non-`Var`/`Constructor`/`Label`/
`EmptyHole` RHS in `MultiHole([Exp(…)])` before constructing the `Dot`.
This is a correctness-preserving sort mismatch: the parse molds for the
Dot RHS expect a label-sort operand, so integers get wrapped as "wrong
sort, but preserved for editor use".

**Fix**: add `Atom(Int(_))` (and a narrow `Parens({term: Atom(Int(_)),
…})` for the `x.(0)` form) as explicit passthrough cases in `MakeTerm`.

## 3. The `x.(0)` form collides with function application if chained

**Expected**: `x.(0).(1)` to be a readable "explicit-index" chained
positional access. If `.0.1` is float-ambiguous, `.(0).(1)` seemed like
the natural escape hatch.

**Actual**: `x.(0).(1)` parses as `Ap(Forward, Dot(x, 0), 1)` — the
second `.(…)` is read as an Ap-parens applied to `x.0`. The precedence
breakdown of `.` + function-application puts `.()` into the application
category when no operator separates them.

**Fix**: use outer parens instead (`(x.0).1`). Documented in
`Test_Evaluator_TupleIndex.re`.

## 4. Broad `Parens(_)` passthrough in MakeTerm broke MenhirParser fuzzer

**Expected**: Allowing `Parens(_)` through as a Dot RHS (with a dynamics
fallback that peeks inside) would handle arbitrary parenthesized
expressions without harming anything else.

**Actual**: `MenhirParser` fuzz test caught `0 . ((()))` diverging
between the Menhir and MakeTerm parsers after this change. The Menhir
path still wraps Parens in the failure handling, so the two parsers
produce different terms.

**Fix**: narrow the passthrough to exactly `Parens({term: Atom(Int(_)),
…})` — only the positional-access sugar case. Other parenthesized
expressions fall through to the original `MultiHole` wrapping, matching
Menhir. The dynamics branch for generic `Parens` was also removed since
MakeTerm no longer produces it.

## 5. Empty-tuple patterns are irrefutable but shouldn't destructure

**Expected**: `Pat.is_irrefutable_tuple_pattern` = "irrefutable AND
contains a tuple" was the natural gate. This treats `()` as
irrefutable-tuple (it is both — `Tuple([])` has no children to refute).

**Actual**: `let () = A in ?` has existing semantics — the empty tuple
pattern fails to match on a non-unit value, whole Let stays Indet. My
rewrite turned this into `let () = () in ?` which matched and let the
body run. This broke `Test_Evaluator_Sum_Types.SumTypes 4 "Indet when
unboxing constructor as tuple"`.

**Fix**: add a third gating condition — `Pat.bound_vars(dp) != []`. If
the pattern has no variables to bind, the rewrite adds nothing; the
pattern's only purpose is refutability (shape check), and that
refutability must stay observable.

Also covers `let (_, _) = ? in 1` and similar all-wild patterns.

## 6. Duplicated scrutinee re-traverses Closures → spurious probe samples

**Expected**: `pat_proj(Tuple([Var a, Var b]), d)` produces
`Tuple([Dot(d, 0), Dot(d, 1)])` — two syntactic copies of `d`. Since
`d` is already `req_final`'d, I expected evaluating the two Dots to be
cheap and non-observable beyond the syntactic bloat.

**Actual**: when `d` is a `Closure(env, inner)` (i.e., the result of a
stuck recursive call), each Dot's own `req_final` re-enters the closure
and traverses `inner`. Any probes inside `inner` fire *once per copy*.
For the `Test_Evaluator_Probes.duplicate_prevention_tests`
recursive-indet tests this quadrupled the sample counts — 4 and 10
samples where 1 was expected under the old stuck-let semantics. The
new semantics "does let more probes fire" is correct; the *duplication
factor* was not.

**Fix attempted**: sharing the scrutinee via an outer `let _share = d
in let pat = (_share.0, _share.1) in body`. Rejected because
substitution unfolds `_share` back to `d` before the inner Let
matches, reverting to duplication.

**Fix taken**: gate the rewrite to skip when `d1'` is a `Closure`.
All existing probe-dedup tests pass unchanged. Users hit this guard
when destructuring the result of a stuck recursive call; they can
work around it with an outer `let x = recursive_call() in let (a, b)
= x in body`, which finalizes the call's result into a non-Closure
form before the destructure. The underlying closure-re-entry issue is
not specific to this feature — it's a general artifact of
`req_final` walking through `Closure` wrappers — but it's deferred as
outside the scope of stuck-destructure.

## 7. Ascriptions inside patterns propagate to bound values

**Expected**: `let (a: Int, b) = ? in a` → `?.0` (plain positional
projection).

**Actual**: `?.0 : Int` — the ascription from the pattern is preserved on
the bound value by `PatternMatch.re`'s existing `Asc(p, t1) => recur(p,
Ascriptions.transition_multiple(Asc(d, t1) |> DHExp.fresh))` rule. My
`pat_proj` correctly descends past the `Asc` to produce `?.0` for the
scrutinee, but then the regular match step re-wraps the bound value in
`Asc` per the pattern's type.

**Not a bug**. Existing Hazel behavior, consistent with how labeled
tuples handle type-checked fields. Test expectation was adjusted from
`"?.0"` to `"?.0 : Int"` and a companion test verifies unascribed slots
stay bare.

## 8. Constructor payload-type metadata fails `Exp.equal` unless ignored

**Expected**: `parse_and_evaluate_test("let Cons(h, t) = ? in h", "let
Cons(h, t) = ? in h")` — the expected and the actual are the exact same
string, so they should compare equal.

**Actual**: the `parse_and_evaluate_test` prelude parses the expected
string *without* elaboration and compares to the elaborated+evaluated
actual. Elaboration attaches optional type info to constructors
(`Constructor("Cons", None)` vs `Constructor("Cons", Some None)` vs
`Some(Some(ty))`), so the two sides don't compare structurally equal.

**Fix**: pass `~ignore_constructor_types=true` to
`parse_and_evaluate_test` for tests that use constructor patterns.
Already a supported flag on the existing prelude.

## 9. MultiHole handling was stricter than expected

When a user writes `x.0` directly in the editor, the expected parse path
was `Var("x") . Atom(Int(0))`. Before my MakeTerm patch, this came
through as `Dot(Var("x"), MultiHole([Exp(Atom(Int(0)))]))`. The Int is
preserved inside a MultiHole marking "sort mismatch for this position."
The existing `Dot` statics has a `BadLabel(Exp(…))` fallthrough that
would trigger on this, but only because `_` catches MultiHole, not
because the statics was explicitly MultiHole-aware.

Understanding this disambiguation took some reading of `MakeTerm.re` to
realize that sort-incorrect tokens are preserved inside MultiHole rather
than being replaced with a hole.
