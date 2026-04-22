# Positional Tuple Access and Stuck Tuple Destructuring

Two related features added in `stuck-pattern-projection`:

- **Feature A — positional tuple access**: `x.0`, `x.1`, …, `x.n` projects the
  n-th field of a tuple (labeled or unlabeled).
- **Feature B — stuck tuple destructuring**: `let (a, b, …) = SCRUT in body`
  (and `(fun (a, b, …) -> body)(SCRUT)`) no longer gets stuck when SCRUT is
  indeterminate; it rewrites to a parallel tuple of positional projections
  and continues.

Both features are **scoped to tuples only** (irrefutable patterns). Refutable
patterns (`Cons`, `[a, b, …]`, constructor `C(x)`, atomic `42`) keep the
prior stuck-on-indet behavior.

## Positional dot access

```
(1, 2, 3).0              // => 1
(1, 2, 3).1              // => 2
(1, 2, 3).2              // => 3
(a=1, b=2).0             // => 1  — labeled tuples support positional access
(a=1, b=2).a             // => 1  — labeled access still works
((1, 2), 3).1            // => 3
(((1,2),3).0).1          // => 2  — chained access
```

Design choices:

- **Reuse the `.` infix**. The right-hand side's sort (label vs int literal)
  decides which projection semantics apply. Labels and ints are
  syntactically disjoint (`Token.var_regexp` vs `Token.int_regexp`), so
  there is no ambiguity.
- **Labeled tuples allow positional access**. `(a=1, b=2).0` is `1`. The
  label is ignored by the positional lookup; the underlying element is
  unwrapped from its `TupLabel` wrapper. Users can mix styles freely.
- **List-of-tuples maps positionally**, mirroring the existing labeled
  behavior: `[(1, 2), (3, 4)].0` is `[1, 3]`.
- **Out-of-bounds mirrors `LabelNotFound`**. Statically: a
  `TupleIndexOutOfBounds(idx, len)` mark. Dynamically: stays indet
  (`(1, 2).5` stays as `(1, 2).5`). This matches labeled behavior
  (`(a=1,b=2).c` behaves identically) so the gradual-editing story is
  symmetric — an OOB expression can become well-typed by extending the
  tuple's type, without the dynamics crashing in the meantime.
- **Negative indices are malformed**. `x.(-1)` is parsed as
  `Dot(x, UnaryMinus(Atom(Int(1))))` — the RHS is not an `Atom(Int)`, so it
  falls through to the `BadLabel` mark. No reverse-indexing support.
- **`x.(0)` is sugar for `x.0`**. `MakeTerm` strips `Parens(Atom(Int(_)))`
  in the Dot RHS so the elaborator and dynamics see the plain int form.
  The `x.(0)` form is useful in chains, see "Known parse limitation" below.

### Implementation

- `MakeTerm.re`: added two new RHS branches for `.` — plain `Atom(Int(_))`
  passes through; `Parens({term: Atom(Int(_)), …})` strips the parens.
  Anything else still falls through to the existing `MultiHole` wrapping.
- `Unboxing.re`: new `TuplePositional(int)` request. Mirrors
  `LabeledTupleProjection` — returns `Matches(expr)` when the input is a
  `Tuple` of at least `i+1` elements, `IndetMatch` otherwise, and
  `Matches(expr)` for list-of-tuples to let Transition.re do the mapping.
- `Transition.re` Dot case: new `Atom(Int(i))` RHS branch parallel to the
  existing `Label(name)` branch. Extracts `List.nth(ds, i)`, strips
  `TupLabel` wrapper if present, steps to the element. List-of-tuples maps
  `Dot(d, Int(i))` over elements.
- `Statics.re` Dot case: new `Atom(Int(idx))` branch in both the `Prod(ts)`
  and `List(Prod(ts))` type cases. Bounds-checked against `List.length(ts)`.
- `Mark.re`: new `TupleIndexOutOfBounds(int, int)` constructor carrying
  `(index, length)`. Wired into `CursorInspector.re` and
  `haz3lcore/TyDi/ErrorPrint.re`.

### Float disambiguation: `chained_dot_edge_case`

The existing float tokenizer regex accepts `0.0`, `0.1`, etc. as float
literals, which creates an ambiguity when chaining positional access with
literal ints: naively, `x.0.1` would greedy-merge `.0.1` into the float
`0.1` and Dot would see a float on its RHS.

Fix: added a context-aware merge guard `chained_dot_edge_case` in
`src/haz3lcore/zipper/action/Insert.re`. When inserting `.`, if the left
sibling is a pure-int tile and the next non-secondary tile to its left is
a `.` operator, the left-append merge is blocked and the `.` becomes its
own tile. Typing `0.5` from scratch is unaffected because there's no
preceding `.` operator tile.

With this fix, `((1, 2), 3).0.1` parses cleanly as `Dot(Dot(..., 0), 1)`.
No workaround needed. Tests cover both the new direct form and the
previously-used workarounds (`(x.0).1`, space-separated `x . 0 . 1`) so
all three remain valid.

## Stuck tuple destructuring

### Before

```
let (a, b) = ? in 1 + 1 + a
→ [stuck — entire Let is Indet]
```

### After

```
let (a, b) = ? in 1 + 1 + a
   ↓ StuckDestructure     -- one new step, def is rewritten
let (a, b) = (?.0, ?.1) in 1 + 1 + a
   ↓ Let                  -- matches succeeds normally now
body[a := ?.0, b := ?.1]
→ 1 + 1 + (?.0)
→ 2 + (?.0)
→ [stuck, but at the smallest sub-expression]
```

### Scope rule: irrefutable tuple patterns with at least one bound var

The rewrite fires when **all three** conditions hold:

1. `Pat.is_irrefutable(dp)` — no refutable sub-patterns anywhere.
   Recursively: `Var`, `EmptyHole`, `Wild`, and the transparent wrappers
   `Parens`, `Projector`, `Asc`, `TupLabel` (over irrefutable children),
   plus `Tuple(ps)` when every `p ∈ ps` is irrefutable.
2. `Pat.contains_tuple(dp)` — otherwise the pattern is a bare `Var` (which
   already matches anything at the `Matches` path) or a bare `Wild` (no
   variables to bind, no match failure to avoid).
3. `Pat.bound_vars(dp) != []` — at least one variable is being introduced.
   Without this, the rewrite would silently skip a pure refutability check
   like `let () = A in 1` or `let (_, _) = ? in 1`, and those cases must
   stay stuck.

Rationale for (3): the empty-tuple pattern `()` matches only unit values.
If we rewrote `let () = A in body` to `let () = () in body`, we'd lose the
runtime check that `A` is a unit. That's both semantically wrong and
observably incorrect vs. existing tests. Requiring at least one binding
limits the feature to cases where the user clearly wants to read values
out, not just assert a shape.

### Scope rule: skip Closure scrutinees

The scrutinee is expected to already be in final form (it has been through
`req_final`). When that final form is a `Closure(env, inner)`, the rewrite
is skipped:

```
| Closure(_) => Indet
| _ => ... rewrite ...
```

Why: `pat_proj(dp, d1')` produces a `Tuple([Dot(d1', 0), Dot(d1', 1), …])`
— N syntactic copies of `d1'`. When `d1'` is a plain indet (a hole, an
applied builtin with an indet arg, etc.), evaluating each `Dot` is free
because the inner scrutinee is already final and doesn't step further.
But when `d1'` is a `Closure` wrapping an indet computation, each `Dot`'s
own `req_final` call re-enters the closure and traverses its body.
Probes inside the closure then fire *once per duplicate dot*, yielding N×
the expected sample count.

Detected by the recursive-indet / wrap_closure tests in
`Test_Evaluator_Probes.duplicate_prevention_tests`. With the Closure
skip, all existing probe-dedup tests pass unchanged.

Trade-off: a Let whose scrutinee is a Closure stays stuck even when
destructure would have worked. In practice this shows up most often when
the scrutinee is a recursive call whose result is still being computed
lazily. Users who hit this can force the scrutinee to finalize first with
an outer `let`. See the "Unexpected observations" section of
`positional-dot-and-stuck-destructure-surprises.md` for the full detail.

### The rewrite: `pat_proj`

Pattern-directed walk that at each `Tuple(n)` level of the pattern produces
a parallel `Tuple(n)` of `Dot(scrut, i)` projections; everywhere else it
leaves the scrut unchanged:

```
pat_proj(Var(_) | EmptyHole | Wild, d)             = d
pat_proj(Parens(p) | Projector(_, p)
       | Asc(p, _) | TupLabel(_, p), d)            = pat_proj(p, d)
pat_proj(Tuple(ps), d)                             =
  Tuple(List.mapi((i, p) => pat_proj(p, Dot(d, Int(i))), ps))
```

Output property: `matches(dp, pat_proj(dp, d1'))` is guaranteed to succeed
for any irrefutable tuple pattern `dp` — the pattern's Tuple skeleton meets
a literal Tuple of the same arity at every level, and leaves bind to
deep-`Dot` chains.

### FunAp gets the same treatment

```
(fun (a, b) -> a + b)(?)
→ (fun (a, b) -> a + b)((?.0, ?.1))    -- StuckDestructure on the argument
→ a + b  [with a := ?.0, b := ?.1]
→ ?.0 + ?.1
```

Same gating rules apply: pattern must be irrefutable + contain a Tuple +
bind at least one variable; argument value must not be a Closure. Applied
to both `FunEnv` and `FunNoEnv` branches of the `Ap` case.

`FixF` is left alone. It already does per-variable
`let dp = d1 in var(v)` expansion in its own path when the pattern isn't a
single variable (see `Transition.re` around the `FixF` case), so the
existing mechanism covers the recursive-function case.

`Match` is also left alone — deliberately. A Match has alternatives, and
opportunistically committing to the first rule's pattern when the
scrutinee is indet would pick the wrong branch in general. Staying stuck
is correct.

### Step kind and probe recording

New `StuckDestructure` step kind. `side_effects: []` — no
`RecordPatProbes` is fired, because the scrutinee didn't actually match
against a value; the probe would be recording projection expressions, not
concrete bindings. If we later want to capture that a destructure happened
(e.g., for explaining stepper behavior), a dedicated sample kind can be
added without changing the core rewrite.

## Test coverage

- `test/evaluator/Test_Evaluator_TupleIndex.re` — 32 tests: positional
  access at various arities, nested access (with parse workarounds for the
  float-lex issue), mixed labeled/positional, list-of-tuples mapping,
  arithmetic composition, out-of-bounds indet, ascribed bindings,
  parenthesized `x.(0)` form.
- `test/evaluator/Test_Evaluator_StuckLet.re` — 36 tests: existing
  irrefutable-let regressions, pair/triple/nested stuck destructure,
  labeled-tuple destructure, ascribed-slot destructure, wild-slot
  destructure, hole-scrutinee through var/fun bindings, FunAp parameter
  destructure, refutable-pattern stays-stuck regressions
  (Cons/ListLit/Mixed), definitively-failing matches stay stuck,
  empty-tuple/all-wild patterns stay stuck.

Full test suite: 2561 tests, 0 failures.
