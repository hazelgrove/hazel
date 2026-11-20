# Parameterized Types

This document tracks the design and implementation of parameterized types in Hazel.

## Surface Goal

Hazel accepts declarations such as:

```hazel
type Option(a) =
  + None
  + Some(a)
in
let x : Option(Int) = Some(3) in
...
```

and multi-parameter declarations:

```hazel
type Either(a, b) =
  + A(a)
  + B(b)
in
let x : Either(Int, Bool) = A(3) in
...
```

Each parameter has kind `Type`. A declaration with `n` parameters introduces
a type constructor of *tuple-arrow* kind `(Type, ..., Type) -> Type`.
Applying that constructor to its `n` type arguments at once produces an
ordinary type. Tuple-arrow kinds are atomic: they admit neither partial
application nor over-application of a multi-param constructor.

- `Either(Int, Bool)` — well-kinded, becomes a regular `Type`.
- `Either(Int)` — rejected as `expects 2 arguments, got 1`.
- `List(Int, Bool)` (for a 1-parameter `List`) — rejected as
  `expects 1 argument, got 2`.
- `Either((Int, Bool))` — extra parens make `(Int, Bool)` a single
  tuple-typed argument, so this is also a 1-of-2 arity error.
- `List((Int, Bool))` — a list of pairs (single-arg application whose
  argument is a `Prod`).

Recursive aliases remain implicit, matching existing Hazel type aliases:

```hazel
type List(a) =
  + Nil
  + Cons(a, List(a))
in
...
```

Recursion is fully supported, including non-uniform recursion such
as `type List(a) = + Nil + Cons(a, List((Int, a)))` (see *Higher-kinded
recursive types* below).

### Type-level type functions (`typfun`) as alias bodies

The prefix-binder form `type T(a, b, …) = body` has an equivalent spelling
using a type-level `typfun`:

```hazel
type Option = typfun a -> + None + Some(a) in
type Either = typfun a, b -> + Left(a) + Right(b) in
type List = typfun a -> + Nil + Cons(a, List(a)) in
```

`typfun a -> body` introduces a `TypFun(a, body)` in the type language. In
alias-head position, a single `typfun` (multi-binder or otherwise) is
collapsed into the corresponding `Param(head, params)` form so it elaborates
the same way as the prefix-binder syntax — same `(Type, …) -> Type` kind,
same polymorphic constructor schemas.

A *curried* alias body like `typfun a -> typfun b -> body` is **not**
collapsed: each unary `TypFun` stays as its own binder so the alias has the
curried kind `Type -> Type -> kind(body)` and accepts curried applications
`T(a)(b)`. Partial applications like `T(String) :: Type -> Type` are
well-kinded and may themselves be used as alias bodies.

The `TypFun` form is meaningful only as the body of a `type` declaration;
higher-kinded type parameters (e.g. `type Functor(f : Type -> Type) = …`)
are not part of this implementation.

## Naming

| Concept | AST | User-facing |
|---|---|---|
| Type-level: parameterized type applied to args — `Either(Int, Bool)` | `Typ.TypParamAp(typ_t, typ_t)` | "Type parameter application" |
| Multi-arg bundle inside a `TypParamAp` | `Typ.TypTuple(list(typ_t))` | "Type parameter argument tuple" |
| Type-level type function — `typfun a -> body` | `Typ.TypFun(tpat_t, typ_t)` | "Type-level function" |

`TypTuple` only ever appears as the second argument of a `TypParamAp`. It
has no kind on its own; its elements must match the callee's tuple-arrow
kind.

## Parsing & disambiguation

Source-level `T(args)` is always a `Typ.TypParamAp(T, arg)` at the AST. The
disambiguation between "multi-arg" and "single-arg-tuple" is in the *shape*
of the argument:

- `T(a, b)` → `TypParamAp(T, TypTuple([a, b]))` — multi-argument application.
- `T((a, b))` → `TypParamAp(T, Prod([a, b]))` — single argument that is a
  tuple type. (Hazel's structured editor preserves the user's extra parens
  by producing a `Parens(Prod([…]))`-shaped tile that the term builder
  unwraps to `Prod`; the `TypTuple` form is *only* produced when the comma
  list sits directly under `T(...)` without extra wrapping.)
- `T(a)` → `TypParamAp(T, a)` — single argument, not in a `TypTuple`.

`MakeTerm.apply_typ_param_args` and the Menhir parser implement this lift;
the sum-variant extractor reverses it locally when a `Cons(a, b)` shape
sits in a sum-type position (so a constructor variant with a tuple payload
remains a `Prod`, not a `TypTuple`).

## Kinds

```ocaml
type TypKind.t =
  | Unknown
  | Type
  | Arrow(list(t), t);
```

`Arrow([k1, …, kN], r)` renders as `(k1, …, kN) -> r` (single-arg as
`k -> r`). `Unknown` renders as `?` and is the kind assigned to unbound
type variables and unknown types — it propagates consistently like
`Typ.Unknown`, so a free `L` in `Cons(a, L(a))` produces only one error
(the free-variable mark on `L`) instead of also erroring on the surrounding
application.

`TypKind.apply_all` consumes an entire argument list at once against a
single `Arrow`, so a multi-binder `T : (k1, …, kN) -> R` is atomic. A
*curried* arrow `T : Type -> Type -> Type = Arrow([Type], Arrow([Type],
Type))` consumes one argument per `TypParamAp` node, with the partial
application's residual kind exposed to the next outer site. Both helpers
absorb `Unknown` callees: applying any args to `Unknown` yields `Unknown`.

Kind comparison uses `TypKind.consistent` (similar to `Typ.is_consistent`):
two kinds are consistent if `Unknown` could be refined to make them match.

The `Mark.TypParamApplyArityMismatch` is emitted by `Statics.status_for_node`
when the argument count differs from the kind's arity at a `TypParamAp` site.
Its message — which includes the callee type pretty-printed — reads:

```
`Either` expects 2 arguments, got 1
```

`Mark.TypParamApplyNonArrowKind` is emitted when the callee has kind `Type`
(not an arrow at all).

The `TypExpectation.AnyKindExpected` variant distinguishes alias-body
positions (where any kind is fine — the alias inherits the body's kind)
from value-annotation positions (where the kind must be `Type`).
`Statics.TyAlias` passes `AnyKindExpected` so curried alias bodies and
partial parameterized applications used as alias bodies don't get spurious
`expected Type, got Type -> Type` marks.

The kind of a type expression is computed by the top-level
`Statics.kind_of_typ`, which is the single source of truth: `TyAlias` calls
it once on the alias body and threads the result through `Ctx.extend_alias`
(so downstream `Var(name)` references see the right kind via
`Ctx.lookup_tvar_typ_kind`) and `utpat_to_info_map`'s `~alias_kind`
parameter (so the cursor inspector at the alias's name shows the matching
kind).

## Schemas of parameterized constructors

Sum constructors from parameterized declarations are registered with
polymorphic schemas. For `Either(a, b)`:

- `A` has schema `poly a, b -> a -> Either(a, b)`
- `B` has schema `poly a, b -> b -> Either(a, b)`

Multi-binder schemas use a single `Poly` whose binder is a
`TPat.Tuple([a, b, …])` — *not* a curried chain of single `Poly`s.
Substitution and equality treat the binder list element-wise via
`TPat.binders_of`, which returns `[tpat]` for a single binder and the
elements of a `Tuple`. The `TypParamAp` reduction zips a `TypTuple`
argument against the tuple binder element-wise in one substitution step.

When an expected type is available, such as `Either(Int, Bool)`,
constructor checking uses the instantiated sum so `A(3)` is checked against
`Int -> Either(Int, Bool)`.

## Elaboration of polymorphic constructors

When a polymorphic constructor appears in an analytic position — at any
nesting depth — elaboration makes the implicit type instantiation explicit
by wrapping the bare constructor in an internal `TypAp` node. For
`A(3) : Either(Int, Bool)`:

```text
Asc(Ap(TypAp(A, TypTuple([Int, Bool])), 3), Either(Int, Bool))
```

Single-argument cases (`Some(3) : Option(Int)`) keep a bare
`TypAp(Some, Int)` without a `TypTuple` wrapper.

The inner `Constructor` node keeps its *polymorphic schema* as its type
ascription, fully normalized so no alias names leak into the elab. For
`Cons` the annotation is the `poly`-quantified form of the result sum
(aliases like `List` expand to their underlying `Rec` body inside the
`Poly`). Re-statics on the elaborated term is well-typed because `TypAp`
expects a `Poly`-typed callee and the constructor carries exactly that. At
runtime, `TypAp(Constructor(c, Some(Some(Poly(_, body)))), tau)` specializes
the schema by substituting `tau` for the binder, stepping to
`Constructor(c, Some(Some(subst(tau, _, body))))`. For multi-arg
applications the runtime substitutes a `TypTuple`'s elements element-wise
against the `TPat.Tuple` binder in one step. The constructor stays a final
value, now carrying a monomorphic (and still normalized) ascription.

Monomorphic constructors (e.g. `B` in `type T2 = + A(Int -> Int) + B`) have
a ctx schema that is just their declaring alias `Var("T2")` — an opaque
name that would hide arrow types inside the sum from
`DHExp.ty_comparable`'s `Typ.has_fun` check. Their annotation uses the
site-normalized specialized type instead, which unfolds the alias to
`Sum[A(Int -> Int), B]` and lets dynamics reject equality comparisons on
values that might hide functions.

Constructors whose schema is not actually polymorphic (e.g. a bare tag from
`type x = + A`) are never wrapped.

## Higher-kinded recursive types

A parameterized recursive type like `type List(a) = + Nil + Cons(a, List(a))`
is the fixed point at kind `* → *` of the type-level function that takes `a`
and produces the sum body — i.e.

\[
  \mathit{List} \;=\; \mu X{:}* \to *.\; \lambda a.\; +\, \mathit{Nil}
  \;+\; \mathit{Cons}(a, X(a))
\]

Hazel stores this as `Rec(List, TypFun(a, Sum[Nil, Cons(a,
TypParamAp(Var("List"), a))]))`: the `Rec` binder names the higher-kinded
fixed point and the inner `TypFun` exposes the type-level abstraction over
`a`. Inside the body, `Var("List")` refers to the `Rec` binder and has kind
`* → *`, so `TypParamAp(Var("List"), arg)` is well-formed for any `arg`.

The application `TypParamAp(Var("List"), Int)` is the canonical normal form
for `List(Int)`. After alias resolution it becomes `TypParamAp(Rec(List,
TypFun(a, …)), Int)`, and `weak_head_normalize` intentionally leaves it in
that shape — *it is the WHNF*. Eagerly β-reducing through the `TypFun`
would expose the body's self-references to a binder that no longer wraps a
`TypFun`, leaving them ill-formed.

To peer inside a higher-kinded recursive type (for constructor matching,
sum extraction, type meet across `Sum`/`Rec` shapes, etc.), use
`Typ.unfold_one`. It performs one step of the standard μ-unrolling rule:

\[
  \mu X{:}\kappa.\; F \;\equiv\; F[\mu X / X]
\]

For `TypParamAp(Rec(name, TypFun(p, body)), arg)` it substitutes the whole
`Rec(name, …)` for `Var(name)` in `body`, then β-reduces with `arg`. (For
multi-arg applications the helper `Typ.apply_args` peels one `TypFun` per
`TypTuple` element.) The resulting body has self-references of the shape
`TypParamAp(Rec(name, TypFun(p, body)), <inner_arg>)` — each one is the
recursive family applied at the relevant inner argument, exactly the
canonical encoding for that specialization. For uniform recursion
`<inner_arg> = arg`; for non-uniform recursion `<inner_arg>` may be a
transformation of `arg`.

### Where this matters

- `get_sum_constructors` calls `unfold_one` on `TypParamAp(Rec, _)` to
  extract the constructor map for a parameterized recursive type.
- `meet` compares two `TypParamAp(Rec, _)` structurally (same `Rec`, meet
  arguments) and falls back to one-step unfolding when one side is a
  `Sum`/`Rec` form that needs to be rolled into the other's shape.
- `normalize` treats `TypParamAp(Rec(_, TypFun(_, _)), _)` as a normal form,
  so recursive types do not infinitely expand.
- Constructor elaboration carries the canonical
  `TypParamAp(Rec(_, TypFun(_, _)), _)` form in
  `Constructor(_, Some(Some(_)))` annotations, so re-statics on evaluated
  results meets and unfolds them correctly even after the original
  `type List(a) = …` alias has been stripped from the elaboration.

### Non-uniform recursion

Non-uniform parameterized aliases like
`type List(a) = + Nil + Cons(a, List((Int, a)))` use the recursive family
at a *different* type than the outer parameter. Each `Cons`'s
self-application has the form `TypParamAp(Var("List"), Prod(Int, a))` where
the argument is a *transformation* of the parameter, not the parameter
itself. With the higher-kinded representation this is straightforward:
after one unfolding the resulting body has
`TypParamAp(Rec(List, TypFun(a, …)), Prod(Int, Int))` self-references at
the same `Rec`, applied at the inner argument. Static type-checking
elaborates each nested constructor with its own `TypAp(Cons, …)` wrapper
at the right level, evaluation runs to completion, and re-statics on the
evaluated result produces no marks.

## Modules

Parameterized type aliases inside modules export their full representation
through the module's labeled-tuple type. See `docs/modules.md` for the full
mechanics; the parameterized-types specifics:

- `ExpandModule.collect_type_exports` handles `Param(head, params)` tpats
  by building a `TypFun`-chain over the params (wrapped in `Rec` for
  self-referential definitions) and `Ctx.extend_alias`-ing it with the
  matching `(Type, …) -> Type` kind.
- `M.T(Int)` requires `dot` to bind tighter than the type-level postfix
  `T(args)` (`type_sum_ap`). `Precedence.dot` is set to a smaller (=
  tighter) value than `type_sum_ap` so qualified type access binds first.
- `Statics.kind_of_typ` for `ProdProjection(_)` resolves the projection
  through `weak_head_normalize` and recurses on the projected field's
  actual representation, so a parameterized export reports its full
  `(Type, …) -> Type` kind.
- Constructors declared inside a module are *not* added to the outer ctx —
  only type aliases are exported. `ctr_ana_typ` already routes constructor
  type-checking through the analysis target's `get_sum_constructors`;
  `Info.get_binding_site` mirrors that path for var-highlight, peeling
  `Arrow` layers (constructor-as-function position) and reading the
  variant's `ann.ids[0]` for the binding-site id.

## Tests

Focused coverage lives in:

- `test/Test_Menhir.re` for `type Option(a)` and `Option(Int)` parsing.
- `test/statics/Test_Statics_ParameterizedTypes.re` for kind errors,
  multi-arg applications, arity mismatches, constructor elaboration, and
  curried `typfun` aliases.
- `test/statics/Test_Statics_Modules.re` for parameterized type aliases
  exported from a module, including recursive ones
  (`module M = { type L(a) = +Nil + Cons(a, L(a)) }`).
- `test/evaluator/Test_Evaluator_TypAp.re` for the runtime reduction of
  parameterized constructor applications, including non-uniform recursion.
- `test/Test_Typ.re` for β-normalization and recursive family lookup.
- `test/Test_VarHighlight.re` for var-highlighting of constructors declared
  inside modules.

Useful targeted commands while iterating:

```sh
./run_tests test 'MenhirParser' -q
./run_tests test 'Statics.ParameterizedTypes' -q
./run_tests test 'Evaluator.TypAp' -q
./run_tests test 'Typ.normalize' -q
```
