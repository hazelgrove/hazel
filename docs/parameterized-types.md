# Parameterized Types

This document tracks the design and implementation of first-order
parameterized type constructors in Hazel.

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

Each parameter has kind `Type`. A declaration with `n` parameters introduces a
type constructor of *tuple-arrow* kind `(Type, ..., Type) -> Type`. Applying
that constructor to its `n` type arguments at once produces an ordinary type.
Tuple-arrow kinds are atomic: they admit neither partial application nor
over-application.

- `Either(Int, Bool)` — well-kinded, becomes a regular `Type`.
- `Either(Int)` — rejected as `expects 2 arguments, got 1`.
- `List(Int, Bool)` (for a 1-parameter `List`) — rejected as
  `expects 1 argument, got 2`.
- `Either((Int, Bool))` — extra parens make `(Int, Bool)` a single tuple-typed
  argument, so this is also a 1-of-2 arity error.
- `List((Int, Bool))` — a list of pairs (single-arg application whose argument
  is a `Prod`).

Recursive aliases remain implicit, matching existing Hazel type aliases:

```hazel
type List(a) =
  + Nil
  + Cons(a, List(a))
in
...
```

The recursive form is fully supported, including non-uniform recursion such as
`type List(a) = + Nil + Cons(a, List((Int, a)))` (see *Higher-kinded recursive
types* below).

No explicit surface syntax for type-level lambda, no higher-kinded type
parameters, and no value-level multi-binder type abstractions are part of this
implementation.

## Naming

Hazel has two related "type application" concepts that used to be named
identically and are now visibly distinct:

| Concept | AST | User-facing |
|---|---|---|
| Expression-level: typfun applied to a type — `f@<Int>` | `Exp.TypAp(exp_t, typ_t)` | "Type application" |
| Type-level: parameterized type applied to args — `Either(Int, Bool)` | `Typ.TypParamAp(typ_t, typ_t)` | "Type parameter application" |
| Multi-arg bundle inside a `TypParamAp` | `Typ.TypTuple(list(typ_t))` | "Type parameter argument tuple" |

`TypTuple` only ever appears as the second argument of a `TypParamAp`. It has
no kind on its own; its elements must match the callee's tuple-arrow kind.

## Parsing & disambiguation

Source-level `T(args)` is always a `Typ.TypParamAp(T, arg)` at the AST. The
disambiguation between "multi-arg" and "single-arg-tuple" is in the *shape* of
the argument:

- `T(a, b)` → `TypParamAp(T, TypTuple([a, b]))` — multi-argument application.
- `T((a, b))` → `TypParamAp(T, Prod([a, b]))` — single argument that is a
  tuple type. (Hazel's structured editor preserves the user's extra parens
  by producing a `Parens(Prod([…]))`-shaped tile that the term builder
  unwraps to `Prod`; the `TypTuple` form is *only* produced when the comma
  list sits directly under `T(...)` without extra wrapping.)
- `T(a)` → `TypParamAp(T, a)` — single argument, not in a `TypTuple`.

`MakeTerm.apply_typ_param_args` and the Menhir parser implement this lift; the
sum-variant extractor reverses it locally when a `Cons(a, b)` shape sits in a
sum-type position (so a constructor variant with a tuple payload remains a
`Prod`, not a `TypTuple`).

## Kinds

```ocaml
type TypKind.t =
  | Unknown
  | Type
  | Arrow(list(t), t);
```

`Arrow([k1, …, kN], r)` is rendered as `(k1, …, kN) -> r` (single-arg as
`k -> r`). `Unknown` renders as `?` and is the kind assigned to unbound type
variables and unknown types — it propagates consistently like `Typ.Unknown`,
so a free `L` in `Cons(a, L(a))` produces only one error (the free-variable
mark on `L`) instead of also erroring on the surrounding application.

`TypKind.apply` only consumes a *single* argument against a single-argument
arrow `Arrow([k], r)`. Multi-argument applications go through
`TypKind.apply_all`, which requires the entire argument list to match the
arrow's slot list at once. There is no partial application. Both helpers
absorb `Unknown` callees: applying any args to `Unknown` yields `Unknown`.

Kind comparison uses `TypKind.consistent` (similar to `Typ.is_consistent`):
two kinds are consistent if `Unknown` could be refined to make them match.

The `Mark.TypParamApplyArityMismatch` is emitted by `Statics.status_for_node`
when the argument count differs from the kind's arity. Its message — which
includes the callee type pretty-printed — reads:

```
`Either` expects 2 arguments, got 1
```

`Mark.TypParamApplyNonArrowKind` is emitted when the callee has kind `Type`
(not an arrow at all).

## Schemas of parameterized constructors

Sum constructors from parameterized declarations are registered with
polymorphic schemas. For `Either(a, b)`:

- `A` has schema `poly a -> poly b -> a -> Either(a, b)`
- `B` has schema `poly a -> poly b -> b -> Either(a, b)`

When an expected type is available, such as `Either(Int, Bool)`, constructor
checking uses the instantiated sum so `A(3)` is checked against
`Int -> Either(Int, Bool)`.

## Elaboration of polymorphic constructors

When a polymorphic constructor appears in an analytic position — at any
nesting depth — elaboration makes the implicit type instantiation explicit by
wrapping the bare constructor in a `TypAp`. For example,
`A(3) : Either(Int, Bool)` elaborates (roughly) to

```text
Asc(Ap(TypAp(A, TypTuple([Int, Bool])), 3), Either(Int, Bool))
```

Single-argument cases (`Some(3) : Option(Int)`) keep a bare `TypAp(Some,
Int)` without a `TypTuple` wrapper. Multi-argument cases bundle all the type
arguments into a `TypTuple` and reduce in one `TypAp` step.

The inner `Constructor` node keeps its *polymorphic schema* as its type
ascription, fully normalized so no alias names leak into the elab. For `Cons`
the annotation is the `poly`-quantified form of the result sum (aliases like
`List` expand to their underlying `Rec` body inside the `Poly`). Re-statics
on the elaborated term is well-typed because `TypAp` expects a `Poly`-typed
callee and the constructor carries exactly that. At runtime,
`TypAp(Constructor(c, Some(Some(Poly(a, body)))), tau)` specializes the
schema by substituting `tau` for `a`, stepping to
`Constructor(c, Some(Some(subst(tau, a, body))))`. For multi-arg
applications the runtime peels one `Poly` per `TypTuple` element. The
constructor stays a final value, now carrying a monomorphic (and still
normalized) ascription.

Monomorphic constructors (e.g. `B` in `type T2 = +A(Int->Int)+B`) have a ctx
schema that is just their declaring alias `Var("T2")` — an opaque name that
would hide arrow types inside the sum from `DHExp.ty_comparable`'s
`Typ.has_fun` check. Their annotation uses the site-normalized specialized
type instead, which unfolds the alias to `Sum[A(Int->Int), B]` and lets
dynamics reject equality comparisons on values that might hide functions.

## Higher-kinded recursive types

A parameterized recursive type like `type List(a) = + Nil + Cons(a, List(a))`
is the fixed point at kind `* → *` of the type-level function that takes `a`
and produces the sum body — i.e.

\[
  \mathit{List} \;=\; \mu X{:}* \to *.\; \lambda a.\; +\, \mathit{Nil}
  \;+\; \mathit{Cons}(a, X(a))
\]

Hazel stores this as `Rec(List, TypLam(a, Sum[Nil, Cons(a,
TypParamAp(Var("List"), a))]))`: the `Rec` binder names the higher-kinded
fixed point and the inner `TypLam` exposes the type-level abstraction over
`a`. Inside the body, `Var("List")` refers to the `Rec` binder and has kind
`* → *`, so `TypParamAp(Var("List"), arg)` is well-formed for any `arg`.

The application `TypParamAp(Var("List"), Int)` is the canonical normal form
for `List(Int)`. After alias resolution it becomes `TypParamAp(Rec(List,
TypLam(a, …)), Int)`, and `weak_head_normalize` intentionally leaves it in
that shape — *it is the WHNF*. Eagerly β-reducing through the `TypLam` would
expose the body's self-references to a binder that no longer wraps a
`TypLam`, leaving them ill-formed and producing
`TypParamAp(Rec(_, Sum[…]), arg)` artifacts in downstream type comparisons.

To peer inside a higher-kinded recursive type (for constructor matching, sum
extraction, type meet across `Sum`/`Rec` shapes, etc.), use `Typ.unfold_one`.
It performs one step of the standard μ-unrolling rule:

\[
  \mu X{:}\kappa.\; F \;\equiv\; F[\mu X / X]
\]

For `TypParamAp(Rec(name, TypLam(p, body)), arg)` it substitutes the whole
`Rec(name, …)` for `Var(name)` in `body`, then β-reduces with `arg`. (For
multi-arg applications the helper `Typ.apply_args` peels one `TypLam` per
`TypTuple` element.) The resulting body has self-references of the shape
`TypParamAp(Rec(name, TypLam(p, body)), <inner_arg>)` — each one is the
recursive family applied at the relevant inner argument, exactly the
canonical encoding for that specialization. For uniform recursion
`<inner_arg> = arg`, so every self-reference is the same outer type; for
non-uniform recursion `<inner_arg>` may be a transformation of `arg`, and the
structural form distinguishes the inner specialization from the outer one.

### Where this matters

- `get_sum_constructors` calls `unfold_one` on `TypParamAp(Rec, _)` to
  extract the constructor map for a parameterized recursive type.
- `meet` compares two `TypParamAp(Rec, _)` structurally (same `Rec`, meet
  arguments) and falls back to one-step unfolding when one side is a
  `Sum`/`Rec` form that needs to be rolled into the other's shape.
- `normalize` treats `TypParamAp(Rec(_, TypLam(_, _)), _)` as a normal form,
  so recursive types do not infinitely expand.
- Constructor elaboration carries the canonical
  `TypParamAp(Rec(_, TypLam(_, _)), _)` form in
  `Constructor(_, Some(Some(_)))` annotations, so re-statics on evaluated
  results meets and unfolds them correctly even after the original
  `type List(a) = …` alias has been stripped from the elaboration.

### Non-uniform recursion

Non-uniform parameterized aliases like
`type List(a) = + Nil + Cons(a, List((Int, a)))` use the recursive family at
a *different* type than the outer parameter. Each `Cons`'s self-application
has the form `TypParamAp(Var("List"), Prod(Int, a))` where the argument is a
*transformation* of the parameter, not the parameter itself. With the
higher-kinded representation this is straightforward: after one unfolding the
resulting body has `TypParamAp(Rec(List, TypLam(a, …)), Prod(Int, Int))`
self-references at the same `Rec`, applied at the inner argument. Static
type-checking elaborates each nested constructor with its own
`TypAp(Cons, …)` wrapper at the right level, evaluation runs to completion,
and re-statics on the evaluated result produces no marks — the result type
is well-formed and the constructor annotations agree with the outer
ascription via structural meet on `TypParamAp(Rec, …)`.

Constructors whose schema is not actually polymorphic (e.g. a bare tag from
`type x = + A`) are never wrapped: writing `A @<?>` keeps the explicit
`TypAp` Indet, matching the pre-existing behavior of type application over
non-`TypFun` values.

## Tests

Focused coverage lives in:

- `test/Test_Menhir.re` for `type Option(a)` and `Option(Int)` parsing.
- `test/statics/Test_Statics_ParameterizedTypes.re` for kind errors,
  multi-arg applications, arity mismatches, and constructor elaboration.
- `test/evaluator/Test_Evaluator_TypAp.re` for the runtime reduction of
  parameterized constructor applications, including non-uniform recursion.
- `test/Test_Typ.re` for β-normalization and recursive family lookup.

Useful targeted commands while iterating:

```sh
./run_tests test 'MenhirParser' -q
./run_tests test 'Statics.ParameterizedTypes' -q
./run_tests test 'Evaluator.TypAp' -q
./run_tests test 'Typ.normalize' -q
```
