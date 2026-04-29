# Parameterized Types

This document tracks the implementation of first-order parameterized type
constructors in Hazel.

## Surface Goal

Hazel should accept declarations such as:

```hazel
type Option(a) =
  + None
  + Some(a)
in
let x : Option(Int) = Some(3) in
...
```

Each parameter has kind `Type`. A declaration with `n` parameters introduces a
type constructor of *tuple-arrow* kind `(Type, ..., Type) -> Type`. Applying
that constructor to `n` type arguments at once — `Either(Int, Bool)` — produces
an ordinary type. Partial application (`Either(Int)`) is rejected as a kind
error, and over-application (`List(Int, Bool)` for a single-parameter `List`)
is rejected as an arity mismatch.

The argument list inside `T(...)` is parsed as a *type-level argument tuple*
(`TypTuple([…])`) rather than a regular product (`Prod([…])`). To pass a
single tuple-typed argument the user must wrap it in extra parens:
`List((Int, Bool))` is a list of pairs (a regular `Prod` argument), while
`List(Int, Bool)` is the ill-arity application.

Recursive aliases remain implicit, matching existing Hazel type aliases:

```hazel
type List(a) =
  + Nil
  + Cons(a, List(a))
in
...
```

No explicit surface syntax for type-level lambda and no higher-kinded type
parameters are part of this first implementation.

## Core Invariants

- Bare type constructors such as `Option` are not ordinary types when their kind
  is an arrow kind. They must be applied before appearing where kind `Type` is
  expected.
- Type-level functions and applications are internal core forms used to model
  parameterized declarations and `Option(Int)`-style applications.
- Existing unparameterized aliases continue to have kind `Type`.
- Recursive parameterized aliases unfold with their actual type arguments, so
  `List(Int)` recursively refers to `List(Int)`, not to a raw uninstantiated
  `List`.

## Implementation Notes

The first implementation is intentionally first-order: formal parameters always
have kind `Type`. The internal kind language still has arrow kinds so Hazel can
distinguish ordinary types from type constructors and report kind errors for
misuse such as writing `Option` where `Option(Int)` is required.

Type declarations are elaborated into internal type-level functions. For
example, `type Option(a) = ...` introduces `Option` with kind `Type -> Type`,
and `Option(Int)` beta-reduces the internal function body with `a := Int`.

Recursive parameterized aliases use the same implicit recursion rule as existing
Hazel aliases. A self-reference such as `List(a)` in the body is represented as
an application of the recursive family and unfolds enough to expose the top-level
sum constructors for typechecking.

Sum constructors from parameterized declarations are registered with polymorphic
schemas. For `Option(a)`, `None` has a schema equivalent to `poly a -> Option(a)`
and `Some` has a schema equivalent to `poly a -> a -> Option(a)`. When an
expected type is available, such as `Option(Int)`, constructor checking uses the
instantiated sum so `Some(3)` is checked against `Int -> Option(Int)`.

## Elaboration of polymorphic constructors

When a polymorphic constructor appears in an analytic position — at any
nesting depth — elaboration makes the implicit type instantiation explicit
by wrapping the bare constructor in a spine of `TypAp` nodes. For example,
`Cons(0, Cons(1, Nil)) : List(Int)` elaborates (roughly) to

```text
Ap(TypAp(Cons, Int), (0, Ap(TypAp(Cons, Int), (1, TypAp(Nil, Int)))))
```

The inner `Constructor` node keeps its *polymorphic schema* as its type
ascription, fully normalized so no alias names leak into the elab. For
`Cons` the annotation is the `poly`-quantified form of the result sum
(aliases like `List` expand to their underlying `Rec` body inside the
`Poly`). Re-statics on the elaborated term is now well-typed because
`TypAp` expects a `Poly`-typed callee and the constructor carries
exactly that. At runtime, `TypAp(Constructor(c, Some(Some(Poly(a,
body)))), tau)` specializes the schema by substituting `tau` for `a`,
stepping to `Constructor(c, Some(Some(subst(tau, a, body))))`. The
constructor stays a final value, now carrying a monomorphic (and still
normalized) ascription; this is the form the result view, the stepper,
and `DHExp.ty_comparable` consume.

Monomorphic constructors (e.g. `B` in `type T2 = +A(Int->Int)+B`) have
a ctx schema that is just their declaring alias `Var("T2")` — an opaque
name that would hide arrow types inside the sum from
`DHExp.ty_comparable`'s `Typ.has_fun` check. Their annotation uses the
site-normalized specialized type instead, which unfolds the alias to
`Sum[A(Int->Int), B]` and lets dynamics reject equality comparisons on
values that might hide functions.

## Higher-kinded recursive types

A parameterized recursive type like `type List(a) = + Nil + Cons(a,
List(a))` is the fixed point at kind `* → *` of the type-level function
that takes `a` and produces the sum body — i.e.

\[
  \mathit{List} \;=\; \mu X{:}* \to *.\; \lambda a.\; +\, \mathit{Nil}
  \;+\; \mathit{Cons}(a, X(a))
\]

Hazel stores this as `Rec(List, TypLam(a, Sum[Nil, Cons(a,
TypParamAp(Var("List"), a))]))`: the `Rec` binder names the higher-kinded
fixed point and the inner `TypLam` exposes the type-level abstraction
over `a`. Inside the body, `Var("List")` refers to the `Rec` binder and
its kind is `* → *`, so `TypParamAp(Var("List"), arg)` is well-formed for
any `arg`.

The application `TypParamAp(Var("List"), Int)` is the canonical normal form
for `List(Int)`. After alias resolution it becomes
`TypParamAp(Rec(List, TypLam(a, …)), Int)`, and `weak_head_normalize`
intentionally leaves it in that shape — *it is the WHNF*. Eagerly
β-reducing through the `TypLam` would expose the body's self-references
to a binder that no longer wraps a `TypLam`, leaving them ill-formed
and producing `TypParamAp(Rec(_, Sum[…]), arg)` artifacts in downstream
type comparisons.

To peer inside a higher-kinded recursive type (for constructor
matching, sum extraction, type meet across `Sum`/`Rec` shapes, etc.),
use `Typ.unfold_one`. It performs one step of the standard
μ-unrolling rule:

\[
  \mu X{:}\kappa.\; F \;\equiv\; F[\mu X / X]
\]

For `TypParamAp(Rec(name, TypLam(p, body)), arg)` it substitutes the whole
`Rec(name, …)` for `Var(name)` in `body`, then β-reduces with `arg`.
The resulting body has self-references of the shape
`TypParamAp(Rec(name, TypLam(p, body)), <inner_arg>)` — each one is the
recursive family applied at the relevant inner argument, exactly the
canonical encoding for that specialization. For uniform recursion
`<inner_arg> = arg`, so every self-reference is the same outer type;
for non-uniform recursion `<inner_arg>` may be a transformation of
`arg`, and the structural form distinguishes the inner specialization
from the outer one.

### Where this matters

- `get_sum_constructors` calls `unfold_one` on `TypParamAp(Rec, _)` to
  extract the constructor map for a parameterized recursive type.
- `meet` compares two `TypParamAp(Rec, _)` structurally (same `Rec`, meet
  arguments) and falls back to one-step unfolding when one side is a
  `Sum`/`Rec` form that needs to be rolled into the other's shape.
- `normalize` treats `TypParamAp(Rec(_, TypLam(_, _)), _)` as a normal
  form, so recursive types do not infinitely expand.
- Constructor elaboration carries the canonical
  `TypParamAp(Rec(_, TypLam(_, _)), _)` form in
  `Constructor(_, Some(Some(_)))` annotations, so re-statics on
  evaluated results meets and unfolds them correctly even after the
  original `type List(a) = …` alias has been stripped from the
  elaboration.

### Non-uniform recursion

Non-uniform parameterized aliases like `type List(a) = + Nil + Cons(a,
List((Int, a)))` use the recursive family at a *different* type than
the outer parameter. Each `Cons`'s self-application has the form
`TypParamAp(Var("List"), Prod(Int, a))` where the argument is a
*transformation* of the parameter, not the parameter itself. With the
higher-kinded representation this is straightforward: after one
unfolding the resulting body has `TypParamAp(Rec(List, TypLam(a, …)),
Prod(Int, Int))` self-references at the same `Rec`, applied at the
inner argument. Static type-checking elaborates each nested
constructor with its own `TypAp(Cons, …)` wrapper at the right level,
evaluation runs to completion, and re-statics on the evaluated result
produces no marks — the result type is well-formed and the constructor
annotations agree with the outer ascription via structural meet on
`TypParamAp(Rec, …)`.

Constructors whose schema is not actually polymorphic (e.g. a bare tag
from `type x = + A`) are never wrapped: writing `A @<?>` keeps the
explicit `TypAp` Indet, matching the pre-existing behavior of type
application over non-`TypFun` values.

## Tests

Focused coverage lives in:

- `test/Test_Menhir.re` for `type Option(a)` and `Option(Int)` parsing.
- `test/statics/Test_Statics_ParameterizedTypes.re` for kind errors and
  constructor checking.
- `test/Test_Typ.re` for beta-normalization and recursive family lookup.

Useful targeted commands while iterating:

```sh
./run_tests test 'MenhirParser' -q
./run_tests test 'Statics.ParameterizedTypes' -q
./run_tests test 'Typ.normalize' -q
```
