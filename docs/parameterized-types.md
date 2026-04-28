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
type constructor of kind `Type -> ... -> Type`, and applying that constructor to
`n` type arguments produces an ordinary type.

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
