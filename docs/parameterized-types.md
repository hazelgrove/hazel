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
