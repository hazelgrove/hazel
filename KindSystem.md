# Type Aliases and Type Variables

# New Syntax

Type aliases:

```
type t = ? in
fun x : t { ? }

# result of type: t -> ?
```

Local type aliases:

```
let f =
  type t = Int in
  fun x : t { ? }
  
$ result of type: Int -> ?
```

Type variables:

```
fun x : t { ? }
```

Type variable holes:

```
type Int = ...

# error on Int: builtin type

type fun = ...

# error on fun: reserved keyword

type 123 = ...

# error on 123: invalid type variable name

fun x : z { ? }

# error on z: unbound type variable
```

Also, more free-form text entry in type positions.

# New HTyp API

Type variables "break" structural equality. For example, in Hazel:

```
type t1 = ... in
type t2 = ... in ...
```

becomes in ReasonML something like:

```
let ty1 = TyVar(idx1, "t1");
let ty2 = TyVar(idx2, "t2");
```

where `idx1` and `idx2` are de Bruijn indices corresponding to bindings for `t1`
and `t2` in some typing context.

But recall, type variables represent type equivalence classes, so structural
equality is often too strong to be useful. In such cases, we check for
*equivalence* of types instead, which is defined recursively as equivalence of
the underlying types of type variables, and structural equality everywhere else.

In the example above, `t1` and `t2` may be equivalent, depending on what the
typing context binds `idx1` and `idx2` to, but they are not structurally equal
because they were bound under different names.

Here's an example of where this distinction matters in the existing code base,
from Statics_Exp.re on branch `dev`:

```
switch (syn_rule(ctx, rule, pat_ty)) {
| None => false
| Some(syn_ty) => HTyp.eq(rule_ty, syn_ty)
}
```

Since `HTyp.eq` is just an alias for `(==)`, this code would fail to equate two
type variables bound to the same type, for instance if `t1` and `t2` were both
aliases of `Int`.

To avoid this problem, use `HTyp.equivalent` instead:
```
switch (syn_rule(ctx, rule, pat_ty)) {
| None => false
| Some(syn_ty) => HTyp.equivalent(ctx, rule_ty, syn_ty)
}
```

And this more general notion of equivalence is precisely what we get by adding
kinds to the type system! It offers three kinds:

- `Hole` the unknown kind
- `Type` the base kind
- `S(ty)` the singleton kind of all types equivalent to `ty`

And then we can bind type variable names to indices in the new typing context,
like so:

```
let ctx = Context.add_tyvar(ctx, "t", Kind.Hole);
let (idx, t, _) = Context.tyvar(ctx, "t");
let ctx = Context.add_var(ctx, "x", HTyp.tyvar(idx, t));
```

And what is `idx`? It's just the (de Bruin) index for the type variable `t` in
the typing context `ctx`.

Indices are (typed) references into a typing context. Outside of the typing
context, indices use absolute positioning, so they are like a generalization of
integer-based array indices. For example, `HTyp.t` is defined roughly like so:

```
type t = HTyp.Syntax.t(Index.Abs.t);
```

and we say `Index.Abs.t` "brands" `HTyp.t` as using absolutely positioned
indices. Inside the typing context, we use relative positioning. By branding
types with the type of index they use, we can rely on the (ReasonML) type system
to ensure we never mix the two.

You may have also noticed that the new code uses constructor functions instead
of operating directly on constructors of `HTyp.t`. In the new system, `HTyp.t`
is an opaque type, so its instances can't be pattern matched against directly:

```
switch (HTyp.Int) { ...

# error on HTyp.Int

switch (HTyp.Syntax.Int) { ...

# error on HTyp.Syntax.Int
```

To get the underlying constructors of an `HTyp.t`, use `HTyp.to_syntax`:

```
switch (HTyp.to_syntax(HTyp.Syntax.Int)) { ...
```

and in the other direction:

```
let ty = HTyp.of_syntax(HTyp.Syntax.Int)
```

There are two other forms of `HTyp` to be aware of: normalized and head-normalized.

A normalized `HTyp` is a type without any type variables of singleton kind.
Sometimes (e.g., in the evaluator) we have access to types but no typing context
(and therefore no type variables).

A head-normalized `HTyp` is a type that is not itself a type variable, but that
may contain type variables in its subterms. Sometimes it's bad to normalize "too
far" (e.g., inside `HTyp.matched_arrow`).

# New Type System

<diagram>

# New Challenges

Don't mix index types, except possibly while working inside `Context.re` and its
supporting modules.

Beware of stale types or contexts:

```
let (idx, t, _) = Context.tyvar(ctx, "t");
let ty = HTyp.tyvar(idx, t);
switch (syn_elab_lines(ctx, ...)) {
| Elaborates((new_ctx, new_ty)) => HTyp.equivalent(ctx, ty, new_ty);
```
