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

But recall: type variables represent type equivalence classes, so structural
equality is often too strong to be useful. For such
cases, we provide a relaxed
equivalence comparison, defined recursively as kind equivalence if either
operand is a type variable and structural equality (modulo type variables)
otherwise.

From Statics_Exp.re (dev):
```
switch (syn_rule(ctx, rule, pat_ty)) {
| None => false
| Some(syn_ty) => HTyp.eq(rule_ty, syn_ty)
}
```
From Statics_Exp.re (type-alias-3):
```
switch (syn_rule(ctx, rule, pat_ty)) {
| None => false
| Some(syn_ty) => HTyp.equivalent(ctx, rule_ty, syn_ty)
}
```

# New Type System

# New Challenges
