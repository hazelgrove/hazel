/*
 To discuss:

 1. putting info inside expressions
 2. The issue with recursion capture


 */
/*
 DExps that can appear during evaluation, and thus won't have static information.

 - Closure
 - Var [for mutual recursion; could probably get rid of if needed...]
 - Let [for mutual recursion]
 - Tuple([])
 - Cast
 - Ap [in the casting rules for functions & in builtins]
 - DynamicErrorHole
 - FailedCast
 - Int
 - Bool
 - Float
 - String
 - ListLit
 - BuiltinFun

 It is important that the following do not appear during evaluation, because they
 (theoretically) require static information:

  - Fun
  - FixF

 */

/* DExp.re

   This module is specifically for dynamic expressions. They are stored
   using the same data structure as user expressions, but have a few
   important invariants.
   */

include Exp;

let term_of = ({term, _}) => term;
let fast_copy = (id, {term, _}) => {ids: [id], term, copied: true};
// All children of term must have expression-unique ids.
let fresh = term => {
  {ids: [Id.mk()], copied: false, term};
};
let unwrap = ({ids, term, copied}) => (term, term => {ids, term, copied});

let mk = (ids, term) => {
  {ids, copied: true, term};
};

// All children of d must have expression-unique ids.
let fresh_cast = (d: t, t1: Typ.t, t2: Typ.t): t =>
  if (Typ.eq(t1, t2) || t2 == Unknown(SynSwitch)) {
    d;
  } else {
    fresh(Cast(d, t1, t2));
  };

let apply_casts = (d: t, casts: list((Typ.t, Typ.t))): t =>
  List.fold_left((d, (ty1, ty2)) => fresh_cast(d, ty1, ty2), d, casts);

// TODO: make this function emit a map of changes
let rec repair_ids = (require: bool, d: t) => {
  let child_require = require || d.copied;
  let repair_ids = repair_ids(child_require);
  let term = term_of(d);
  let rewrap = term => {
    ids:
      child_require
        ? {
          let id = Id.mk();
          [id];
        }
        : d.ids,
    copied: false,
    term,
  };
  (
    switch (term) {
    | EmptyHole
    | Invalid(_)
    | Var(_)
    | BuiltinFun(_)
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | Constructor(_) => term
    | StaticErrorHole(static_id, d1) =>
      StaticErrorHole(static_id, repair_ids(d1))
    | DynamicErrorHole(d1, x) => DynamicErrorHole(repair_ids(d1), x)
    | FailedCast(d1, t1, t2) => FailedCast(repair_ids(d1), t1, t2)
    | Closure(env, d1) => Closure(env, repair_ids(d1))
    | Filter(flt, d1) => Filter(flt, repair_ids(d1))
    | Seq(d1, d2) => Seq(repair_ids(d1), repair_ids(d2))
    | Let(dp, d1, d2) => Let(dp, repair_ids(d1), repair_ids(d2))
    | FixF(f, d1, env) => FixF(f, repair_ids(d1), env)
    | TyAlias(tp, t, d) => TyAlias(tp, t, repair_ids(d))
    | Fun(dp, d1, env, f) => Fun(dp, repair_ids(d1), env, f)
    | Ap(dir, d1, d2) => Ap(dir, repair_ids(d1), repair_ids(d2))
    | Test(d1) => Test(repair_ids(d1))
    | UnOp(op, d1) => UnOp(op, repair_ids(d1))
    | BinOp(op, d1, d2) => BinOp(op, repair_ids(d1), repair_ids(d2))
    | ListLit(ds) => ListLit(List.map(repair_ids, ds))
    | Cons(d1, d2) => Cons(repair_ids(d1), repair_ids(d2))
    | Parens(d1) => Parens(repair_ids(d1))
    | ListConcat(d1, d2) => ListConcat(repair_ids(d1), repair_ids(d2))
    | Tuple(ds) => Tuple(List.map(repair_ids, ds))
    // TODO: repair ids inside multihole
    | MultiHole(ds) => MultiHole(ds)
    | Match(d1, rls) =>
      Match(
        repair_ids(d1),
        List.map(((p, d)) => (p, repair_ids(d)), rls),
      )
    | Cast(d1, t1, t2) => Cast(repair_ids(d1), t1, t2)
    | If(d1, d2, d3) => If(repair_ids(d1), repair_ids(d2), repair_ids(d3))
    }
  )
  |> rewrap;
};

let repair_ids = repair_ids(false);

// Also strips static error holes - kinda like unelaboration
let rec strip_casts = d => {
  let (term, rewrap) = unwrap(d);
  switch (term) {
  | Closure(ei, d) => Closure(ei, strip_casts(d)) |> rewrap
  | Cast(d, _, _) => strip_casts(d)
  | FailedCast(d, _, _) => strip_casts(d)
  | Tuple(ds) => Tuple(ds |> List.map(strip_casts)) |> rewrap
  | Cons(d1, d2) => Cons(strip_casts(d1), strip_casts(d2)) |> rewrap
  | ListConcat(d1, d2) =>
    ListConcat(strip_casts(d1), strip_casts(d2)) |> rewrap
  | ListLit(ds) => ListLit(List.map(strip_casts, ds)) |> rewrap
  // TODO[Matt]: Strip multihole casts
  | MultiHole(ds) => MultiHole(ds) |> rewrap
  | StaticErrorHole(_, d) => strip_casts(d)
  | Seq(a, b) => Seq(strip_casts(a), strip_casts(b)) |> rewrap
  | Filter(f, b) => Filter(strip_filter_casts(f), strip_casts(b)) |> rewrap
  | Let(dp, b, c) => Let(dp, strip_casts(b), strip_casts(c)) |> rewrap
  | FixF(a, c, env) => FixF(a, strip_casts(c), env) |> rewrap
  | TyAlias(tp, t, d) => TyAlias(tp, t, strip_casts(d)) |> rewrap
  | Fun(a, c, e, d) => Fun(a, strip_casts(c), e, d) |> rewrap
  | Ap(dir, a, b) => Ap(dir, strip_casts(a), strip_casts(b)) |> rewrap
  | Test(a) => Test(strip_casts(a)) |> rewrap
  | BuiltinFun(fn) => BuiltinFun(fn) |> rewrap
  | UnOp(op, d) => UnOp(op, strip_casts(d)) |> rewrap
  | BinOp(a, b, c) => BinOp(a, strip_casts(b), strip_casts(c)) |> rewrap
  | Match(a, rules) =>
    Match(
      strip_casts(a),
      List.map(((k, v)) => (k, strip_casts(v)), rules),
    )
    |> rewrap
  | Parens(d1) => Parens(strip_casts(d1)) |> rewrap
  | EmptyHole as d
  | Invalid(_) as d
  | Var(_) as d
  | Bool(_) as d
  | Int(_) as d
  | Float(_) as d
  | String(_) as d
  | Constructor(_) as d
  | DynamicErrorHole(_) as d => d |> rewrap
  | If(c, d1, d2) =>
    If(strip_casts(c), strip_casts(d1), strip_casts(d2)) |> rewrap
  };
}
and strip_filter_casts = f => {
  switch (f) {
  | Filter({act, pat}) => Filter({act, pat: pat |> strip_casts})
  | Residue(idx, act) => Residue(idx, act)
  };
};

let rec fast_equal = ({term: d1, _} as d1exp, {term: d2, _} as d2exp): bool => {
  switch (d1, d2) {
  /* Primitive forms: regular structural equality */
  | (Var(_), _)
  /* TODO: Not sure if this is right... */
  | (Bool(_), _)
  | (Int(_), _)
  | (Float(_), _)
  | (Constructor(_), _) => d1 == d2
  | (String(s1), String(s2)) => String.equal(s1, s2)
  | (String(_), _) => false

  | (Parens(x), _) => fast_equal(x, d2exp)
  | (_, Parens(x)) => fast_equal(d1exp, x)

  /* Non-hole forms: recurse */
  | (Test(d1), Test(d2)) => fast_equal(d1, d2)
  | (Seq(d11, d21), Seq(d12, d22)) =>
    fast_equal(d11, d12) && fast_equal(d21, d22)
  | (Filter(f1, d1), Filter(f2, d2)) =>
    filter_fast_equal(f1, f2) && fast_equal(d1, d2)
  | (Let(dp1, d11, d21), Let(dp2, d12, d22)) =>
    dp1 == dp2 && fast_equal(d11, d12) && fast_equal(d21, d22)
  | (FixF(f1, d1, sigma1), FixF(f2, d2, sigma2)) =>
    f1 == f2
    && fast_equal(d1, d2)
    && Option.equal(ClosureEnvironment.id_equal, sigma1, sigma2)
  | (Fun(dp1, d1, None, s1), Fun(dp2, d2, None, s2)) =>
    dp1 == dp2 && fast_equal(d1, d2) && s1 == s2
  | (Fun(dp1, d1, Some(env1), s1), Fun(dp2, d2, Some(env2), s2)) =>
    dp1 == dp2
    && fast_equal(d1, d2)
    && ClosureEnvironment.id_equal(env1, env2)
    && s1 == s2
  | (Ap(dir1, d11, d21), Ap(dir2, d12, d22)) =>
    dir1 == dir2 && fast_equal(d11, d12) && fast_equal(d21, d22)
  | (Cons(d11, d21), Cons(d12, d22)) =>
    fast_equal(d11, d12) && fast_equal(d21, d22)
  | (ListConcat(d11, d21), ListConcat(d12, d22)) =>
    fast_equal(d11, d12) && fast_equal(d21, d22)
  | (Tuple(ds1), Tuple(ds2)) =>
    List.length(ds1) == List.length(ds2)
    && List.for_all2(fast_equal, ds1, ds2)
  | (BuiltinFun(f1), BuiltinFun(f2)) => f1 == f2
  | (ListLit(ds1), ListLit(ds2)) =>
    List.length(ds1) == List.length(ds2)
    && List.for_all2(fast_equal, ds1, ds2)
  | (UnOp(op1, d1), UnOp(op2, d2)) => op1 == op2 && fast_equal(d1, d2)
  | (BinOp(op1, d11, d21), BinOp(op2, d12, d22)) =>
    op1 == op2 && fast_equal(d11, d12) && fast_equal(d21, d22)
  | (TyAlias(tp1, ut1, d1), TyAlias(tp2, ut2, d2)) =>
    tp1 == tp2 && ut1 == ut2 && fast_equal(d1, d2)
  | (Cast(d1, ty11, ty21), Cast(d2, ty12, ty22))
  | (FailedCast(d1, ty11, ty21), FailedCast(d2, ty12, ty22)) =>
    fast_equal(d1, d2) && ty11 == ty12 && ty21 == ty22
  | (DynamicErrorHole(d1, reason1), DynamicErrorHole(d2, reason2)) =>
    fast_equal(d1, d2) && reason1 == reason2
  | (Match(s1, rs1), Match(s2, rs2)) =>
    fast_equal(s1, s2)
    && List.length(rs2) == List.length(rs2)
    && List.for_all2(
         ((k1, v1), (k2, v2)) => k1 == k2 && fast_equal(v1, v2),
         rs1,
         rs2,
       )
  | (If(d11, d12, d13), If(d21, d22, d23)) =>
    fast_equal(d11, d21) && fast_equal(d12, d22) && fast_equal(d13, d23)
  /* We can group these all into a `_ => false` clause; separating
     these so that we get exhaustiveness checking. */
  | (Seq(_), _)
  | (Filter(_), _)
  | (Let(_), _)
  | (FixF(_), _)
  | (Fun(_), _)
  | (Test(_), _)
  | (Ap(_), _)
  | (BuiltinFun(_), _)
  | (Cons(_), _)
  | (ListConcat(_), _)
  | (ListLit(_), _)
  | (Tuple(_), _)
  | (UnOp(_), _)
  | (BinOp(_), _)
  | (Cast(_), _)
  | (FailedCast(_), _)
  | (TyAlias(_), _)
  | (DynamicErrorHole(_), _)
  | (If(_), _)
  | (Match(_), _) => false

  /* Hole forms: when checking environments, only check that
     environment ID's are equal, don't check structural equality.

     (This resolves a performance issue with many nested holes.) */
  | (EmptyHole, EmptyHole) => true
  | (MultiHole(_), MultiHole(_)) => rep_id(d1exp) == rep_id(d2exp)
  | (StaticErrorHole(sid1, d1), StaticErrorHole(sid2, d2)) =>
    sid1 == sid2 && d1 == d2
  | (Invalid(text1), Invalid(text2)) => text1 == text2
  | (Closure(sigma1, d1), Closure(sigma2, d2)) =>
    ClosureEnvironment.id_equal(sigma1, sigma2) && fast_equal(d1, d2)
  | (EmptyHole, _)
  | (MultiHole(_), _)
  | (StaticErrorHole(_), _)
  | (Invalid(_), _)
  | (Closure(_), _) => false
  };
}
and filter_fast_equal = (f1, f2) => {
  switch (f1, f2) {
  | (Filter(f1), Filter(f2)) =>
    fast_equal(f1.pat, f2.pat) && f1.act == f2.act
  | (Residue(idx1, act1), Residue(idx2, act2)) =>
    idx1 == idx2 && act1 == act2
  | _ => false
  };
};
