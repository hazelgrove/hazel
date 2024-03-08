/* DHExp.re

   This module is specifically for dynamic expressions. They are stored
   using the same data structure as user expressions, but dynamic
   expressions are specifically paired with a `Satic.Map.t`. 
   */

include Exp;

let term_of = ({term, _}) => term;
let fast_copy = (id, {term, _}) => {ids: [id], term, copied: true};

let mk = (ids, term) => {
  {ids, copied: true, term};
};

let fresh_cast = (d: t, t1: Typ.t, t2: Typ.t): t =>
  if (Typ.eq(t1, t2) || t2 == Unknown(SynSwitch)) {
    d;
  } else {
    fresh(Cast(d, t1, t2));
  };

let apply_casts = (d: t, casts: list((Typ.t, Typ.t))): t =>
  List.fold_left((d, (ty1, ty2)) => fresh_cast(d, ty1, ty2), d, casts);

// TODO: make this function emit a map of changes
let replace_all_ids =
  map_term(
    ~f_exp=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_pat=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_typ=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_tpat=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
    ~f_rul=(continue, exp) => {...exp, ids: [Id.mk()]} |> continue,
  );

// TODO: make this function emit a map of changes
let repair_ids =
  map_term(
    ~f_exp=
      (continue, exp) =>
        if (exp.copied) {
          replace_all_ids(exp);
        } else {
          continue(exp);
        },
    _,
  );

// Also strips static error holes - kinda like unelaboration
let rec strip_casts =
  map_term(
    ~f_exp=
      (continue, exp) => {
        switch (term_of(exp)) {
        /* Leave non-casts unchanged */
        | Tuple(_)
        | Cons(_)
        | ListConcat(_)
        | ListLit(_)
        | MultiHole(_)
        | Seq(_)
        | Filter(_)
        | Let(_)
        | FixF(_)
        | TyAlias(_)
        | Fun(_)
        | Ap(_)
        | Test(_)
        | BuiltinFun(_)
        | UnOp(_)
        | BinOp(_)
        | Match(_)
        | Parens(_)
        | EmptyHole
        | Invalid(_)
        | Var(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | String(_)
        | Constructor(_)
        | DynamicErrorHole(_)
        | Closure(_)
        | If(_) => continue(exp)
        /* Remove casts*/
        | StaticErrorHole(_, d)
        | FailedCast(d, _, _)
        | Cast(d, _, _) => strip_casts(d)
        }
      },
    _,
  );

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
