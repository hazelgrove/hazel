open Util.OptUtil.Syntax;

let rec match_rewrite =
        (
          ~quantifiers: list((string, TermBase.Typ.t)),
          rw_from: Exp.t,
          expr: Exp.t,
          ~type_of: Exp.t => Typ.t,
          ~ctx_of: Exp.t => Ctx.t,
        )
        : option(Environment.t) => {
  let go = (~quantifiers=quantifiers) =>
    match_rewrite(~quantifiers, ~type_of, ~ctx_of);
  switch (rw_from |> Exp.term_of, expr |> Exp.term_of) {
  // Special cases:
  | (Var(x), _) when List.exists(((y, _)) => x == y, quantifiers) =>
    let expected_type = List.assoc(x, quantifiers);
    let actual_type = type_of(expr);
    if (Typ.is_consistent(ctx_of(expr), expected_type, actual_type)) {
      Some(Environment.singleton((x, expr)));
    } else {
      None;
    };
  | (Parens(x), _) => go(x, expr)
  | (_, Parens(y)) => go(rw_from, y)
  // Normal cases:
  | (Invalid(x), Invalid(y)) when x == y => Some(Environment.empty)
  | (Invalid(_), _) => None
  | (EmptyHole, EmptyHole) => Some(Environment.empty)
  | (EmptyHole, _) => None
  | (MultiHole(x), MultiHole(y)) when List.equal(Any.fast_equal, x, y) =>
    Some(Environment.empty)
  | (MultiHole(_), _) => None
  | (DynamicErrorHole(x1, e1), DynamicErrorHole(x2, e2)) when e1 == e2 =>
    go(x1, x2)
  | (DynamicErrorHole(_), _) => None
  | (FailedCast(x1, t1, t2), FailedCast(x2, t3, t4))
      when Typ.fast_equal(t1, t3) && Typ.fast_equal(t2, t4) =>
    go(x1, x2)
  | (FailedCast(_), _) => None
  | (Deferral(d1), Deferral(d2)) when d1 == d2 => Some(Environment.empty)
  | (Deferral(_), _) => None
  | (Bool(x), Bool(y)) when x == y => Some(Environment.empty)
  | (Bool(_), _) => None
  | (Int(x), Int(y)) when x == y => Some(Environment.empty)
  | (Int(_), _) => None
  | (Float(x), Float(y)) when x == y => Some(Environment.empty)
  | (Float(_), _) => None
  | (String(x), String(y)) when x == y => Some(Environment.empty)
  | (String(_), _) => None
  | (ListLit(x), ListLit(y)) when List.length(x) != List.length(y) => None
  | (ListLit(x), ListLit(y)) =>
    List.fold_left2(
      (acc, x, y) => {
        let* env = acc;
        let* env' = go(x, y);
        Some(Environment.union(env, env'));
      },
      Some(Environment.empty),
      x,
      y,
    )
  | (ListLit(_), _) => None
  | (Constructor(x), Constructor(y)) when x == y => Some(Environment.empty)
  | (Constructor(_), _) => None
  | (Fun(p1, x1, e1, _), Fun(p2, x2, e2, _))
      when e1 == e2 && Pat.fast_equal(p1, p2) =>
    // TODO: alpha equivalence here (and in similar places)?
    let bound_vars = Pat.bound_vars(p2);
    let quantifiers =
      List.filter(((x, _)) => !List.mem(x, bound_vars), quantifiers);
    go(~quantifiers, x1, x2);
  | (Fun(_), _) => None
  | (TypFun(p1, x1, _), TypFun(p2, x2, _)) when TPat.fast_equal(p1, p2) =>
    go(x1, x2)
  | (TypFun(_), _) => None
  | (Tuple(x), Tuple(y)) when List.length(x) != List.length(y) => None
  | (Tuple(x), Tuple(y)) =>
    List.fold_left2(
      (acc, x, y) => {
        let* env = acc;
        let* env' = go(x, y);
        Some(Environment.union(env, env'));
      },
      Some(Environment.empty),
      x,
      y,
    )
  | (Tuple(_), _) => None
  | (Var(x), Var(y)) when x == y => Some(Environment.empty)
  | (Var(_), _) => None
  | (Let(p1, x1, e1), Let(p2, x2, e2)) when Pat.fast_equal(p1, p2) =>
    let quantifiers =
      List.filter(
        ((x, _)) => !List.mem(x, Pat.bound_vars(p2)),
        quantifiers,
      );
    let* env = go(x1, x2);
    let+ env' = go(~quantifiers, e1, e2);
    Environment.union(env, env');
  | (Let(_), _) => None
  | (FixF(p1, x1, e1), FixF(p2, x2, e2))
      when Pat.fast_equal(p1, p2) && e1 == e2 =>
    let quantifiers =
      List.filter(
        ((x, _)) => !List.mem(x, Pat.bound_vars(p2)),
        quantifiers,
      );
    go(~quantifiers, x1, x2);
  | (FixF(_), _) => None
  | (TyAlias(p1, t1, x1), TyAlias(p2, t2, x2))
      when TPat.fast_equal(p1, p2) && Typ.fast_equal(t1, t2) =>
    go(x1, x2)
  | (TyAlias(_), _) => None
  | (Ap(d1, x1, y1), Ap(d2, x2, y2)) when d1 == d2 =>
    let* env = go(x1, x2);
    let+ env' = go(y1, y2);
    Environment.union(env, env');
  | (Ap(_), _) => None
  | (TypAp(x1, t1), TypAp(x2, t2)) when Typ.fast_equal(t1, t2) =>
    go(x1, x2)
  | (TypAp(_), _) => None
  | (DeferredAp(_, xs1), DeferredAp(_, xs2))
      when List.length(xs1) != List.length(xs2) =>
    None
  | (DeferredAp(x1, xs1), DeferredAp(x2, xs2)) =>
    List.fold_left2(
      (acc, x, y) => {
        let* env = acc;
        let* env' = go(x, y);
        Some(Environment.union(env, env'));
      },
      go(x1, x2),
      xs1,
      xs2,
    )
  | (DeferredAp(_), _) => None
  | (If(x1, y1, z1), If(x2, y2, z2)) =>
    let* env = go(x1, x2);
    let* env' = go(y1, y2);
    let+ env'' = go(z1, z2);
    Environment.union(env, Environment.union(env', env''));
  | (If(_), _) => None
  | (Seq(x1, y1), Seq(x2, y2)) =>
    let* env = go(x1, x2);
    let+ env' = go(y1, y2);
    Environment.union(env, env');
  | (Seq(_), _) => None
  | (Test(x1), Test(x2)) => go(x1, x2)
  | (Test(_), _) => None
  | (Filter(k1, x1), Filter(k2, x2)) when k1 == k2 => go(x1, x2)
  | (Filter(_), _) => None
  | (Closure(e1, x1), Closure(e2, x2)) when e1 == e2 => go(x1, x2) // we probably shouldn't be comparing environments here
  | (Closure(_), _) => None
  | (Cons(x1, y1), Cons(x2, y2)) =>
    let* env = go(x1, x2);
    let+ env' = go(y1, y2);
    Environment.union(env, env');
  | (Cons(_), _) => None
  | (ListConcat(x1, y1), ListConcat(x2, y2)) =>
    let* env = go(x1, x2);
    let+ env' = go(y1, y2);
    Environment.union(env, env');
  | (ListConcat(_), _) => None
  | (UnOp(o1, x1), UnOp(o2, x2)) when o1 == o2 => go(x1, x2)
  | (UnOp(_), _) => None
  | (BinOp(o1, x1, y1), BinOp(o2, x2, y2)) when o1 == o2 =>
    let* env = go(x1, x2);
    let+ env' = go(y1, y2);
    Environment.union(env, env');
  | (BinOp(_), _) => None
  | (BuiltinFun(x), BuiltinFun(y)) when x == y => Some(Environment.empty)
  | (BuiltinFun(_), _) => None
  | (Match(_, cs1), Match(_, cs2))
      when List.length(cs1) != List.length(cs2) =>
    None
  | (Match(x1, cs1), Match(x2, cs2)) =>
    let* env = go(x1, x2);
    let+ env' =
      List.fold_left2(
        (acc, (p1, x1), (p2, x2)) => {
          let* env = acc;
          let* env' = go(x1, x2);
          Pat.fast_equal(p1, p2)
            ? Some(Environment.union(env, env')) : None;
        },
        Some(Environment.empty),
        cs1,
        cs2,
      );
    Environment.union(env, env');
  | (Match(_), _) => None
  | (Cast(x1, t1, t2), Cast(x2, t3, t4))
      when Typ.fast_equal(t1, t3) && Typ.fast_equal(t2, t4) =>
    go(x1, x2)
  | (Cast(_), _) => None
  };
};
