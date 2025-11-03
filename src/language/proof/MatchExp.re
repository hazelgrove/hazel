open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type match_ctx = list((string, (Typ.t, option(Exp.t))));

let match_ctx_has = (ctx: match_ctx, name: string): bool =>
  List.exists(((n, (_, _))) => n == name, ctx);

type alphas = list((string, string));

let rec are_alpha_equiv = (alphas: alphas, x: string, y: string): bool =>
  switch (alphas) {
  | [] => x == y
  | [(x1, y1), ..._] when x == x1 && y == y1 => true
  | [(x1, _), ..._] when x == x1 => false
  | [(_, y1), ..._] when y == y1 => false
  | [_, ...rest] => are_alpha_equiv(rest, x, y)
  };

let is_in_alphas_l = (alphas: alphas, x: string): bool =>
  List.exists(((x1, _)) => x == x1, alphas);

/* Match exp against another pattern exp_r.

     - alphas: list of alpha equivalences, e.g. [("x", "y")] means x in exp_r
       is equivalent to y in exp.
     - ctx: context of matched variables in exp_r. Some if already matched,
       None if not matched but can be matched.
     - exp_r: the pattern expression to match against.
     - exp: the expression to match.
   */

let rec match_exp =
        (
          ~info_map: Statics.Map.t,
          alphas: alphas,
          ctx: match_ctx,
          exp_r: Exp.t,
          exp: Exp.t,
        )
        : option(match_ctx) => {
  let match_exp = match_exp(~info_map);
  switch (exp_r |> Exp.term_of, exp |> Exp.term_of) {
  /* Parens */
  | (Parens(e1), _) => match_exp(alphas, ctx, e1, exp)
  | (_, Parens(e2)) => match_exp(alphas, ctx, exp_r, e2)
  | (Probe(e1, _), _) => match_exp(alphas, ctx, e1, exp)
  | (_, Probe(e2, _)) => match_exp(alphas, ctx, exp_r, e2)
  // TODO: Better cast logic
  | (Asc(e1, _), _) => match_exp(alphas, ctx, e1, exp)
  | (_, Asc(e2, _)) => match_exp(alphas, ctx, exp_r, e2)
  /* Variables */
  | (Var(x), _) when match_ctx_has(ctx, x) && !is_in_alphas_l(alphas, x) =>
    let (typ, assigned_exp) = List.assoc(x, ctx);
    let exp_statics = Statics.Map.lookup(Exp.rep_id(exp), info_map);
    switch (exp_statics) {
    | Some(InfoExp({ty: exp_typ, _}))
        when {
          !Typ.is_consistent(Ctx.empty, exp_typ, typ);
        } =>
      None
    | Some(_)
    | None =>
      switch (assigned_exp) {
      | None =>
        Some(
          List.map(
            ((n, (t, e))) =>
              n == x ? (n, (t, Some(exp))) : (n, (t, e)),
            ctx,
          ),
        )
      | Some(e) => match_exp(alphas, ctx, e, exp)
      }
    };
  | (Var(x), Var(y)) when are_alpha_equiv(alphas, x, y) => Some(ctx)
  | (Var(_), _) => None
  /* Forms with binders */
  /* Forms without binders */
  | (Invalid(x), Invalid(y)) when x == y => Some(ctx)
  | (Invalid(_), _) => None
  | (EmptyHole, EmptyHole) => Some(ctx)
  | (EmptyHole, _) => None
  | (MultiHole(xs), MultiHole(ys)) when List.length(xs) == List.length(ys) =>
    let* () =
      ListUtil.fold_left_opt(
        ((), (x, y)) => match_any(ctx, x, y),
        (),
        List.combine(xs, ys),
      );
    Some(ctx);
  | (MultiHole(_), _) => None
  | (DynamicErrorHole(e1, err1), DynamicErrorHole(e2, err2))
      when err1 == err2 =>
    match_exp(alphas, ctx, e1, e2)
  | (DynamicErrorHole(_, _), _) => None
  | (Deferral(_), Deferral(_)) => Some(ctx)
  | (Deferral(_), _) => None
  | (Undefined, Undefined) => Some(ctx)
  | (Undefined, _) => None
  | (Atom(Bool(b1)), Atom(Bool(b2))) when b1 == b2 => Some(ctx)
  | (Atom(Bool(_)), _) => None
  | (Atom(Int(i1)), Atom(Int(i2))) when i1 == i2 => Some(ctx)
  | (Atom(Int(_)), _) => None
  | (Atom(Float(f1)), Atom(Float(f2))) when f1 == f2 => Some(ctx)
  | (Atom(Float(_)), _) => None
  | (Atom(String(s1)), Atom(String(s2))) when s1 == s2 => Some(ctx)
  | (Atom(String(_)), _) => None
  | (Atom(SInt(i1)), Atom(SInt(i2))) when i1 == i2 => Some(ctx)
  | (Atom(SInt(_)), _) => None
  | (Atom(Nat(i1)), Atom(Nat(i2))) when i1 == i2 => Some(ctx)
  | (Atom(Nat(_)), _) => None
  | (ListLit(xs), ListLit(ys)) when List.length(xs) == List.length(ys) =>
    ListUtil.fold_left_opt(
      (ctx, (x, y)) => match_exp(alphas, ctx, x, y),
      ctx,
      List.combine(xs, ys),
    )
  | (ListLit(_), _) => None
  | (Constructor(c1, _), Constructor(c2, _)) when c1 == c2 => Some(ctx)
  | (Constructor(_, _), _) => None
  | (Fun(p1, e1, _, _), Fun(p2, e2, _, _)) =>
    let* alphas' = match_pat(p1, p2);
    match_exp(alphas' @ alphas, ctx, e1, e2);
  | (Fun(_, _, _, _), _) => None
  | (Forall(p1, e1), Forall(p2, e2)) =>
    let* alphas' = match_pat(p1, p2);
    match_exp(alphas' @ alphas, ctx, e1, e2);
  | (Forall(_, _), _) => None
  | (TypFun(tp1, e1, _), TypFun(tp2, e2, _)) =>
    // TODO: type alpha equivalence
    let* () = match_tpat(tp1, tp2);
    match_exp(alphas, ctx, e1, e2);
  | (TypFun(_, _, _), _) => None
  | (Tuple(xs), Tuple(ys)) when List.length(xs) == List.length(ys) =>
    ListUtil.fold_left_opt(
      (ctx, (x, y)) => match_exp(alphas, ctx, x, y),
      ctx,
      List.combine(xs, ys),
    )
  | (Tuple(_), _) => None
  | (TupleExtension(xs1, ys1), TupleExtension(xs2, ys2)) =>
    let* ctx = match_exp(alphas, ctx, xs1, xs2);
    match_exp(alphas, ctx, ys1, ys2);
  | (TupleExtension(_, _), _) => None
  | (Let(p1, d1, e1), Let(p2, d2, e2)) =>
    let* alphas' = match_pat(p1, p2);
    let* ctx = match_exp(alphas, ctx, d1, d2);
    match_exp(alphas' @ alphas, ctx, e1, e2);
  | (Let(_, _, _), _) => None
  | (Theorem(p1, e3, e1), Theorem(p2, e4, e2)) =>
    let* alphas' = match_pat(p1, p2);
    let* ctx = match_exp(alphas, ctx, e3, e4);
    match_exp(alphas' @ alphas, ctx, e1, e2);
  | (Theorem(_), _) => None
  | (ProofObject(e1), ProofObject(e2)) => match_exp(alphas, ctx, e1, e2)
  | (ProofObject(_), _) => None
  | (FixF(p1, e1, _), FixF(p2, e2, _)) =>
    let* alphas' = match_pat(p1, p2);
    match_exp(alphas' @ alphas, ctx, e1, e2);
  | (FixF(_, _, _), _) => None
  | (TyAlias(tp1, t1, e1), TyAlias(tp2, t2, e2)) =>
    // TODO: type alpha equivalence
    let* () = match_tpat(tp1, tp2);
    let* () = match_typ(t1, t2);
    match_exp(alphas, ctx, e1, e2);
  | (TyAlias(_, _, _), _) => None
  | (Ap(d1, e1, e2), Ap(d2, e3, e4)) when d1 == d2 =>
    let* ctx = match_exp(alphas, ctx, e1, e3);
    match_exp(alphas, ctx, e2, e4);
  | (Ap(_, _, _), _) => None
  | (TypAp(e1, t1), TypAp(e2, t2)) =>
    let* () = match_typ(t1, t2);
    match_exp(alphas, ctx, e1, e2);
  | (TypAp(_, _), _) => None
  | (DeferredAp(e1, es1), DeferredAp(e2, es2))
      when List.length(es1) == List.length(es2) =>
    let* ctx = match_exp(alphas, ctx, e1, e2);
    ListUtil.fold_left_opt(
      (ctx, (x, y)) => match_exp(alphas, ctx, x, y),
      ctx,
      List.combine(es1, es2),
    );
  | (DeferredAp(_, _), _) => None
  | (If(e1, e2, e3), If(e4, e5, e6)) =>
    let* ctx = match_exp(alphas, ctx, e1, e4);
    let* ctx = match_exp(alphas, ctx, e2, e5);
    match_exp(alphas, ctx, e3, e6);
  | (If(_, _, _), _) => None
  | (Seq(e1, e2), Seq(e3, e4)) =>
    let* ctx = match_exp(alphas, ctx, e1, e3);
    match_exp(alphas, ctx, e2, e4);
  | (Seq(_, _), _) => None
  | (Test(e1), Test(e2)) => match_exp(alphas, ctx, e1, e2)
  | (Test(_), _) => None
  | (HintedTest(e1, e2), HintedTest(e3, e4)) =>
    let* ctx = match_exp(alphas, ctx, e1, e3);
    match_exp(alphas, ctx, e2, e4);
  | (HintedTest(_), _) => None
  | (Filter(f1, e1), Filter(f2, e2)) when f1 == f2 =>
    match_exp(alphas, ctx, e1, e2)
  | (Filter(_, _), _) => None
  // Note: This code should never be called with closures
  | (Closure(_, e1), Closure(_, e2)) => match_exp(alphas, ctx, e1, e2)
  | (Closure(_, _), _) => None
  | (Cons(e1, e2), Cons(e3, e4)) =>
    let* ctx = match_exp(alphas, ctx, e1, e3);
    match_exp(alphas, ctx, e2, e4);
  | (Cons(_, _), _) => None
  | (ListConcat(e1, e2), ListConcat(e3, e4)) =>
    let* ctx = match_exp(alphas, ctx, e1, e3);
    match_exp(alphas, ctx, e2, e4);
  | (ListConcat(_, _), _) => None
  | (UnOp(op1, e1), UnOp(op2, e2)) when op1 == op2 =>
    match_exp(alphas, ctx, e1, e2)
  | (UnOp(_, _), _) => None
  | (BinOp(op1, e1, e2), BinOp(op2, e3, e4)) when op1 == op2 =>
    let* ctx = match_exp(alphas, ctx, e1, e3);
    match_exp(alphas, ctx, e2, e4);
  | (BinOp(_, _, _), _) => None
  | (BuiltinFun(f1), BuiltinFun(f2)) when f1 == f2 => Some(ctx)
  | (BuiltinFun(_), _) => None
  | (Match(e1, rs1), Match(e2, rs2))
      when List.length(rs1) == List.length(rs2) =>
    let* ctx = match_exp(alphas, ctx, e1, e2);
    ListUtil.fold_left_opt(
      (ctx, ((p1, body1), (p2, body2))) => {
        let* alphas' = match_pat(p1, p2);
        match_exp(alphas' @ alphas, ctx, body1, body2);
      },
      ctx,
      List.combine(rs1, rs2),
    );
  | (Match(_, _), _) => None
  | (Label(l1), Label(l2)) when l1 == l2 => Some(ctx)
  | (Label(_), _) => None
  | (ExplicitNonlabel, ExplicitNonlabel) => Some(ctx)
  | (TupLabel(l1, e1), TupLabel(l2, e2)) when l1 == l2 =>
    match_exp(alphas, ctx, e1, e2)
  | (TupLabel({term: ExplicitNonlabel, _}, e1), _) =>
    match_exp(alphas, ctx, e1, exp)
  | (_, TupLabel({term: ExplicitNonlabel, _}, e2)) =>
    match_exp(alphas, ctx, exp_r, e2)
  | (TupLabel(_, _), _) => None
  | (ExplicitNonlabel, _) => None
  | (Dot(e1, l1), Dot(e2, l2)) when l1 == l2 =>
    match_exp(alphas, ctx, e1, e2)
  | (Dot(_, _), _) => None
  | (LivelitName(l1), LivelitName(l2)) when l1 == l2 => Some(ctx)
  | (LivelitName(_), _) => None
  | (Use(t, e), Use(t2, e2)) =>
    let* () = match_typ(t, t2);
    let* ctx = match_exp(alphas, ctx, e, e2);
    Some(ctx);
  | (Use(_, _), _) => None
  };
}

and match_any = (_: match_ctx, _: Any.t, _: Any.t): option(unit) => None

and match_pat = (pat_r: Pat.t, pat: Pat.t): option(alphas) =>
  switch (pat_r |> Pat.term_of, pat |> Pat.term_of) {
  | (Parens(p1) | Probe(p1, _), _) => match_pat(p1, pat)
  | (_, Parens(p2) | Probe(p2, _)) => match_pat(pat_r, p2)
  | (Invalid(x), Invalid(y)) when x == y => Some([])
  | (Invalid(_), _) => None
  | (EmptyHole, EmptyHole) => Some([])
  | (EmptyHole, _) => None
  // Note: not attempting to match multi-pattern-holes
  | (MultiHole(_), _) => None
  | (Wild, Wild) => Some([])
  | (Wild, Var(x)) => Some([("_", x)])
  | (Var(x), Wild) => Some([(x, "_")])
  | (Wild, Tuple([]))
  | (Tuple([]), Wild) => Some([])
  | (Wild, _) => None
  | (Var(x), Var(y)) => Some([(x, y)])
  | (Var(_), _) => None
  | (Atom(Int(i1)), Atom(Int(i2))) when i1 == i2 => Some([])
  | (Atom(Int(_)), _) => None
  | (Atom(Float(f1)), Atom(Float(f2))) when f1 == f2 => Some([])
  | (Atom(Float(_)), _) => None
  | (Atom(Bool(b1)), Atom(Bool(b2))) when b1 == b2 => Some([])
  | (Atom(Bool(_)), _) => None
  | (Atom(String(s1)), Atom(String(s2))) when s1 == s2 => Some([])
  | (Atom(String(_)), _) => None
  | (Atom(SInt(i1)), Atom(SInt(i2))) when i1 == i2 => Some([])
  | (Atom(SInt(_)), _) => None
  | (Atom(Nat(i1)), Atom(Nat(i2))) when i1 == i2 => Some([])
  | (Atom(Nat(_)), _) => None
  | (ListLit(xs), ListLit(ys)) when List.length(xs) == List.length(ys) =>
    ListUtil.fold_left_opt(
      (alphas, (x, y)) =>
        match_pat(x, y) |> Option.map(alphas1 => alphas1 @ alphas),
      [],
      List.combine(xs, ys),
    )
  | (ListLit(_), _) => None
  | (Constructor(c1, _), Constructor(c2, _)) when c1 == c2 => Some([])
  | (Constructor(_, _), _) => None
  | (Cons(x1, x2), Cons(y1, y2)) =>
    let* alphas1 = match_pat(x1, y1);
    let* alphas2 = match_pat(x2, y2);
    Some(alphas1 @ alphas2);
  | (Cons(_, _), _) => None
  | (Tuple(xs), Tuple(ys)) when List.length(xs) == List.length(ys) =>
    ListUtil.fold_left_opt(
      (alphas, (x, y)) =>
        match_pat(x, y) |> Option.map(alphas1 => alphas1 @ alphas),
      [],
      List.combine(xs, ys),
    )
  | (Tuple(_), _) => None
  | (Ap(x1, x2), Ap(y1, y2)) =>
    let* alphas1 = match_pat(x1, y1);
    let* alphas2 = match_pat(x2, y2);
    Some(alphas1 @ alphas2);
  | (Ap(_, _), _) => None
  | (Asc(x, t1), Asc(y, t2)) =>
    let* alphas1 = match_pat(x, y);
    let* () = match_typ(t1, t2);
    Some(alphas1);
  | (Asc(_, _), _) => None
  | (Label(l1), Label(l2)) when l1 == l2 => Some([])
  | (Label(_), _) => None
  | (TupLabel({term: ExplicitNonlabel, _}, pat_r), _) =>
    match_pat(pat_r, pat)
  | (_, TupLabel({term: ExplicitNonlabel, _}, pat)) =>
    match_pat(pat_r, pat)
  | (ExplicitNonlabel, _) =>
    raise(
      Failure(
        "match_pat ExplicitNonlabel shouldn't show up since they're matched above",
      ),
    )
  | (TupLabel(e1, e2), TupLabel(e3, e4)) =>
    let* alphas1 = match_pat(e1, e3);
    let* alphas2 = match_pat(e2, e4);
    Some(alphas1 @ alphas2);
  | (TupLabel(_, _), _) => None
  }

// TODO complete
and match_typ = (typ_r: Typ.t, typ: Typ.t): option(unit) =>
  if (Typ.fast_equal(typ_r, typ)) {
    Some();
  } else {
    None;
  }

// TODO complete
and match_tpat = (tpat_r: TPat.t, tpat: TPat.t): option(unit) =>
  if (TPat.fast_equal(tpat_r, tpat)) {
    Some();
  } else {
    None;
  }

and match_rul = (_ctx: match_ctx, _rul_r: Rul.t, _rul: Rul.t): option(match_ctx) => None /* TODO */ /* }*/;

let substitute_exp = (sub: match_ctx, exp: Exp.t): Exp.t =>
  Substitution.in_exp(
    Environment.of_bindings(
      List.filter_map(
        ((name, (_, assigned_exp))) =>
          assigned_exp |> Option.map(e => (name, e)),
        sub,
      ),
    ),
    exp,
  );

let match_exp' = match_exp;

/* TODO[Matt]: we can probably make this faster by avoiding substituting
   if we know it doesn't match */
let match_exp =
    (
      ~info_map,
      ~alphas=[],
      ~exp_env: Environment.t(Exp.t),
      ~exp_r_ctx: match_ctx,
      exp_r: Exp.t,
      exp: Exp.t,
    ) => {
  let exp = Substitution.in_exp(exp_env, exp);
  match_exp(~info_map, alphas, exp_r_ctx, exp_r, exp);
};
