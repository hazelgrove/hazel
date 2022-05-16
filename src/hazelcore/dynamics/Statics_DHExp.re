open OptUtil.Syntax;
open HTyp;

let rec ana_pat =
        (ctx: Contexts.t, p: DHPat.t, ty: HTyp.t): option(Contexts.t) => {
  let eq_to = target_ty => HTyp.eq(ty, target_ty) ? Some(ctx) : None;
  switch (p) {
  | Var(name) => Some(Contexts.extend_gamma(ctx, (name, ty)))
  | Keyword(_)
  | InvalidText(_)
  | EmptyHole(_)
  | Wild => Some(ctx)
  | NonEmptyHole(_, _, _, p) => ana_pat(ctx, p, Hole)
  | Triv => eq_to(Prod([]))
  | ListNil => eq_to(List(ty))
  | IntLit(_) => eq_to(Int)
  | FloatLit(_) => eq_to(Float)
  | BoolLit(_) => eq_to(Bool)
  | Pair(d0, d1) =>
    switch (ty) {
    | Prod([d0_ty, d1_ty]) =>
      let* ctx' = ana_pat(ctx, d0, d0_ty);
      ana_pat(ctx', d1, d1_ty);
    | _ => None
    }
  | Cons(d, ds) =>
    switch (ty) {
    | List(ty') =>
      let* ctx' = ana_pat(ctx, d, ty');
      ana_pat(ctx', ds, ty);
    | _ => None
    }
  | Inj(side, d) =>
    switch (side, ty) {
    | (L, Sum(ty, _))
    | (R, Sum(_, ty)) => ana_pat(ctx, d, ty)
    | _ => None
    }
  | Ap(_, _) => None
  };
};

let rec syn = (ctx: Contexts.t, delta: Delta.t, d: DHExp.t): option(HTyp.t) => {
  //TODO: instrument elaborator with this as a check
  let syn' = syn(ctx, delta);
  let ana = (d', d_ty) => {
    switch (syn'(d')) {
    | Some(ty) => HTyp.eq(ty, d_ty)
    | None => false
    };
  };
  switch (d) {
  | Triv => Some(Prod([]))
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | ListNil(ty) => Some(List(ty))

  | Cons(d, ds) =>
    let* d_ty = syn'(d);
    let* ds_ty = syn'(ds);
    switch (ds_ty) {
    | List(ty) => HTyp.eq(ty, d_ty) ? Some(List(ty)) : None
    | _ => None
    };
  | Inj(ty_side, side, d) =>
    let* ty = syn'(d);
    switch (side) {
    | L => Some(Sum(ty, ty_side))
    | R => Some(Sum(ty_side, ty))
    };
  | Pair(d0, d1) =>
    let* d0_ty = syn'(d0);
    let* d1_ty = syn'(d1);
    // TODO: this isn't quite right. add Tuples to DHExp Tuple([DHExp]) ; remove Triv
    switch (d1_ty) {
    | Prod(xs) => Some(Prod([d0_ty, ...xs]))
    | _ => Some(Prod([d0_ty, d1_ty]))
    };

  | BinBoolOp(And | Or, a, b) =>
    ana(a, Bool) && ana(b, Bool) ? Some(Bool) : None
  | BinIntOp(Plus | Minus | Times | Divide, a, b) =>
    ana(a, Int) && ana(b, Int) ? Some(Int) : None
  | BinIntOp(LessThan | GreaterThan | Equals, a, b) =>
    ana(a, Int) && ana(b, Int) ? Some(Bool) : None
  | BinFloatOp(FPlus | FMinus | FTimes | FDivide, a, b) =>
    ana(a, Float) && ana(b, Float) ? Some(Float) : None
  | BinFloatOp(FLessThan | FGreaterThan | FEquals, a, b) =>
    ana(a, Float) && ana(b, Float) ? Some(Bool) : None

  | BoundVar(name) => VarMap.lookup(Contexts.gamma(ctx), name)
  | Ap(f, d) =>
    let* f_ty = syn'(f);
    let* d_ty = syn'(d);
    switch (f_ty) {
    | Arrow(in_ty, out_ty) => HTyp.eq(d_ty, in_ty) ? Some(out_ty) : None
    | _ => None
    };
  | Lam(p, p_ty, body) =>
    let* body_ctx = ana_pat(ctx, p, p_ty);
    let* body_ty = syn(body_ctx, delta, body);
    Some(Arrow(p_ty, body_ty));
  | Let(p, d, body) =>
    let* d_ty = syn'(d);
    let* ctx' = ana_pat(ctx, p, d_ty);
    syn(ctx', delta, body);
  | ConsistentCase(Case(scrut, rules, n)) =>
    let* _guard = List.length(rules) > n ? Some() : None;
    let* scrut_ty = syn'(scrut);
    let syn_rule = (DHExp.Rule(p, d)) => {
      let* ctx' = ana_pat(ctx, p, scrut_ty);
      syn(ctx', delta, d);
    };
    switch (rules) {
    | [] => None
    | [r, ...rs] =>
      List.fold_left(
        (acc, rule) => {
          let* acc_ty = acc;
          let* d_ty = syn_rule(rule);
          HTyp.eq(acc_ty, d_ty) ? acc : None;
        },
        syn_rule(r),
        rs,
      )
    };
  | FixF(name, ty, body) =>
    let ctx' = Contexts.extend_gamma(ctx, (name, ty));
    let* ty_body = syn(ctx', delta, body);
    HTyp.eq(ty, ty_body) ? Some(ty) : None;
  | ApBuiltin(name, _) => VarCtx.lookup(Builtins.ctx, name)

  | EmptyHole(u, _, sigma)
  | Keyword(u, _, sigma, _)
  | FreeVar(u, _, sigma, _)
  | InvalidText(u, _, sigma, _) => syn_hole(ctx, u, delta, sigma)
  | NonEmptyHole(_, u, _, sigma, d) =>
    let* _ty_d = syn'(d);
    syn_hole(ctx, u, delta, sigma);
  | InconsistentBranches(u, _, sigma, case) =>
    let* ty = syn'(ConsistentCase(case));
    HTyp.eq(ty, Hole) ? syn_hole(ctx, u, delta, sigma) : None;

  | Cast(d', ty1, ty2) =>
    let* ty_d' = syn'(d');
    consistent(ty1, ty2) && HTyp.eq(ty1, ty_d') ? Some(ty2) : None;
  | FailedCast(d', ty1, ty2) =>
    // TODO(andrew): read refined account of gradual typing SNAPL2015
    let* ty_d' = syn'(d');
    HTyp.is_ground(ty1)
    && HTyp.is_ground(ty2)
    && !HTyp.eq(ty1, ty2)
    && HTyp.eq(ty1, ty_d')
      ? Some(ty2) : None;

  | InvalidOperation(d, _) => syn'(d)
  };
}
and syn_hole = (ctx, u, delta, sigma) => {
  let (sort, hole_ty, gamma') = MetaVarMap.find(u, delta);
  switch (sort) {
  | ExpressionHole =>
    check_substitution_typing(ctx, delta, sigma, gamma')
      ? Some(hole_ty) : None
  | PatternHole => None
  };
}
and check_substitution_typing =
    (ctx, delta, sigma: VarMap.t_(DHExp.t), gamma': VarCtx.t): bool => {
  /* checks judgement from Definition 3.7 in POPL19 */
  VarMap.length(sigma) == VarMap.length(gamma')
  && List.fold_left(
       (acc, (x, ty_gamma)) => {
         switch (VarMap.lookup(sigma, x)) {
         | None => false
         | Some(d) =>
           switch (syn(ctx, delta, d)) {
           | None => false
           | Some(ty_sigma) => acc && HTyp.eq(ty_gamma, ty_sigma)
           }
         }
       },
       true,
       gamma',
     );
};
