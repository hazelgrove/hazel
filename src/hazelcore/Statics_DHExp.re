open OptUtil.Syntax;
open HTyp;

let rec ana_pat =
        (ctx: Contexts.t, p: DHPat.t, ty: HTyp.t): option(Contexts.t) => {
  let consistent_with = target_ty =>
    HTyp.consistent(ty, target_ty) ? Some(ctx) : None;
  switch (p) {
  | Var(name) => Some(Contexts.extend_gamma(ctx, (name, ty)))
  | Keyword(_)
  | InvalidText(_)
  | NonEmptyHole(_)
  | EmptyHole(_)
  | Wild => Some(ctx)
  | Triv => consistent_with(Prod([]))
  | ListNil => consistent_with(List(Hole))
  | IntLit(_) => consistent_with(Int)
  | FloatLit(_) => consistent_with(Float)
  | BoolLit(_) => consistent_with(Bool)
  | StringLit(_) => consistent_with(String)
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
  | Ap(_, _) => None // TODO(andrew): check expansion
  };
};

let rec syn = (ctx: Contexts.t, delta: Delta.t, d: DHExp.t): option(HTyp.t) => {
  let syn' = syn(ctx, delta);
  let ana = (d', d_ty) => {
    switch (syn'(d')) {
    | Some(ty) => ty == d_ty
    | None => false
    };
  };
  switch (d) {
  | Triv => Some(Prod([]))
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | ListNil(ty) => Some(List(ty))

  | Cons(d, ds) =>
    let* d_ty = syn'(d);
    let* ds_ty = syn'(ds);
    switch (ds_ty) {
    | List(ty) => ty == d_ty ? Some(List(ty)) : None
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
    // TODO: add Tuples to DHExp Tuple([DHExp]) ; remove Triv
    Some(Prod([d0_ty, d1_ty]));

  | Subscript(str, l, r) =>
    ana(str, String) && ana(l, Int) && ana(r, Int) ? Some(String) : None
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
  | BinStrOp(Caret, a, b) =>
    ana(a, String) && ana(b, String) ? Some(String) : None

  | BoundVar(name) => VarMap.lookup(Contexts.gamma(ctx), name)
  | Ap(f, d) =>
    let* f_ty = syn'(f);
    let* d_ty = syn'(d);
    switch (f_ty) {
    | Arrow(in_ty, out_ty) => d_ty == in_ty ? Some(out_ty) : None
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
          acc_ty == d_ty ? acc : None;
        },
        syn_rule(r),
        rs,
      )
    };
  | FixF(name, ty, body) =>
    let ctx' = Contexts.extend_gamma(ctx, (name, ty));
    let* ty_body = syn(ctx', delta, body);
    ty == ty_body ? Some(ty) : None;
  | ApBuiltin(name, ds) =>
    let* ty_name' = VarCtx.lookup(BuiltinFunctions.ctx, name);
    let* ty_ds = ds |> List.map(syn') |> OptUtil.sequence;
    let rec builtin_ap =
            (fn_ty: HTyp.t, arg_tys: list(HTyp.t)): option(HTyp.t) => {
      switch (arg_tys, fn_ty) {
      | ([], _) => Some(fn_ty)
      | ([ty, ...tys], Arrow(in_ty, out_ty)) =>
        ty == in_ty ? builtin_ap(out_ty, tys) : None
      // TODO: check what evaluator is doing here
      | _ => None
      };
    };
    builtin_ap(ty_name', ty_ds);

  | EmptyHole(u, _, sigma)
  | Keyword(u, _, sigma, _)
  | FreeVar(u, _, sigma, _)
  | InvalidText(u, _, sigma, _)
  | FreeLivelit(u, _, sigma, _) =>
    let (sort, hole_ty, gamma') = MetaVarMap.find(u, delta); // TODO: check sort
    switch (sort) {
    | ExpressionHole =>
      check_substitution_typing(syn', sigma, gamma') ? Some(hole_ty) : None
    | PatternHole => None
    };
  | NonEmptyHole(_, u, _, sigma, d) =>
    let* _ty_d = syn'(d);
    let (_, hole_ty, gamma') = MetaVarMap.find(u, delta);
    check_substitution_typing(syn', sigma, gamma') ? Some(hole_ty) : None;

  | Cast(d', ty1, ty2) =>
    let* ty_d' = syn'(d');
    consistent(ty1, ty2) && ty1 == ty_d' ? Some(ty2) : None;
  | FailedCast(d', ty1, ty2) =>
    // use ground_cases_of
    // read refined account of gradual typing SNAPL2015
    let* ty_d' = syn'(d');
    HTyp.is_ground_type(ty1)
    && HTyp.is_ground_type(ty2)
    && ty1 != ty2
    && ty1 == ty_d'
      ? Some(ty2) : None;

  | InvalidOperation(d, _) => syn'(d)
  | FailedAssert(d) =>
    let* _ty_d = syn'(d);
    Some(Prod([]));
  // same as above; unit type. TODO: check evaluator
  | LivelitHole(_, _, _, _, _, _, _)
  // use expansion type
  // check splices
  // ~ check subst type
  // cc_expansion in livelits paper
  | InconsistentBranches(_, _, _, _) =>
    // check expansion
    // same as consistent, but has to be hole
    None // check rule types synth; if so, hole?
  //TODO: instrument elaborator with this fn
  };
}
and check_substitution_typing =
    (syn', sigma: VarMap.t_(DHExp.t), gamma': VarCtx.t): bool => {
  /* checks judgement from Definition 3.7 in POPL19 */
  VarMap.length(sigma) == VarMap.length(gamma')
  && List.fold_left(
       (acc, (x, ty_gamma)) => {
         switch (VarMap.lookup(sigma, x)) {
         | None => false
         | Some(d) =>
           switch (syn'(d)) {
           | None => false
           | Some(ty_sigma) => acc && ty_gamma == ty_sigma
           }
         }
       },
       true,
       gamma',
     );
};
