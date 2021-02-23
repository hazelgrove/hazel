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
  | NonEmptyHole(TypeInconsistent(_) | WrongLength, _, _, _) // TODO(andrew): wronglength case?
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
  | Ap(_, _) => None // TODO(andrew): Why is this a thing?
  };
};

let rec syn = (ctx: Contexts.t, delta: Delta.t, d: DHExp.t): option(HTyp.t) => {
  let syn' = syn(ctx, delta);
  let ana = (d', d_ty) => {
    switch (syn'(d')) {
    | Some(ty) => HTyp.consistent(ty, d_ty)
    | None => false
    };
  };
  switch (d) {
  | Triv => Some(Prod([]))
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | ListNil(_) => Some(List(Hole))

  | Cons(d, ds) =>
    let* d_ty = syn'(d);
    let* ds_ty = syn'(ds);
    switch (ds_ty) {
    | List(ty) => HTyp.consistent(ty, d_ty) ? Some(List(ty)) : None
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
    | Arrow(in_ty, out_ty) =>
      HTyp.consistent(d_ty, in_ty) ? Some(out_ty) : None
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
  | ConsistentCase(Case(scrut, rules, _n)) =>
    // TODO(andrew): what is n?
    let* scrut_ty = syn'(scrut);
    List.fold_left(
      (acc, DHExp.Rule(p, d)) => {
        let* acc_ty = acc;
        let* ctx' = ana_pat(ctx, p, scrut_ty);
        let* d_ty = syn(ctx', delta, d);
        HTyp.join(GLB, acc_ty, d_ty);
      }, //TODO(andrew): right join?
      Some(HTyp.Hole),
      rules,
    );
  | FixF(name, ty_p, body) =>
    let ctx' = Contexts.extend_gamma(ctx, (name, ty_p));
    let* body_ty = syn(ctx', delta, body);
    Some(Arrow(ty_p, body_ty));
  | ApBuiltin(name, ds) =>
    let* ty_name' = VarCtx.lookup(BuiltinFunctions.ctx, name);
    let* ty_ds = ds |> List.map(syn') |> OptUtil.sequence;
    let rec builtin_ap =
            (fn_ty: HTyp.t, arg_tys: list(HTyp.t)): option(HTyp.t) => {
      switch (arg_tys, fn_ty) {
      | ([], _) => Some(fn_ty)
      | ([ty, ...tys], Arrow(in_ty, out_ty)) =>
        HTyp.consistent(ty, in_ty) ? builtin_ap(out_ty, tys) : None
      | _ => None
      };
    };
    builtin_ap(ty_name', ty_ds);

  | EmptyHole(u, _, sigma)
  | Keyword(u, _, sigma, _)
  | FreeVar(u, _, sigma, _)
  | InvalidText(u, _, sigma, _)
  | FreeLivelit(u, _, sigma, _) =>
    let (_, hole_ty, gamma') = MetaVarMap.find(u, delta);
    check_substitution_typing(syn', sigma, gamma') ? Some(hole_ty) : None;
  | NonEmptyHole(TypeInconsistent(_), u, _, sigma, d) =>
    let* _ty_d = syn'(d);
    let (_, hole_ty, gamma') = MetaVarMap.find(u, delta);
    check_substitution_typing(syn', sigma, gamma') ? Some(hole_ty) : None;

  | Cast(d', ty1, ty2) =>
    let* ty_d' = syn'(d');
    HTyp.consistent(ty1, ty_d') ? Some(ty2) : None;
  | FailedCast(d', ty1, ty2) =>
    let* ty_d' = syn'(d');
    HTyp.consistent(ty1, ty_d') && ty1 != ty2 ? Some(ty2) : None;

  // TODO(andrew): Figure out these cases
  | NonEmptyHole(WrongLength, _, _, _, _)
  | InvalidOperation(_)
  | FailedAssert(_)
  | LivelitHole(_, _, _, _, _, _, _)
  | InconsistentBranches(_, _, _, _) => None
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
           | Some(ty_sigma) => acc && HTyp.consistent(ty_gamma, ty_sigma)
           }
         }
       },
       true,
       gamma',
     );
};
