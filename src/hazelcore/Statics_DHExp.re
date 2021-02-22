open OptUtil.Syntax;
open HTyp;

let rec ana_pat =
        (ctx: Contexts.t, p: DHPat.t, ty: HTyp.t): option(Contexts.t) => {
  let consistent_with = target_ty =>
    HTyp.consistent(ty, target_ty) ? Some(ctx) : None;
  switch (p) {
  | Var(name) => Some(Contexts.extend_gamma(ctx, (name, ty)))
  | Keyword(_) // TODO(andrew): ?
  | InvalidText(_) // TODO(andrew): ?
  | NonEmptyHole(TypeInconsistent(_) | WrongLength,_,_,_) // TODO(andrew): first fine, second not?
  | EmptyHole(_)
  | Wild => Some(ctx)
  | Triv => consistent_with(Prod([]))
  | ListNil => consistent_with(List(Hole))
  | IntLit(_) => consistent_with(Int)
  | FloatLit(_) => consistent_with(Float)
  | BoolLit(_) => consistent_with(Bool)
  | StringLit(_) => consistent_with(String)
  | Pair(a, b) =>
    switch (ty) {
    | Prod([a_ty, b_ty]) =>
      let* ctx' = ana_pat(ctx, a, a_ty);
      ana_pat(ctx', b, b_ty);
    | _ => None
    }
  | Cons(x, xs) =>
    switch (ty) {
    | List(ty') =>
      let* ctx' = ana_pat(ctx, x, ty');
      ana_pat(ctx', xs, ty);
    | _ => None
    }
  | Inj(_, p') => ana_pat(ctx, p', ty) // TODO(andrew): verify
  | Ap(_, _) // TODO(andrew): Why is this a thing?
   => None
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
  | Cons(x, xs) =>
    let* x_ty = syn'(x);
    let* xs_ty = syn'(xs);
    switch (xs_ty) {
    | List(ty) => HTyp.consistent(ty, x_ty) ? Some(List(ty)) : None
    | _ => None
    };
  | Inj(ty_side, side, d') =>
    let* ty = syn'(d');
    switch (side) {
    | L => Some(Sum(ty, ty_side))
    | R => Some(Sum(ty_side, ty))
    };
  | Pair(a, b) =>
    let* a_ty = syn'(a);
    let* b_ty = syn'(b);
    Some(Prod([a_ty, b_ty]));

  | Subscript(str, start, last) =>
    ana(str, String) && ana(start, Int) && ana(last, Int)
      ? Some(String) : None
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
  | Ap(f, x) =>
    let* f_ty = syn'(f);
    let* a_ty = syn'(x);
    switch (f_ty) {
    | Arrow(in_ty, out_ty) =>
      HTyp.consistent(a_ty, in_ty) ? Some(out_ty) : None
    | _ => None
    };
  | Lam(p, p_ty, body) =>
    let* body_ctx = ana_pat(ctx, p, p_ty);
    let* body_ty = syn(body_ctx, delta, body);
    Some(Arrow(p_ty, body_ty));
  | Let(p, def, body) =>
    let* def_ty = syn'(def);
    let* ctx' = ana_pat(ctx, p, def_ty);
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
    let* ty_ds = OptUtil.sequence(List.map(syn', ds));
    let rec partial_ap =
            (fn_ty: HTyp.t, arg_tys: list(HTyp.t)): option(HTyp.t) => {
      switch (arg_tys, fn_ty) {
      | ([], _) => Some(fn_ty)
      | ([ty, ...tys], Arrow(in_ty, out_ty)) =>
        HTyp.consistent(ty, in_ty) ? partial_ap(out_ty, tys) : None
      | _ => None
      };
    };
    partial_ap(ty_name', ty_ds);

  | Cast(d', ty1, ty2) =>
    let* ty_d' = syn'(d');
    HTyp.consistent(ty1, ty_d') ? Some(ty2) : None;

  // TODO(andrew): Figure out these cases
  | FailedCast(_, _, _)
  | InvalidOperation(_)
  | FailedAssert(_)
  | Keyword(_, _, _, _)
  | FreeVar(_, _, _, _)
  | FreeLivelit(_, _, _, _)
  | InvalidText(_)
  | EmptyHole(_, _, _)
  | NonEmptyHole(_, _, _, _, _)
  | LivelitHole(_)
  | InconsistentBranches(_, _, _, _) => None
  };
};
