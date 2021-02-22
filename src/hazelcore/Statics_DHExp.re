open OptUtil.Syntax;
open HTyp;

let rec ana_pat =
        (ctx: Contexts.t, p: DHPat.t, ty: HTyp.t): option(Contexts.t) => {
  let consistent_with = target_ty =>
    HTyp.consistent(ty, target_ty) ? Some(ctx) : None;
  switch (p) {
  | Triv => consistent_with(Prod([]))
  | Var(_)
  | Wild
  | EmptyHole(_) => consistent_with(Hole)
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
  | Inj(_, p') => ana_pat(ctx, p', Hole) // TODO(andrew): ???
  // TODO(andrew): what's the deal with these:
  | Ap(_, _)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_) => None
  };
};

let rec syn = (ctx: Contexts.t, d: DHExp.t): option(HTyp.t) => {
  let ana = (d', d_ty) => {
    switch (syn(ctx, d')) {
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
    let* x_ty = syn(ctx, x);
    let* xs_ty = syn(ctx, xs);
    switch (xs_ty) {
    | List(ty) => HTyp.consistent(ty, x_ty) ? Some(List(ty)) : None
    | _ => None
    };
  | Inj(ty_side, side, d') =>
    let* ty = syn(ctx, d');
    switch (side) {
    | L => Some(Sum(ty, ty_side))
    | R => Some(Sum(ty_side, ty))
    };
  | Pair(a, b) =>
    let* a_ty = syn(ctx, a);
    let* b_ty = syn(ctx, b);
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

  | Ap(f, x) =>
    let* f_ty = syn(ctx, f);
    let* a_ty = syn(ctx, x);
    switch (f_ty) {
    | Arrow(in_ty, out_ty) =>
      HTyp.consistent(a_ty, in_ty) ? Some(out_ty) : None
    | _ => None
    };
  | Lam(p, p_ty, body) =>
    let* body_ctx = ana_pat(ctx, p, p_ty);
    let* body_ty = syn(body_ctx, body);
    Some(Arrow(p_ty, body_ty));
  | Let(p, def, body) =>
    let* def_ty = syn(ctx, def);
    let* ctx' = ana_pat(ctx, p, def_ty);
    syn(ctx', body);

  | FixF(_, _, _) => failwith("TODO")
  | ConsistentCase(_) => failwith("TODO")
  | BoundVar(_) => failwith("TODO")
  | ApBuiltin(_, _) => failwith("TODO")
  | Cast(_, _, _) => failwith("TODO")

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
