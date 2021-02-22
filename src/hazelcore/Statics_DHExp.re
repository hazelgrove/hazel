open OptUtil.Syntax;
open HTyp;

let rec syn_pat = (ctx: Contexts.t, p: DHPat.t): option(HTyp.t) => {
  switch (p) {
  | Triv => Some(Prod([]))
  | Var(_)
  | Wild
  | EmptyHole(_) => Some(Hole)
  | ListNil => Some(List(Hole))
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | BoolLit(_) => Some(Bool)
  | StringLit(_) => Some(String)
  | Pair(a, b) =>
    let* a_ty = syn_pat(ctx, a);
    let* b_ty = syn_pat(ctx, b);
    Some(Prod([a_ty, b_ty]));
  | Cons(x, xs) =>
    let* x_ty = syn_pat(ctx, x);
    let* xs_ty = syn_pat(ctx, xs);
    HTyp.consistent(List(x_ty), xs_ty) ? Some(xs_ty) : None;
  | Inj(_, p') => syn_pat(ctx, p')
  // TODO(andrew): do these None cases make sense?
  | Ap(_, _)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_) => None
  };
}
and ana_pat = (ctx: Contexts.t, p: DHPat.t, ty: HTyp.t): option(Contexts.t) => {
  switch (p) {
  | Var(name) => Some(Contexts.extend_gamma(ctx, (name, ty)))
  | Triv
  | Wild
  | EmptyHole(_)
  | ListNil
  | IntLit(_)
  | FloatLit(_)
  | BoolLit(_)
  | StringLit(_)
  | Pair(_)
  | Cons(_)
  | Inj(_)
  | Ap(_, _)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_) =>
    switch (syn_pat(ctx, p)) {
    | None => None
    | Some(ty) => Some(ctx)
    }
  };
};

let rec syn = (ctx: Contexts.t, d: DHExp.t): option(HTyp.t) => {
  let _syn_bin =
      (
        (a: DHExp.t, a_ty: HTyp.t),
        (b: DHExp.t, b_ty: HTyp.t),
        ret_ty: HTyp.t,
      )
      : option(HTyp.t) =>
    ana(ctx, a, a_ty) && ana(ctx, b, b_ty) ? Some(ret_ty) : None;
  switch (d) {
  | Triv => Some(Prod([]))
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | ListNil(_) => Some(List(Hole))
  | Cons(x, xs) =>
    let* x_ty = syn(ctx, x);
    ana(ctx, xs, List(x_ty)) ? Some(List(x_ty)) : None;
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
    ana(ctx, str, String) && ana(ctx, start, Int) && ana(ctx, last, Int)
      ? Some(String) : None
  | BinBoolOp(And | Or, a, b) =>
    ana(ctx, a, Bool) && ana(ctx, b, Bool) ? Some(Bool) : None
  | BinIntOp(Plus | Minus | Times | Divide, a, b) =>
    ana(ctx, a, Int) && ana(ctx, b, Int) ? Some(Int) : None
  | BinIntOp(LessThan | GreaterThan | Equals, a, b) =>
    ana(ctx, a, Int) && ana(ctx, b, Int) ? Some(Bool) : None
  | BinFloatOp(FPlus | FMinus | FTimes | FDivide, a, b) =>
    ana(ctx, a, Float) && ana(ctx, b, Float) ? Some(Float) : None
  | BinFloatOp(FLessThan | FGreaterThan | FEquals, a, b) =>
    ana(ctx, a, Float) && ana(ctx, b, Float) ? Some(Bool) : None
  | BinStrOp(Caret, a, b) =>
    ana(ctx, a, String) && ana(ctx, b, String) ? Some(String) : None

  | Ap(f, x) =>
    let* f_ty = syn(ctx, f);
    let* a_ty = syn(ctx, x);
    switch (f_ty) {
    | Arrow(in_ty, out_ty) =>
      HTyp.consistent(a_ty, in_ty) ? Some(out_ty) : None
    | _ => None
    };
  | Lam(p, p_ty_ann, body) =>
    let* p_ty = syn_pat(ctx, p);
    let* body_ctx = ana_pat(ctx, p, p_ty_ann); // implicit check consistency?
    let* body_ty = syn(body_ctx, body); // TODO(andrew): need to add to ctx
    Some(Arrow(p_ty, body_ty));
  | Let(p, def, body) =>
    let* p_ty = syn_pat(ctx, p); // extend def ctx?
    let* def_ty = syn(ctx, def);
    let* ctx' = ana_pat(ctx, p, def_ty);
    // TODO(andrew): check consistency
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
}

and ana = (ctx: Contexts.t, d: DHExp.t, ty: HTyp.t): bool => {
  let _ana_all = ls =>
    List.fold_left((acc, (d, ty)) => acc && ana(ctx, d, ty), true, ls);
  switch (d) {
  | Triv
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | StringLit(_)
  | ListNil(_)
  | Inj(_)
  | Pair(_)
  | Subscript(_)
  | BinBoolOp(_)
  | BinIntOp(_)
  | BinFloatOp(_)
  | BinStrOp(_) =>
    switch (syn(ctx, d)) {
    | Some(ty_syn) => HTyp.consistent(ty, ty_syn)
    | None => false
    }
  | Cons(x, xs) =>
    switch (ty) {
    | List(ty') => ana(ctx, x, ty') && ana(ctx, xs, ty)
    | _ => false
    }

  | Let(_, _, _) => failwith("TODO") // TODO: statics_DHPat
  | FixF(_, _, _) => failwith("TODO")
  | Lam(_, _, _) => failwith("TODO")
  | Ap(_f, _x) => failwith("TODO")
  //ana(ctx, f, Arrow(Hole, ty))
  //&& ana(ctx, x, )

  | ConsistentCase(_) => failwith("TODO")

  | BoundVar(_) => failwith("TODO")

  | ApBuiltin(_, _) => failwith("TODO")

  | Cast(_, _, _) => failwith("TODO")

  | FailedCast(_, _, _) => failwith("TODO")
  | InvalidOperation(_) => failwith("TODO")
  | FailedAssert(_) => failwith("TODO")

  // TODO(andrew): Figure out these cases
  // or defer to syn
  | Keyword(_, _, _, _)
  | FreeVar(_, _, _, _)
  | FreeLivelit(_, _, _, _)
  | InvalidText(_)
  | EmptyHole(_, _, _)
  | NonEmptyHole(_, _, _, _, _)
  | LivelitHole(_)
  | InconsistentBranches(_, _, _, _) => false
  };
};

/* | Inj(ty_ann, side, d') =>
         // TODO(andrew): ty_ann interpretation correct?
         switch (ty, side) {
         | (Sum(ty_l, ty_r), L) =>
           ana(ctx, d', ty_l) && HTyp.consistent(ty_r, ty_ann)
         | (Sum(ty_l, ty_r), R) =>
           ana(ctx, d', ty_r) && HTyp.consistent(ty_l, ty_ann)
         | _ => false
         }
      | Pair(x, xs) =>
        switch (ty) {
        | Prod([ty_x, ...ty_xs]) =>
          ana(ctx, x, ty_x) && ana(ctx, xs, Prod(ty_xs))
        | _ => false
        }
   | Triv => consistent_with(Prod([]))
   | Subscript(str, start, last) =>
     consistent_with(String)
     && ana(ctx, str, String)
     && ana(ctx, start, Int)
     && ana(ctx, last, Int)
   | BinBoolOp(And | Or, a, b) =>
     consistent_with(Bool) && ana(ctx, a, Bool) && ana(ctx, b, Bool)
   | BinIntOp(Plus | Minus | Times | Divide, a, b) =>
     consistent_with(Int) && ana(ctx, a, Int) && ana(ctx, b, Int)
   | BinIntOp(LessThan | GreaterThan | Equals, a, b) =>
     consistent_with(Bool) && ana(ctx, a, Int) && ana(ctx, b, Int)
   | BinFloatOp(FPlus | FMinus | FTimes | FDivide, a, b) =>
     consistent_with(Float) && ana(ctx, a, Float) && ana(ctx, b, Float)
   | BinFloatOp(FLessThan | FGreaterThan | FEquals, a, b) =>
     consistent_with(Bool) && ana(ctx, a, Float) && ana(ctx, b, Float)
   | BinStrOp(Caret, a, b) =>
     consistent_with(String) && ana(ctx, a, String) && ana(ctx, b, String)*/
