module EvalCtx = {
  [@deriving sexp]
  type t =
    | Mark
    | Ap1(t, DHExp.t)
    | Ap2(DHExp.t, t)
    | BinBoolOp1(DHExp.BinBoolOp.t, t, DHExp.t)
    | BinBoolOp2(DHExp.BinBoolOp.t, DHExp.t, t)
    | BinIntOp1(DHExp.BinIntOp.t, t, DHExp.t)
    | BinIntOp2(DHExp.BinIntOp.t, DHExp.t, t)
    | BinFloatOp1(DHExp.BinFloatOp.t, t, DHExp.t)
    | BinFloatOp2(DHExp.BinFloatOp.t, DHExp.t, t)
    | Cons1(t, DHExp.t)
    | Cons2(DHExp.t, t)
    | Pair1(t, DHExp.t)
    | Pair2(DHExp.t, t)
    | Let1(DHPat.t, t, DHExp.t)
    | Let2(DHPat.t, DHExp.t, t)
    | Inj(HTyp.t, InjSide.t, t)
    | NonEmptyHole(
        ErrStatus.HoleReason.t,
        MetaVar.t,
        MetaVarInst.t,
        //TODO: VarMap.t_(t),
        t,
      )
    | Cast(t, HTyp.t, HTyp.t)
    | FailedCast(t, HTyp.t, HTyp.t);
};

let is_ground = (ty: HTyp.t): bool =>
  switch (ty) {
  | Bool
  | Int
  | Float
  | Arrow(Hole, Hole)
  | Sum(Hole, Hole)
  | List(Hole) => true
  | _ => false
  };

let is_val = (d: DHExp.t): bool =>
  switch (d) {
  | FreeVar(_, _, _, _)
  | BoundVar(_)
  | Lam(_, _, _)
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_) => true
  | _ => false
  };

let rec is_boxedval = (d: DHExp.t): bool =>
  switch (d) {
  | Cast(d1, Arrow(ty1, ty2), Arrow(ty3, ty4)) =>
    is_boxedval(d1) && !(HTyp.eq(ty1, ty3) && HTyp.eq(ty2, ty4))
  | Cast(d1, ty, Hole) => is_ground(ty) && is_boxedval(d1)
  | _ => is_val(d)
  };

let rec is_final = (d: DHExp.t): bool => is_boxedval(d) || is_indet(d)
and is_indet = (d: DHExp.t): bool =>
  switch (d) {
  | EmptyHole(_, _, _) => true
  | NonEmptyHole(_, _, _, _, d1) => is_final(d1)
  | Ap(Cast(_, Arrow(_, _), Arrow(_, _)), _) => false
  | Ap(d1, d2) => is_indet(d1) && is_final(d2)
  | Cast(d1, ty, Hole) => is_indet(d1) && is_ground(ty)
  | Cast(Cast(_, _, Hole), Hole, _) => false
  | Cast(d1, Hole, ty) => is_indet(d1) && is_ground(ty)
  | Cast(d, Arrow(ty1, ty2), Arrow(ty3, ty4)) =>
    is_indet(d) && !(HTyp.eq(ty1, ty3) && HTyp.eq(ty2, ty4))
  | FailedCast(d1, ty1, ty2) =>
    is_final(d1) && is_ground(ty1) && is_ground(ty2) && !HTyp.eq(ty1, ty2)
  | _ => false
  };

let rec decompose = (d: DHExp.t): (EvalCtx.t, DHExp.t) =>
  switch (d) {
  | EmptyHole(_, _, _)
  | Keyword(_, _, _, _)
  | FreeVar(_, _, _, _)
  | InvalidText(_, _, _, _)
  | BoundVar(_)
  | Lam(_, _, _)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => (Mark, d)
  | Ap(d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Ap2(d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Ap1(ctx, d2), d0);
    }
  | NonEmptyHole(reason, mvar, mvarinst, _, d1) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (NonEmptyHole(reason, mvar, mvarinst, ctx), d0);
    }
  | BinBoolOp(op, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (BinBoolOp2(op, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (BinBoolOp1(op, ctx, d2), d0);
    }
  | BinIntOp(op, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (BinIntOp2(op, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (BinIntOp1(op, ctx, d2), d0);
    }
  | BinFloatOp(op, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (BinFloatOp2(op, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (BinFloatOp1(op, ctx, d2), d0);
    }
  | Cons(d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Cons2(d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Cons1(ctx, d2), d0);
    }
  | Cast(d1, ty1, ty2) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (Cast(ctx, ty1, ty2), d0);
    }
  | FailedCast(d1, ty1, ty2) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (FailedCast(ctx, ty1, ty2), d0);
    }
  | Pair(d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Pair2(d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Pair1(ctx, d2), d0);
    }
  | Let(dp, d1, d2) =>
    if (is_final(d1)) {
      if (is_final(d2)) {
        (Mark, d);
      } else {
        let (ctx, d0) = decompose(d2);
        (Let2(dp, d1, ctx), d0);
      };
    } else {
      let (ctx, d0) = decompose(d1);
      (Let1(dp, ctx, d2), d0);
    }
  | Inj(ty, side, d1) =>
    if (is_final(d1)) {
      (Mark, d);
    } else {
      let (ctx, d0) = decompose(d1);
      (Inj(ty, side, ctx), d0);
    }
  // | FixF(Var.t, HTyp.t, t)
  // | ConsistentCase(case)
  // | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  // | InvalidOperation(t, InvalidOperationError.t)
  | _ => (Mark, d)
  };

let rec compose = ((ctx, d): (EvalCtx.t, DHExp.t)): DHExp.t =>
  switch (ctx) {
  | Mark => d
  | Ap1(ctx1, d1) => Ap(compose((ctx1, d)), d1)
  | Ap2(d1, ctx1) => Ap(d1, compose((ctx1, d)))
  | BinBoolOp1(op, ctx1, d1) => BinBoolOp(op, compose((ctx1, d)), d1)
  | BinBoolOp2(op, d1, ctx1) => BinBoolOp(op, d1, compose((ctx1, d)))
  | BinIntOp1(op, ctx1, d1) => BinIntOp(op, compose((ctx1, d)), d1)
  | BinIntOp2(op, d1, ctx1) => BinIntOp(op, d1, compose((ctx1, d)))
  | BinFloatOp1(op, ctx1, d1) => BinFloatOp(op, compose((ctx1, d)), d1)
  | BinFloatOp2(op, d1, ctx1) => BinFloatOp(op, d1, compose((ctx1, d)))
  | Cons1(ctx1, d1) => Cons(compose((ctx1, d)), d1)
  | Cons2(d1, ctx1) => Cons(d1, compose((ctx1, d)))
  | Pair1(ctx1, d1) => Pair(compose((ctx1, d)), d1)
  | Pair2(d1, ctx1) => Pair(d1, compose((ctx1, d)))
  | Let1(dp, ctx1, d1) => Let(dp, compose((ctx1, d)), d1)
  | Let2(dp, d1, ctx1) => Let(dp, d1, compose((ctx1, d)))
  | Inj(ty, side, ctx1) => Inj(ty, side, compose((ctx1, d)))

  | Cast(ctx1, ty1, ty2) => Cast(compose((ctx1, d)), ty1, ty2)
  | FailedCast(ctx1, ty1, ty2) => FailedCast(compose((ctx1, d)), ty1, ty2)
  // | NonEmptyHole(
  //     ErrStatus.HoleReason.t,
  //     MetaVar.t,
  //     MetaVarInst.t,
  //     // VarMap.t_(t),
  //     t,
  //   )
  | _ => d
  };

// Copy from Evaluator.re
// Those things will be deleted!

let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
  switch (op) {
  | And => BoolLit(b1 && b2)
  | Or => BoolLit(b1 || b2)
  };

let eval_bin_int_op = (op: DHExp.BinIntOp.t, n1: int, n2: int): DHExp.t => {
  switch (op) {
  | Minus => IntLit(n1 - n2)
  | Plus => IntLit(n1 + n2)
  | Times => IntLit(n1 * n2)
  | Divide => IntLit(n1 / n2)
  | LessThan => BoolLit(n1 < n2)
  | GreaterThan => BoolLit(n1 > n2)
  | Equals => BoolLit(n1 == n2)
  };
};

let eval_bin_float_op =
    (op: DHExp.BinFloatOp.t, f1: float, f2: float): DHExp.t => {
  switch (op) {
  | FPlus => FloatLit(f1 +. f2)
  | FMinus => FloatLit(f1 -. f2)
  | FTimes => FloatLit(f1 *. f2)
  | FDivide => FloatLit(f1 /. f2)
  | FLessThan => BoolLit(f1 < f2)
  | FGreaterThan => BoolLit(f1 > f2)
  | FEquals => BoolLit(f1 == f2)
  };
};

let instruction_step = (d: DHExp.t): option(DHExp.t) =>
  switch (d) {
  // | Ap(d1, d2) =>
  //   switch(d1) {
  //     | Lam(pat, ty, d0) => Some()
  //   }
  | Let(dp, d1, d2) =>
    switch (Elaborator_Exp.matches(dp, d1)) {
    | Indet => None // maybe they should be errors
    | DoesNotMatch => None
    | Matches(env) => Some(Elaborator_Exp.subst(env, d2))
    }
  | BinBoolOp(op, d1, d2) =>
    switch (d1, d2) {
    | (DHExp.BoolLit(b1), DHExp.BoolLit(b2)) =>
      Some(eval_bin_bool_op(op, b1, b2))
    | _ => None
    }
  | BinIntOp(op, d1, d2) =>
    switch (d1, d2) {
    | (DHExp.IntLit(i1), DHExp.IntLit(i2)) =>
      Some(eval_bin_int_op(op, i1, i2))
    | _ => None
    }
  | BinFloatOp(op, d1, d2) =>
    switch (d1, d2) {
    | (DHExp.FloatLit(f1), DHExp.FloatLit(f2)) =>
      Some(eval_bin_float_op(op, f1, f2))
    | _ => None
    }
  // | FixF(Var.t, HTyp.t, t)
  // | Ap(t, t)
  // | Inj(HTyp.t, InjSide.t, t)
  // | Lam(DHPat.t, HTyp.t, t)
  // | Cast(t, HTyp.t, HTyp.t)
  // | FailedCast(t, HTyp.t, HTyp.t)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Cons(_, _)
  | Pair(_, _)
  | Triv
  | ConsistentCase(_)
  // | InconsistentBranches(MetaVar.t, MetaVarInst.t, VarMap.t_(t), case)
  | InvalidOperation(_, _)
  | _ => None
  };

let evaluate_step = (d: DHExp.t): option(DHExp.t) => {
  let (ctx, d0) = decompose(d);
  let res = instruction_step(d0);
  if (res == None) {
    None;
  } else {
    Some(compose((ctx, d)));
  };
};
