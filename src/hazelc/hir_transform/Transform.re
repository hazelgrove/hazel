module HDelta = Delta;
module HInjSide = InjSide;

open Hir_expr;
open Hir_expr.Expr;
open Hir_expr.Typ;

open TransformMonad;
open TransformMonad.Syntax;

let rec transform_typ: HTyp.t => Typ.t =
  fun
  | Hole => THole
  | Int => TInt
  | Float => TFloat
  | Bool => TBool
  | Arrow(t1, t2) => TArrow(transform_typ(t1), transform_typ(t2))
  | Sum(t1, t2) => TSum(transform_typ(t1), transform_typ(t2))
  | Prod([t1, t2]) => TPair(transform_typ(t1), transform_typ(t2))
  | Prod([]) => TUnit
  | Prod(_) => failwith("non-pair or unit product type not supported")
  | List(t') => TList(transform_typ(t'));
let transform_var = Ident.v;
let transform_hole_reason: ErrStatus.HoleReason.t => Holes.HoleReason.t =
  fun
  | TypeInconsistent => TypeInconsistent
  | WrongLength => WrongLength;
let transform_expanding_keyword: ExpandingKeyword.t => Holes.ExpandingKeyword.t =
  fun
  | Let => Let
  | Case => Case
  | Fun => Fun;
let transform_invalid_operation_error:
  InvalidOperationError.t => Holes.InvalidOperationError.t =
  fun
  | DivideByZero => DivideByZero;

let transform_delta = (delta: HDelta.t) =>
  delta
  |> MetaVarMap.bindings
  |> List.map(((u, (sort, ty, gamma))) => {
       let sort =
         switch (sort) {
         | HDelta.ExpressionHole => Delta.ExpressionHole
         | HDelta.PatternHole => Delta.PatternHole
         };
       let ty = transform_typ(ty);
       let gamma =
         gamma
         |> VarCtx.to_list
         |> List.map(((x, ty)) => (transform_var(x), transform_typ(ty)))
         |> List.to_seq
         |> TypContext.of_seq;
       (u, (sort, ty, gamma));
     })
  |> List.to_seq
  |> Holes.MetaVarMap.of_seq
  |> return;

let rec transform_exp = (d: DHExp.t) => {
  switch (d) {
  | EmptyHole(u, i, sigma) =>
    let* sigma = transform_var_map(sigma);
    let+ label = next_expr_label;
    {kind: EEmptyHole(u, i, sigma), label};

  | NonEmptyHole(reason, u, i, sigma, d') =>
    let reason = transform_hole_reason(reason);
    let* sigma = transform_var_map(sigma);
    let* e' = transform_exp(d');
    let+ label = next_expr_label;
    {kind: ENonEmptyHole(reason, u, i, sigma, e'), label};

  | ExpandingKeyword(u, i, sigma, k) =>
    let k = transform_expanding_keyword(k);
    let* sigma = transform_var_map(sigma);
    let+ label = next_expr_label;
    {kind: EKeyword(u, i, sigma, k), label};

  | FreeVar(u, i, sigma, x) =>
    let* sigma = transform_var_map(sigma);
    let x = Ident.v(x);
    let+ label = next_expr_label;
    {kind: EFreeVar(u, i, sigma, x), label};

  | InvalidText(u, i, sigma, text) =>
    let* sigma = transform_var_map(sigma);
    let+ label = next_expr_label;
    {kind: EInvalidText(u, i, sigma, text), label};

  | BoundVar(x) =>
    let x = transform_var(x);
    let+ label = next_expr_label;
    {kind: EBoundVar(x), label};

  | FixF(_) => failwith("lone FixF")

  /* TODO: Not really sure if any of this recursive function handling is
   * right... */
  | Let(Var(_), FixF(x, ty, Fun(dp, dp_ty, d3)), body) =>
    let x = transform_var(x);
    let p_ty = transform_typ(dp_ty);
    let* p = transform_pat(dp);

    let o_ty =
      switch (ty) {
      | Arrow(i_ty, o_ty) when HTyp.eq(i_ty, dp_ty) => transform_typ(o_ty)
      | _ => failwith("invalid function fixf type")
      };

    let* e3 = transform_exp(d3);
    let* body = transform_exp(body);
    let+ label = next_expr_label;
    {kind: ELetRec(x, p, p_ty, o_ty, e3, body), label};

  | Let(dp, d', body) =>
    let* e' = transform_exp(d');
    let* p = transform_pat(dp);
    let* body = transform_exp(body);
    let+ label = next_expr_label;
    {kind: ELet(p, e', body), label};

  | Fun(dp, dp_ty, body) =>
    let p_ty = transform_typ(dp_ty);
    let* p = transform_pat(dp);
    let* body = transform_exp(body);
    let+ label = next_expr_label;
    {kind: EFun(p, p_ty, body), label};

  | Ap(fn, arg) =>
    let* fn = transform_exp(fn);
    let* arg = transform_exp(arg);
    switch (fn.kind) {
    // TODO: expand arrow casts and do transform_exp recursively here
    | ECast(_fn, _ty1, _ty2) => failwith("FnCastExpansion")
    | EFun(_, _, _) =>
      let+ label = next_expr_label;
      {kind: EAp(fn, arg), label};
    | _ => failwith("NotImplemented")
    };

  | ApBuiltin(name, args) =>
    let name = transform_var(name);
    let* args = args |> List.map(transform_exp) |> sequence;

    let+ label = next_expr_label;
    {kind: EApBuiltin(name, args), label};

  | BinBoolOp(op, d1, d2) =>
    let op = transform_bool_op(op);
    let* e1 = transform_exp(d1);
    let* e2 = transform_exp(d2);

    let+ label = next_expr_label;
    {kind: EBinBoolOp(op, e1, e2), label};

  | BinIntOp(op, d1, d2) =>
    let op = transform_int_op(op);
    let* e1 = transform_exp(d1);
    let* e2 = transform_exp(d2);

    let+ label = next_expr_label;
    {kind: EBinIntOp(op, e1, e2), label};

  | BinFloatOp(op, d1, d2) =>
    let op = transform_float_op(op);
    let* e1 = transform_exp(d1);
    let* e2 = transform_exp(d2);

    let+ label = next_expr_label;
    {kind: EBinFloatOp(op, e1, e2), label};

  | Pair(d1, d2) =>
    let* e1 = transform_exp(d1);
    let* e2 = transform_exp(d2);
    let+ label = next_expr_label;
    {kind: EPair(e1, e2), label};

  | Cons(d1, d2) =>
    let* e1 = transform_exp(d1);
    let* e2 = transform_exp(d2);
    let+ label = next_expr_label;
    {kind: ECons(e1, e2), label};

  | Inj(other_ty, side, d') =>
    let other_ty = transform_typ(other_ty);
    let side =
      switch (side) {
      | L => L
      | R => R
      };
    let* e' = transform_exp(d');

    let+ label = next_expr_label;
    {kind: EInj(other_ty, side, e'), label};

  | BoolLit(b) =>
    let+ label = next_expr_label;
    {kind: EBoolLit(b), label};

  | IntLit(i) =>
    let+ label = next_expr_label;
    {kind: EIntLit(i), label};

  | FloatLit(f) =>
    let+ label = next_expr_label;
    {kind: EFloatLit(f), label};

  | ListNil(ty) =>
    let ty = transform_typ(ty);
    let+ label = next_expr_label;
    {kind: ENil(ty), label};

  | Triv =>
    let+ label = next_expr_label;
    {kind: ETriv, label};

  | ConsistentCase(case) =>
    let* case = transform_case(case);
    let+ label = next_expr_label;
    {kind: EConsistentCase(case), label};

  | InconsistentBranches(u, i, sigma, case) =>
    let* sigma = transform_var_map(sigma);
    let* case = transform_case(case);
    let+ label = next_expr_label;
    {kind: EInconsistentBranches(u, i, sigma, case), label};

  | Cast(d', ty, ty') =>
    // FIXME: default implementation of Cast
    // let (d', _) = transform_exp(d');
    // ({kind: ECast(d', ty, ty')}, ty');
    switch (HTyp.ground_cases_of(ty), HTyp.ground_cases_of(ty')) {
    | (GNotGroundOrHole(_), GNotGroundOrHole(_)) =>
      if (HTyp.eq(ty, ty')) {
        transform_exp(d');
      } else {
        let* e' = transform_exp(d');
        let ty = transform_typ(ty);
        let ty' = transform_typ(ty');
        let+ label = next_expr_label;
        {kind: ECast(e', ty, ty'), label};
      }

    | _ =>
      let* e' = transform_exp(d);
      let ty = transform_typ(ty);
      let ty' = transform_typ(ty');
      let+ label = next_expr_label;
      {kind: ECast(e', ty, ty'), label};
    }

  | FailedCast(d', ty, ty') =>
    let* e' = transform_exp(d');
    let ty = transform_typ(ty);
    let ty' = transform_typ(ty');
    let+ label = next_expr_label;
    {kind: EFailedCast(e', ty, ty'), label};

  | InvalidOperation(d', err) =>
    let* e' = transform_exp(d');
    let err = transform_invalid_operation_error(err);
    let+ label = next_expr_label;
    {kind: EInvalidOperation(e', err), label};
  };
}

and transform_bool_op = (op: DHExp.BinBoolOp.t): Expr.bin_bool_op =>
  switch (op) {
  | And => Expr.OpAnd
  | Or => Expr.OpOr
  }

and transform_int_op = (op: DHExp.BinIntOp.t): Expr.bin_int_op => {
  switch (op) {
  | Minus => Expr.OpMinus
  | Plus => Expr.OpPlus
  | Times => Expr.OpTimes
  | Divide => Expr.OpDivide
  | LessThan => Expr.OpLessThan
  | GreaterThan => Expr.OpGreaterThan
  | Equals => Expr.OpEquals
  };
}

and transform_float_op = (op: DHExp.BinFloatOp.t): Expr.bin_float_op => {
  switch (op) {
  | FMinus => Expr.OpFMinus
  | FPlus => Expr.OpFPlus
  | FTimes => Expr.OpFTimes
  | FDivide => Expr.OpFDivide
  | FLessThan => Expr.OpFLessThan
  | FGreaterThan => Expr.OpFGreaterThan
  | FEquals => Expr.OpFEquals
  };
}

and transform_case = (case: DHExp.case) => {
  switch (case) {
  // TODO: Check that all rules have same type?
  | Case(scrut, rules, _) =>
    let* scrut = transform_exp(scrut);
    let+ rules = rules |> List.map(transform_rule) |> sequence;
    {case_kind: ECase(scrut, rules)};
  };
}

and transform_rule = (rule: DHExp.rule) => {
  switch (rule) {
  | Rule(dp, d) =>
    let* p = transform_pat(dp);
    let* e = transform_exp(d);
    let+ rule_label = next_rule_label;
    {rule_kind: ERule(p, e), rule_label};
  };
}

and transform_var_map = (sigma: VarMap.t_(DHExp.t)) =>
  sigma
  |> List.map(((x, d)) => {
       let x = transform_var(x);
       let+ e = transform_exp(d);
       (x, e);
     })
  |> sequence
  >>| List.to_seq
  >>| Sigma.of_seq

and transform_pat = (dp: DHPat.t) => {
  Pat.(
    switch (dp) {
    | EmptyHole(u, i) =>
      let+ label = next_pat_label;
      Pat.{kind: PEmptyHole(u, i), label};

    | NonEmptyHole(reason, u, i, dp) =>
      let reason = transform_hole_reason(reason);
      let* p = transform_pat(dp);
      let+ label = next_pat_label;
      {kind: PNonEmptyHole(reason, u, i, p), label};

    | ExpandingKeyword(u, i, k) =>
      let k = transform_expanding_keyword(k);
      let+ label = next_pat_label;
      {kind: PKeyword(u, i, k), label};

    | InvalidText(u, i, t) =>
      let+ label = next_pat_label;
      {kind: PInvalidText(u, i, t), label};

    | Wild =>
      let+ label = next_pat_label;
      {kind: PWild, label};

    | Ap(dp1, dp2) =>
      let* p1 = transform_pat(dp1);
      let* p2 = transform_pat(dp2);
      let+ label = next_pat_label;
      {kind: PAp(p1, p2), label};

    | Pair(dp1, dp2) =>
      let* p1 = transform_pat(dp1);
      let* p2 = transform_pat(dp2);
      let+ label = next_pat_label;
      {kind: PPair(p1, p2), label};

    | Cons(dp, dps) =>
      let* p = transform_pat(dp);
      let* ps = transform_pat(dps);
      let+ label = next_pat_label;
      {kind: PCons(p, ps), label};

    | Var(x) =>
      let x = transform_var(x);
      let+ label = next_pat_label;
      {kind: PVar(x), label};

    | IntLit(i) =>
      let+ label = next_pat_label;
      {kind: PIntLit(i), label};

    | FloatLit(f) =>
      let+ label = next_pat_label;
      {kind: PFloatLit(f), label};

    | BoolLit(b) =>
      let+ label = next_pat_label;
      {kind: PBoolLit(b), label};

    | Inj(side, dp') =>
      let side =
        switch (side) {
        | L => L
        | R => R
        };
      let* p' = transform_pat(dp');
      let+ label = next_pat_label;
      {kind: PInj(side, p'), label};

    | ListNil =>
      let+ label = next_pat_label;
      {kind: PNil, label};

    | Triv =>
      let+ label = next_pat_label;
      {kind: PTriv, label};
    }
  );
};

let transform = (ctx: Contexts.t, delta: HDelta.t, d: DHExp.t) => {
  let ctx =
    ctx
    |> VarCtx.to_list
    |> List.map(((x, ty)) => (transform_var(x), transform_typ(ty)))
    |> List.to_seq
    |> TypContext.of_seq;

  let m = {
    let* delta = transform_delta(delta);
    let+ e = transform_exp(d);
    (delta, e);
  };

  let (_, (delta, e)) = m(TransformMonad.init);
  (ctx, delta, e);
};
