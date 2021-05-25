open Sexplib.Std;

[@deriving sexp]
type result =
  | InvalidInput(int)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let grounded_Arrow = NotGroundOrHole(Arrow(Hole, Hole));
let grounded_Sum = NotGroundOrHole(Sum(Hole, Hole));
let grounded_Prod = length =>
  NotGroundOrHole(Prod(ListUtil.replicate(length, HTyp.Hole)));
let grounded_List = NotGroundOrHole(List(Hole));

let ground_cases_of = (ctx: Contexts.t, ty: HTyp.t): ground_cases =>
  switch (ty) {
  | TyVarHole(_, _)
  | Hole => Hole
  | Bool
  | Int
  | Float
  | Arrow(Hole, Hole)
  | Sum(Hole, Hole)
  | List(Hole) => Ground
  | TyVar(idx, _) =>
    switch (TyVarCtx.tyvar_with_idx(ctx, idx)) {
    | (_, Singleton(ty2)) => ground_cases_of(ctx, ty2)
    | (_, KHole | Type) => failwith("impossible for bound type variables")
    }
  | Prod(tys) =>
    if (List.for_all(Construction.HTyp.equiv(ctx, HTyp.Hole), tys)) {
      Ground;
    } else {
      tys |> List.length |> grounded_Prod;
    }
  | Arrow(_, _) => grounded_Arrow
  | Sum(_, _) => grounded_Sum
  | List(_) => grounded_List
  };

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

let rec evaluate = (d: DHExp.t): result =>
  switch (d) {
  | BoundVar(_) => InvalidInput(1)
  | Let(dp, d1, d2) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(d1)
    | Indet(d1) =>
      switch (Elaborator_Exp.matches(dp, d1)) {
      | Indet => Indet(d)
      | DoesNotMatch => Indet(d)
      | Matches(env) => evaluate(Elaborator_Exp.subst(env, d2))
      }
    }
  | TyAlias(_, _, _, d) => evaluate(d)
  | FixF(x, _, d1) => evaluate(Elaborator_Exp.subst_var(d, x, d1))
  | Lam(_, _, _) => BoxedValue(d)
  | Ap(d1, d2) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(Lam(dp, _, d3)) =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d2)
      | Indet(d2) =>
        switch (Elaborator_Exp.matches(dp, d2)) {
        | DoesNotMatch => Indet(d)
        | Indet => Indet(d)
        | Matches(env) =>
          /* beta rule */
          evaluate(Elaborator_Exp.subst(env, d3))
        }
      }
    | BoxedValue(Cast(ctx, d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(ctx, d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        evaluate(Cast(ctx, Ap(d1', Cast(ctx, d2', ty1', ty1)), ty2, ty2'))
      }
    | BoxedValue(_) => InvalidInput(2)
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d2')
      | Indet(d2') => Indet(Ap(d1', d2'))
      }
    }
  | ListNil(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | Triv => BoxedValue(d)
  | BinBoolOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(BoolLit(b2)) => BoxedValue(eval_bin_bool_op(op, b1, b2))
      | BoxedValue(_) => InvalidInput(3)
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
      }
    | BoxedValue(_) => InvalidInput(4)
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(IntLit(n1) as d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(IntLit(n2)) =>
        switch (op, n1, n2) {
        | (Divide, _, 0) =>
          Indet(
            InvalidOperation(
              BinIntOp(op, IntLit(n1), IntLit(n2)),
              DivideByZero,
            ),
          )
        | _ => BoxedValue(eval_bin_int_op(op, n1, n2))
        }
      | BoxedValue(_) => InvalidInput(3)
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    | BoxedValue(_) => InvalidInput(4)
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(FloatLit(f1) as d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(FloatLit(f2)) =>
        BoxedValue(eval_bin_float_op(op, f1, f2))
      | BoxedValue(_) => InvalidInput(8)
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    | BoxedValue(_) => InvalidInput(7)
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | InvalidInput(msg) => InvalidInput(msg)
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    }
  | Inj(ty, side, d1) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(d1') => BoxedValue(Inj(ty, side, d1'))
    | Indet(d1') => Indet(Inj(ty, side, d1'))
    }
  | Pair(d1, d2) =>
    switch (evaluate(d1), evaluate(d2)) {
    | (InvalidInput(msg), _)
    | (_, InvalidInput(msg)) => InvalidInput(msg)
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2))
    | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Pair(d1, d2))
    }
  | Cons(d1, d2) =>
    switch (evaluate(d1), evaluate(d2)) {
    | (InvalidInput(msg), _)
    | (_, InvalidInput(msg)) => InvalidInput(msg)
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Cons(d1, d2))
    | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Cons(d1, d2))
    }
  | ConsistentCase(Case(d1, rules, n)) => evaluate_case(None, d1, rules, n)
  | InconsistentBranches(u, i, sigma, Case(d1, rules, n)) =>
    evaluate_case(Some((u, i, sigma)), d1, rules, n)
  | EmptyHole(_) => Indet(d)
  | NonEmptyHole(reason, u, i, sigma, d1) =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(d1')
    | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
    }
  | FreeVar(_) => Indet(d)
  | Keyword(_) => Indet(d)
  | InvalidText(_) => Indet(d)
  | Cast(ctx, d1, ty, ty') =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(d1') as result =>
      switch (ground_cases_of(ctx, ty), ground_cases_of(ctx, ty')) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(ctx, d1', ty, ty'))
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(ctx, d1'', ty'', Hole | TyVarHole(_)) =>
          if (Construction.HTyp.equiv(ctx, ty'', ty')) {
            BoxedValue(d1'');
          } else {
            Indet(FailedCast(ctx, d1', ty, ty'));
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          InvalidInput(6)
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' =
          DHExp.Cast(
            ctx,
            Cast(ctx, d1', ty, ty'_grounded),
            ty'_grounded,
            ty',
          );
        evaluate(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' =
          DHExp.Cast(ctx, Cast(ctx, d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(ctx, d1', ty, ty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (Construction.HTyp.equiv(ctx, ty, ty')) {
          result;
        } else {
          BoxedValue(Cast(ctx, d1', ty, ty'));
        }
      }
    | Indet(d1') as result =>
      switch (ground_cases_of(ctx, ty), ground_cases_of(ctx, ty')) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(ctx, d1', ty, ty'))
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(ctx, d1'', ty'', Hole) =>
          if (Construction.HTyp.equiv(ctx, ty'', ty')) {
            Indet(d1'');
          } else {
            Indet(FailedCast(ctx, d1', ty, ty'));
          }
        | _ => Indet(Cast(ctx, d1', ty, ty'))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' =
          DHExp.Cast(
            ctx,
            Cast(ctx, d1', ty, ty'_grounded),
            ty'_grounded,
            ty',
          );
        evaluate(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' =
          DHExp.Cast(ctx, Cast(ctx, d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(ctx, d1', ty, ty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (Construction.HTyp.equiv(ctx, ty, ty')) {
          result;
        } else {
          Indet(Cast(ctx, d1', ty, ty'));
        }
      }
    }
  | FailedCast(ctx, d1, ty, ty') =>
    switch (evaluate(d1)) {
    | InvalidInput(msg) => InvalidInput(msg)
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(ctx, d1', ty, ty'))
    }
  | InvalidOperation(d, err) => Indet(InvalidOperation(d, err))
  }
and evaluate_case =
    (
      inconsistent_info,
      scrut: DHExp.t,
      rules: list(DHExp.rule),
      current_rule_index: int,
    )
    : result =>
  switch (evaluate(scrut)) {
  | InvalidInput(msg) => InvalidInput(msg)
  | BoxedValue(scrut)
  | Indet(scrut) =>
    switch (List.nth_opt(rules, current_rule_index)) {
    | None =>
      let case = DHExp.Case(scrut, rules, current_rule_index);
      switch (inconsistent_info) {
      | None => Indet(ConsistentCase(case))
      | Some((u, i, sigma)) =>
        Indet(InconsistentBranches(u, i, sigma, case))
      };
    | Some(Rule(dp, d)) =>
      switch (Elaborator_Exp.matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Matches(env) => evaluate(Elaborator_Exp.subst(env, d))
      | DoesNotMatch =>
        evaluate_case(inconsistent_info, scrut, rules, current_rule_index + 1)
      }
    }
  };
