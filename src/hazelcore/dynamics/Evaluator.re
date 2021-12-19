[@deriving sexp]
type result =
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

[@deriving sexp]
type ground_cases =
  | Hole
  | Ground
  | NotGroundOrHole(HTyp.t) /* the argument is the corresponding ground type */;

let grounded_Arrow = NotGroundOrHole(Arrow(Hole, Hole));
let grounded_FiniteSum = (tymap: TagMap.t(option(HTyp.t))): ground_cases => {
  let tymap' = tymap |> TagMap.map(Option.map(_ => HTyp.Hole));
  NotGroundOrHole(Sum(Finite(tymap')));
};
let grounded_ElidedSum = (tag: UHTag.t, ty_opt: option(HTyp.t)): ground_cases => {
  NotGroundOrHole(Sum(Elided(tag, ty_opt |> Option.map(_ => HTyp.Hole))));
};
let grounded_Prod = length =>
  NotGroundOrHole(Prod(ListUtil.replicate(length, HTyp.Hole)));
let grounded_List = NotGroundOrHole(List(Hole));

let rec ground_cases_of = (ty: HTyp.t): ground_cases => {
  let is_ground_arg: option(HTyp.t) => bool =
    fun
    | None
    | Some(HTyp.Hole) => true
    | Some(ty) => ground_cases_of(ty) == Ground;
  switch (ty) {
  | Hole => Hole
  | Bool
  | Int
  | Float
  | Arrow(Hole, Hole)
  | List(Hole) => Ground
  | Prod(tys) =>
    if (List.for_all(HTyp.eq(HTyp.Hole), tys)) {
      Ground;
    } else {
      tys |> List.length |> grounded_Prod;
    }
  | Arrow(_, _) => grounded_Arrow
  | Sum(Finite(tymap)) =>
    tymap |> TagMap.is_ground(is_ground_arg)
      ? Ground : grounded_FiniteSum(tymap)
  | Sum(Elided(tag, ty_opt)) =>
    is_ground_arg(ty_opt) ? Ground : grounded_ElidedSum(tag, ty_opt)
  | List(_) => grounded_List
  };
};

let cast_tymaps =
    (tymap1: TagMap.t(option(HTyp.t)), tymap2: TagMap.t(option(HTyp.t)))
    : option(TagMap.t((HTyp.t, HTyp.t))) => {
  let (tags1, types1) = tymap1 |> TagMap.bindings |> List.split;
  let (tags2, types2) = tymap2 |> TagMap.bindings |> List.split;
  if (tags1 == tags2) {
    let tys1 = types1 |> List.filter(Option.is_some) |> List.map(Option.get);
    let tys2 = types2 |> List.filter(Option.is_some) |> List.map(Option.get);
    if (List.length(tys1) == List.length(tys2)) {
      Some(List.(combine(tys1, tys2) |> combine(tags1)) |> TagMap.of_list);
    } else {
      None;
    };
  } else {
    None;
  };
};

type match_result =
  | Matches(Environment.t)
  | DoesNotMatch
  | Indet;

let rec matches = (dp: DHPat.t, d: DHExp.t): match_result =>
  switch (dp, d) {
  | (_, BoundVar(_)) => DoesNotMatch
  | (EmptyHole(_, _), _)
  | (NonEmptyHole(_, _, _, _), _) => Indet
  | (Wild, _) => Matches(Environment.empty)
  | (Keyword(_, _, _), _) => DoesNotMatch
  | (InvalidText(_), _) => Indet
  | (Var(x), _) =>
    let env = Environment.extend(Environment.empty, (x, d));
    Matches(env);
  | (_, EmptyHole(_, _, _)) => Indet
  | (_, NonEmptyHole(_, _, _, _, _)) => Indet
  | (_, FailedCast(_, _, _)) => Indet
  | (_, InvalidOperation(_)) => Indet
  | (_, FreeVar(_, _, _, _)) => Indet
  | (_, InvalidText(_)) => Indet
  | (_, Let(_, _, _)) => Indet
  | (_, FixF(_, _, _)) => DoesNotMatch
  | (_, Lam(_, _, _)) => DoesNotMatch
  | (_, Ap(_, _)) => Indet
  | (_, BinBoolOp(_, _, _)) => Indet
  | (_, BinIntOp(_, _, _)) => Indet
  | (_, BinFloatOp(_, _, _)) => Indet
  | (_, ConsistentCase(Case(_, _, _))) => Indet
  | (BoolLit(b1), BoolLit(b2)) =>
    if (b1 == b2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (BoolLit(_), Cast(d, Bool, Hole)) => matches(dp, d)
  | (BoolLit(_), Cast(d, Hole, Bool)) => matches(dp, d)
  | (BoolLit(_), _) => DoesNotMatch
  | (IntLit(n1), IntLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (IntLit(_), Cast(d, Int, Hole)) => matches(dp, d)
  | (IntLit(_), Cast(d, Hole, Int)) => matches(dp, d)
  | (IntLit(_), _) => DoesNotMatch
  | (FloatLit(n1), FloatLit(n2)) =>
    if (n1 == n2) {
      Matches(Environment.empty);
    } else {
      DoesNotMatch;
    }
  | (FloatLit(_), Cast(d, Float, Hole)) => matches(dp, d)
  | (FloatLit(_), Cast(d, Hole, Float)) => matches(dp, d)
  | (FloatLit(_), _) => DoesNotMatch
  | (Inj((tag1, dp_opt)), Inj((_, tag2, d_opt))) =>
    switch (tag1, tag2) {
    | (Tag(_), Tag(_))
    | (EmptyTagHole(_), EmptyTagHole(_)) when UHTag.equal(tag1, tag2) =>
      switch (dp_opt, d_opt) {
      | (None, None) => Matches(Environment.empty)
      | (Some(dp), Some(d)) => matches(dp, d)
      | (None, Some(_))
      | (Some(_), None) => DoesNotMatch
      }
    | (Tag(_), Tag(_)) => DoesNotMatch
    | (EmptyTagHole(_), EmptyTagHole(_))
    | (Tag(_), EmptyTagHole(_))
    | (EmptyTagHole(_), Tag(_)) =>
      switch (dp_opt, d_opt) {
      | (None, None) => Indet
      | (Some(dp), Some(d)) =>
        switch (matches(dp, d)) {
        | Matches(_)
        | Indet => Indet
        | DoesNotMatch => DoesNotMatch
        }
      | (None, Some(_))
      | (Some(_), None) => DoesNotMatch
      }
    }
  | (
      Inj((tag, dp_opt)),
      Cast(d, Sum(Finite(tymap1)), Sum(Finite(tymap2))),
    ) =>
    switch (cast_tymaps(tymap1, tymap2)) {
    | Some(castmap) => matches_cast_Inj(tag, dp_opt, d, [castmap])
    | None => DoesNotMatch
    }
  | (Inj((_, _)), Cast(d, Sum(_), Hole))
  | (Inj((_, _)), Cast(d, Hole, Sum(_))) => matches(dp, d)
  | (Inj((_, _)), _) => DoesNotMatch
  | (InjError(_, _, _, (_, _)), _) => Indet
  | (Pair(dp1, dp2), Pair(d1, d2)) =>
    switch (matches(dp1, d1)) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | (
      Pair(dp1, dp2),
      Cast(d, Prod([head1, ...tail1]), Prod([head2, ...tail2])),
    ) =>
    matches_cast_Pair(
      dp1,
      dp2,
      d,
      [(head1, head2)],
      List.combine(tail1, tail2),
    )
  | (Pair(_, _), Cast(d, Hole, Prod(_)))
  | (Pair(_, _), Cast(d, Prod(_), Hole)) => matches(dp, d)
  | (Pair(_, _), _) => DoesNotMatch
  | (Triv, Triv) => Matches(Environment.empty)
  | (Triv, Cast(d, Hole, Prod([]))) => matches(dp, d)
  | (Triv, Cast(d, Prod([]), Hole)) => matches(dp, d)
  | (Triv, _) => DoesNotMatch
  | (ListNil, ListNil(_)) => Matches(Environment.empty)
  | (ListNil, Cast(d, Hole, List(_))) => matches(dp, d)
  | (ListNil, Cast(d, List(_), Hole)) => matches(dp, d)
  | (ListNil, Cast(d, List(_), List(_))) => matches(dp, d)
  | (ListNil, _) => DoesNotMatch
  | (Cons(dp1, dp2), Cons(d1, d2)) =>
    switch (matches(dp1, d1)) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
    | Matches(env1) =>
      switch (matches(dp2, d2)) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | (Cons(dp1, dp2), Cast(d, List(ty1), List(ty2))) =>
    matches_cast_Cons(dp1, dp2, d, [(ty1, ty2)])
  | (Cons(_, _), Cast(d, Hole, List(_))) => matches(dp, d)
  | (Cons(_, _), Cast(d, List(_), Hole)) => matches(dp, d)
  | (Cons(_, _), _) => DoesNotMatch
  | (Ap(_, _), _) => DoesNotMatch
  }
and matches_cast_Inj =
    (
      tag: UHTag.t,
      dp_opt: option(DHPat.t),
      d: DHExp.t,
      castmaps: list(TagMap.t((HTyp.t, HTyp.t))),
    )
    : match_result =>
  switch (d) {
  | Inj((_, tag, d_opt)) =>
    switch (dp_opt, d_opt) {
    | (None, None) => Matches(Environment.empty)
    | (Some(dp), Some(d')) =>
      switch (
        castmaps
        |> List.map(castmap => TagMap.find_opt(tag, castmap))
        |> OptUtil.sequence
      ) {
      | Some(side_casts) => matches(dp, DHExp.apply_casts(d', side_casts))
      | None => DoesNotMatch
      }
    | _ => DoesNotMatch
    }
  | Cast(d', Sum(Finite(tymap1)), Sum(Finite(tymap2))) =>
    switch (cast_tymaps(tymap1, tymap2)) {
    | Some(castmap) =>
      matches_cast_Inj(tag, dp_opt, d', [castmap, ...castmaps])
    | None => DoesNotMatch
    }
  | Cast(d', Sum(_), Hole)
  | Cast(d', Hole, Sum(_)) => matches_cast_Inj(tag, dp_opt, d', castmaps)
  | Cast(_, _, _) => DoesNotMatch
  | InjError(_, _, _, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | ListNil(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Pair(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => Indet
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  }
and matches_cast_Pair =
    (
      dp1: DHPat.t,
      dp2: DHPat.t,
      d: DHExp.t,
      left_casts: list((HTyp.t, HTyp.t)),
      right_casts: list((HTyp.t, HTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Pair(d1, d2) =>
    switch (matches(dp1, DHExp.apply_casts(d1, left_casts))) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      }
    | Matches(env1) =>
      switch (matches(dp2, DHExp.apply_casts(d2, right_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      }
    }
  | Cast(d', Prod([]), Prod([])) =>
    matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
  | Cast(d', Prod([head1, ...tail1]), Prod([head2, ...tail2])) =>
    matches_cast_Pair(
      dp1,
      dp2,
      d',
      [(head1, head2), ...left_casts],
      List.combine(tail1, tail2) @ right_casts,
    )
  | Cast(d', Prod(_), Hole)
  | Cast(d', Hole, Prod(_)) =>
    matches_cast_Pair(dp1, dp2, d', left_casts, right_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | Inj(_)
  | InjError(_) => DoesNotMatch
  | ListNil(_) => DoesNotMatch
  | Cons(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => Indet
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  }
and matches_cast_Cons =
    (
      dp1: DHPat.t,
      dp2: DHPat.t,
      d: DHExp.t,
      elt_casts: list((HTyp.t, HTyp.t)),
    )
    : match_result =>
  switch (d) {
  | Cons(d1, d2) =>
    switch (matches(dp1, DHExp.apply_casts(d1, elt_casts))) {
    | DoesNotMatch => DoesNotMatch
    | Indet =>
      let list_casts =
        List.map(
          (c: (HTyp.t, HTyp.t)) => {
            let (ty1, ty2) = c;
            (HTyp.List(ty1), HTyp.List(ty2));
          },
          elt_casts,
        );
      switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet
      | Matches(_) => Indet
      };
    | Matches(env1) =>
      let list_casts =
        List.map(
          (c: (HTyp.t, HTyp.t)) => {
            let (ty1, ty2) = c;
            (HTyp.List(ty1), HTyp.List(ty2));
          },
          elt_casts,
        );
      switch (matches(dp2, DHExp.apply_casts(d2, list_casts))) {
      | DoesNotMatch => DoesNotMatch
      | Indet => Indet
      | Matches(env2) => Matches(Environment.union(env1, env2))
      };
    }
  | Cast(d', List(ty1), List(ty2)) =>
    matches_cast_Cons(dp1, dp2, d', [(ty1, ty2), ...elt_casts])
  | Cast(d', List(_), Hole) => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Cast(d', Hole, List(_)) => matches_cast_Cons(dp1, dp2, d', elt_casts)
  | Cast(_, _, _) => DoesNotMatch
  | BoundVar(_) => DoesNotMatch
  | FreeVar(_, _, _, _) => Indet
  | InvalidText(_) => Indet
  | Keyword(_, _, _, _) => Indet
  | Let(_, _, _) => Indet
  | FixF(_, _, _) => DoesNotMatch
  | Lam(_, _, _) => DoesNotMatch
  | Ap(_, _) => Indet
  | BinBoolOp(_, _, _)
  | BinIntOp(_, _, _)
  | BinFloatOp(_, _, _)
  | BoolLit(_) => DoesNotMatch
  | IntLit(_) => DoesNotMatch
  | FloatLit(_) => DoesNotMatch
  | Inj(_)
  | InjError(_) => DoesNotMatch
  | ListNil(_) => DoesNotMatch
  | Pair(_, _) => DoesNotMatch
  | Triv => DoesNotMatch
  | ConsistentCase(_)
  | InconsistentBranches(_) => Indet
  | EmptyHole(_, _, _) => Indet
  | NonEmptyHole(_, _, _, _, _) => Indet
  | FailedCast(_, _, _) => Indet
  | InvalidOperation(_) => Indet
  };

/* closed substitution [d1/x]d2*/
let rec subst_var = (d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t =>
  switch (d2) {
  | BoundVar(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | FreeVar(_) => d2
  | InvalidText(_) => d2
  | Keyword(_) => d2
  | Let(dp, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 =
      if (DHPat.binds_var(x, dp)) {
        d4;
      } else {
        subst_var(d1, x, d4);
      };
    Let(dp, d3, d4);
  | FixF(y, ty, d3) =>
    let d3 =
      if (Var.eq(x, y)) {
        d3;
      } else {
        subst_var(d1, x, d3);
      };
    FixF(y, ty, d3);
  | Lam(dp, ty, d3) =>
    if (DHPat.binds_var(x, dp)) {
      d2;
    } else {
      let d3 = subst_var(d1, x, d3);
      Lam(dp, ty, d3);
    }
  | Ap(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Ap(d3, d4);
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | ListNil(_)
  | Triv => d2
  | Cons(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Cons(d3, d4);
  | BinBoolOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinBoolOp(op, d3, d4);
  | BinIntOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinIntOp(op, d3, d4);
  | BinFloatOp(op, d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    BinFloatOp(op, d3, d4);
  | Inj((ty, tag, d3_opt)) =>
    let d3_opt' = Option.map(subst_var(d1, x), d3_opt);
    Inj((ty, tag, d3_opt'));
  | InjError(reason, u, i, sigma, (ty, tag, d3_opt)) =>
    let d3_opt' = Option.map(subst_var(d1, x), d3_opt);
    let sigma' = subst_var_env(d1, x, sigma);
    InjError(reason, u, i, sigma', (ty, tag, d3_opt'));
  | Pair(d3, d4) =>
    let d3 = subst_var(d1, x, d3);
    let d4 = subst_var(d1, x, d4);
    Pair(d3, d4);
  | ConsistentCase(Case(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    ConsistentCase(Case(d3, rules, n));
  | InconsistentBranches(u, i, sigma, Case(d3, rules, n)) =>
    let d3 = subst_var(d1, x, d3);
    let rules = subst_var_rules(d1, x, rules);
    let sigma' = subst_var_env(d1, x, sigma);
    InconsistentBranches(u, i, sigma', Case(d3, rules, n));
  | EmptyHole(u, i, sigma) =>
    let sigma' = subst_var_env(d1, x, sigma);
    EmptyHole(u, i, sigma');
  | NonEmptyHole(reason, u, i, sigma, d3) =>
    let d3' = subst_var(d1, x, d3);
    let sigma' = subst_var_env(d1, x, sigma);
    NonEmptyHole(reason, u, i, sigma', d3');
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    Cast(d', ty1, ty2);
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(d1, x, d);
    FailedCast(d', ty1, ty2);
  | InvalidOperation(d, err) =>
    let d' = subst_var(d1, x, d);
    InvalidOperation(d', err);
  }

and subst_var_rules =
    (d1: DHExp.t, x: Var.t, rules: list(DHExp.rule)): list(DHExp.rule) =>
  rules
  |> List.map((r: DHExp.rule) =>
       switch (r) {
       | Rule(dp, d2) =>
         if (DHPat.binds_var(x, dp)) {
           r;
         } else {
           Rule(dp, subst_var(d1, x, d2));
         }
       }
     )

and subst_var_env =
    (d1: DHExp.t, x: Var.t, sigma: Environment.t): Environment.t =>
  sigma
  |> List.map(xd => {
       let (y, d) = xd;
       (y, subst_var(d1, x, d));
     });

let subst = (env: Environment.t, d: DHExp.t): DHExp.t =>
  env
  |> List.fold_left(
       (d2, xd: (Var.t, DHExp.t)) => {
         let (x, d1) = xd;
         subst_var(d1, x, d2);
       },
       d,
     );

let eval_bin_bool_op = (op: DHExp.BinBoolOp.t, b1: bool, b2: bool): DHExp.t =>
  switch (op) {
  | And => BoolLit(b1 && b2)
  | Or => BoolLit(b1 || b2)
  };

let eval_bin_bool_op_short_circuit =
    (op: DHExp.BinBoolOp.t, b1: bool): option(result) =>
  switch (op, b1) {
  | (Or, true) => Some(BoxedValue(BoolLit(true)))
  | (And, false) => Some(BoxedValue(BoolLit(false)))
  | _ => None
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
  | BoundVar(x) => raise(EvaluatorError.Exception(FreeInvalidVar(x)))
  | Let(dp, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(d1)
    | Indet(d1) =>
      switch (matches(dp, d1)) {
      | Indet => Indet(d)
      | DoesNotMatch => Indet(d)
      | Matches(env) => evaluate(subst(env, d2))
      }
    }
  | FixF(x, _, d1) => evaluate(subst_var(d, x, d1))
  | Lam(_, _, _) => BoxedValue(d)
  | Ap(d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(Lam(dp, _, d3)) =>
      switch (evaluate(d2)) {
      | BoxedValue(d2)
      | Indet(d2) =>
        switch (matches(dp, d2)) {
        | DoesNotMatch => Indet(d)
        | Indet => Indet(d)
        | Matches(env) =>
          /* beta rule */
          evaluate(subst(env, d3))
        }
      }
    | BoxedValue(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2')))
    | Indet(Cast(d1', Arrow(ty1, ty2), Arrow(ty1', ty2'))) =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') =>
        /* ap cast rule */
        evaluate(Cast(Ap(d1', Cast(d2', ty1', ty1)), ty2, ty2'))
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedLam(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
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
    | BoxedValue(BoolLit(b1) as d1') =>
      switch (eval_bin_bool_op_short_circuit(op, b1)) {
      | Some(b3) => b3
      | None =>
        switch (evaluate(d2)) {
        | BoxedValue(BoolLit(b2)) =>
          BoxedValue(eval_bin_bool_op(op, b1, b2))
        | BoxedValue(d2') =>
          raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d2')))
        | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
        }
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinBoolOp(op, d1', d2'))
      }
    }
  | BinIntOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(IntLit(n1) as d1') =>
      switch (evaluate(d2)) {
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
      | BoxedValue(d2') =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2')))
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinIntOp(op, d1', d2'))
      }
    }
  | BinFloatOp(op, d1, d2) =>
    switch (evaluate(d1)) {
    | BoxedValue(FloatLit(f1) as d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(FloatLit(f2)) =>
        BoxedValue(eval_bin_float_op(op, f1, f2))
      | BoxedValue(d2') =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d2')))
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    | BoxedValue(d1') =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1')))
    | Indet(d1') =>
      switch (evaluate(d2)) {
      | BoxedValue(d2')
      | Indet(d2') => Indet(BinFloatOp(op, d1', d2'))
      }
    }
  // VInjNull
  | Inj((_, Tag(_), None)) => BoxedValue(d)
  | Inj((tymap, Tag(_) as tag, Some(d1))) =>
    switch (evaluate(d1)) {
    // VInj, BVInj
    | BoxedValue(d1') => BoxedValue(Inj((tymap, tag, Some(d1'))))
    // IInj
    | Indet(d1') => Indet(Inj((tymap, tag, Some(d1'))))
    }
  // IInjNull
  | Inj((_, EmptyTagHole(_), None))
  | InjError(_, _, _, _, (_, _, None)) => Indet(d)
  | Inj((tymap, EmptyTagHole(_) as tag, Some(d1))) =>
    switch (evaluate(d1)) {
    // IInjTag
    | BoxedValue(d1')
    | Indet(d1') => Indet(Inj((tymap, tag, Some(d1'))))
    }
  | InjError(reason, u, i, sigma, (tymap, tag, Some(d1))) =>
    switch (evaluate(d1)) {
    // IInjTag
    | BoxedValue(d1')
    | Indet(d1') =>
      Indet(InjError(reason, u, i, sigma, (tymap, tag, Some(d1'))))
    }
  | Cast(d1, Sum(_) as ty, Sum(_) as ty') =>
    switch (evaluate(d1)) {
    // BVSumCast
    | BoxedValue(d1') as result =>
      if (HTyp.eq(ty, ty')) {
        result;
      } else {
        BoxedValue(Cast(d1', ty, ty'));
      }
    // ICastSum
    | Indet(d1') as result =>
      if (HTyp.eq(ty, ty')) {
        result;
      } else {
        Indet(Cast(d1', ty, ty'));
      }
    }
  | Pair(d1, d2) =>
    switch (evaluate(d1), evaluate(d2)) {
    | (Indet(d1), Indet(d2))
    | (Indet(d1), BoxedValue(d2))
    | (BoxedValue(d1), Indet(d2)) => Indet(Pair(d1, d2))
    | (BoxedValue(d1), BoxedValue(d2)) => BoxedValue(Pair(d1, d2))
    }
  | Cons(d1, d2) =>
    switch (evaluate(d1), evaluate(d2)) {
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
    | BoxedValue(d1')
    | Indet(d1') => Indet(NonEmptyHole(reason, u, i, sigma, d1'))
    }
  | FreeVar(_) => Indet(d)
  | Keyword(_) => Indet(d)
  | InvalidText(_) => Indet(d)
  | Cast(d1, ty, ty') =>
    switch (evaluate(d1)) {
    | BoxedValue(d1') as result =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        BoxedValue(Cast(d1', ty, ty'))
      | (Hole, Ground) =>
        /* by canonical forms, d1' must be of the form d<ty'' -> ?> */
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            BoxedValue(d1'');
          } else {
            Indet(FailedCast(d1', ty, ty'));
          }
        | _ =>
          // TODO: can we omit this? or maybe call logging? JSUtil.log(DHExp.constructor_string(d1'));
          raise(EvaluatorError.Exception(CastBVHoleGround(d1')))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        BoxedValue(Cast(d1', ty, ty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* they might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          result;
        } else {
          BoxedValue(Cast(d1', ty, ty'));
        }
      }
    | Indet(d1') as result =>
      switch (ground_cases_of(ty), ground_cases_of(ty')) {
      | (Hole, Hole) => result
      | (Ground, Ground) =>
        /* if two types are ground and consistent, then they are eq */
        result
      | (Ground, Hole) =>
        /* can't remove the cast or do anything else here, so we're done */
        Indet(Cast(d1', ty, ty'))
      | (Hole, Ground) =>
        switch (d1') {
        | Cast(d1'', ty'', Hole) =>
          if (HTyp.eq(ty'', ty')) {
            Indet(d1'');
          } else {
            Indet(FailedCast(d1', ty, ty'));
          }
        | _ => Indet(Cast(d1', ty, ty'))
        }
      | (Hole, NotGroundOrHole(ty'_grounded)) =>
        /* ITExpand rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty'_grounded), ty'_grounded, ty');
        evaluate(d');
      | (NotGroundOrHole(ty_grounded), Hole) =>
        /* ITGround rule */
        let d' = DHExp.Cast(Cast(d1', ty, ty_grounded), ty_grounded, ty');
        evaluate(d');
      | (Ground, NotGroundOrHole(_))
      | (NotGroundOrHole(_), Ground) =>
        /* can't do anything when casting between diseq, non-hole types */
        Indet(Cast(d1', ty, ty'))
      | (NotGroundOrHole(_), NotGroundOrHole(_)) =>
        /* it might be eq in this case, so remove cast if so */
        if (HTyp.eq(ty, ty')) {
          result;
        } else {
          Indet(Cast(d1', ty, ty'));
        }
      }
    }
  | FailedCast(d1, ty, ty') =>
    switch (evaluate(d1)) {
    | BoxedValue(d1')
    | Indet(d1') => Indet(FailedCast(d1', ty, ty'))
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
      switch (matches(dp, scrut)) {
      | Indet =>
        let case = DHExp.Case(scrut, rules, current_rule_index);
        switch (inconsistent_info) {
        | None => Indet(ConsistentCase(case))
        | Some((u, i, sigma)) =>
          Indet(InconsistentBranches(u, i, sigma, case))
        };
      | Matches(env) => evaluate(subst(env, d))
      | DoesNotMatch =>
        evaluate_case(inconsistent_info, scrut, rules, current_rule_index + 1)
      }
    }
  };
