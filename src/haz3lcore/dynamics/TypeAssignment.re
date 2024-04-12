open Util;
open OptUtil.Syntax;

let equal_typ_list = (l: list(Typ.t)): option(Typ.t) => {
  switch (l) {
  | [] => None
  | [ty, ..._] =>
    List.fold_left((acc, t) => {acc && Typ.eq(t, ty)}, true, l)
      ? Some(ty) : None
  };
};

let delta_ty = (id: MetaVar.t, m: Statics.Map.t): option(Typ.t) => {
  switch (Id.Map.find_opt(id, m)) {
  | Some(InfoExp({mode, ctx, _})) =>
    switch (mode) {
    | Syn
    | SynFun => Some(Unknown(Internal))
    | Ana(ana_ty) => Some(Typ.normalize(ctx, ana_ty))
    }
  | _ => None
  };
};

let ground = (ty: Typ.t): bool => {
  switch (Transition.CastHelpers.ground_cases_of(ty)) {
  | Ground => true
  | _ => false
  };
};

let dhpat_extend_ctx = (dhpat: DHPat.t, ty: Typ.t, ctx: Ctx.t): option(Ctx.t) => {
  let rec dhpat_var_entry =
          (dhpat: DHPat.t, ty: Typ.t): option(list(Ctx.entry)) => {
    switch (dhpat) {
    | Var(name) =>
      let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty});
      Some([entry]);
    | Tuple(l1) =>
      switch (ty) {
      | Prod(l2) when List.length(l1) == List.length(l2) =>
        let* l =
          List.map2((dhp, typ) => {dhpat_var_entry(dhp, typ)}, l1, l2)
          |> OptUtil.sequence;
        Some(List.concat(l));
      | _ => None
      }
    | Cons(dhp1, dhp2) =>
      switch (ty) {
      | List(typ) =>
        let* l1 = dhpat_var_entry(dhp1, typ);
        let* l2 = dhpat_var_entry(dhp2, typ);
        Some(l1 @ l2);
      | _ => None
      }
    | ListLit(typ1, l) =>
      switch (ty) {
      | List(typ2) when Typ.eq(typ1, typ2) =>
        let* l =
          List.map(dhp => {dhpat_var_entry(dhp, typ1)}, l)
          |> OptUtil.sequence;
        Some(List.concat(l));
      | _ => None
      }
    | Ap(Constructor(_, typ), dhp) =>
      let (ty1, ty2) = Typ.matched_arrow(ctx, typ);
      Typ.eq(ty2, ty) ? dhpat_var_entry(dhp, ty1) : None;
    | EmptyHole(_)
    | NonEmptyHole(_)
    | Wild
    | ExpandingKeyword(_)
    | InvalidText(_)
    | BadConstructor(_)
    | IntLit(_)
    | FloatLit(_)
    | BoolLit(_)
    | StringLit(_)
    | Constructor(_)
    | Ap(_) => Some([])
    };
  };
  let+ l = dhpat_var_entry(dhpat, ty);
  List.fold_left((ctx, entry) => Ctx.extend(ctx, entry), ctx, l);
};

let rec typ_of_dhexp =
        (ctx: Ctx.t, m: Statics.Map.t, dh: DHExp.t): option(Typ.t) => {
  switch (dh) {
  | EmptyHole(id, _) => delta_ty(id, m)
  | NonEmptyHole(_, id, _, d) =>
    switch (typ_of_dhexp(ctx, m, d)) {
    | None => None
    | Some(_) => delta_ty(id, m)
    }
  | FreeVar(id, _, _) => delta_ty(id, m)
  | ExpandingKeyword(_)
  | InvalidText(_) => Some(Unknown(Internal))
  | InconsistentBranches(_, _, Case(d_scrut, d_rules, _)) =>
    let* ty' = typ_of_dhexp(ctx, m, d_scrut);
    let typ_cases =
      d_rules
      |> List.map((DHExp.Rule(dhp, de)) => {
           let* ctx = dhpat_extend_ctx(dhp, ty', ctx);
           typ_of_dhexp(ctx, m, de);
         })
      |> OptUtil.sequence;

    switch (typ_cases) {
    | None => None
    | Some(_) => Some(Typ.Unknown(Internal))
    };
  | Closure(env, d) =>
    let* l =
      env
      |> ClosureEnvironment.to_list
      |> List.map(((name, de)) => {
           let+ ty = typ_of_dhexp(ctx, m, de);
           Ctx.VarEntry({name, id: Id.invalid, typ: ty});
         })
      |> OptUtil.sequence;
    let ctx' =
      List.fold_left(
        (ctx, var_entry) => Ctx.extend(ctx, var_entry),
        ctx,
        l,
      );
    typ_of_dhexp(ctx', m, d);
  | Filter(_, d) => typ_of_dhexp(ctx, m, d)
  | BoundVar(name) =>
    let* var = Ctx.lookup_var(ctx, name);
    Some(var.typ);
  | Sequence(d1, d2) =>
    let* _ = typ_of_dhexp(ctx, m, d1);
    typ_of_dhexp(ctx, m, d2);
  | Let(dhp, de, db) =>
    let* ty1 = typ_of_dhexp(ctx, m, de);
    let* ctx = dhpat_extend_ctx(dhp, ty1, ctx);
    typ_of_dhexp(ctx, m, db);
  | FixF(name, ty1, d) =>
    let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty1});
    typ_of_dhexp(Ctx.extend(ctx, entry), m, d);
  | Fun(dhp, ty1, d, _) =>
    let* ctx = dhpat_extend_ctx(dhp, ty1, ctx);
    let* ty2 = typ_of_dhexp(ctx, m, d);
    Some(Typ.Arrow(ty1, ty2));
  | Ap(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (ty1) {
    | Arrow(tyl, tyr) when Typ.eq(tyl, ty2) => Some(tyr)
    | _ => None
    };
  | ApBuiltin(name, d) =>
    let* var = Ctx.lookup_var(ctx, name);
    let* ty = typ_of_dhexp(ctx, m, d);
    switch (var.typ) {
    | Arrow(tyl, tyr) when Typ.eq(tyl, ty) => Some(tyr)
    | _ => None
    };
  | BuiltinFun(name) =>
    let* var = Ctx.lookup_var(ctx, name);
    Some(var.typ);
  | Test(_, dtest) =>
    let* ty = typ_of_dhexp(ctx, m, dtest);
    Typ.eq(ty, Bool) ? Some(Typ.Prod([])) : None;
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | BinBoolOp(_, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    Typ.eq(ty1, Bool) && Typ.eq(ty2, Bool) ? Some(Typ.Bool) : None;
  | BinIntOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Int) && Typ.eq(ty2, Int)) {
      switch (op) {
      | Minus
      | Plus
      | Times
      | Power
      | Divide => Some(Typ.Int)
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals
      | NotEquals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | BinFloatOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Float) && Typ.eq(ty2, Float)) {
      switch (op) {
      | Minus
      | Plus
      | Times
      | Power
      | Divide => Some(Typ.Float)
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals
      | NotEquals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | BinStringOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, String) && Typ.eq(ty2, String)) {
      switch (op) {
      | Concat => Some(Typ.String)
      | Equals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | ListLit(_, _, ty, _) => Some(List(ty))
  | Cons(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (ty2) {
    | List(ty3) when Typ.eq(ty3, ty1) => Some(ty2)
    | _ => None
    };
  | ListConcat(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (ty1, ty2) {
    | (List(ty1), List(ty2)) when Typ.eq(ty1, ty2) => Some(Typ.List(ty1))
    | _ => None
    };
  | Tuple(dhs) =>
    let+ typ_list =
      dhs |> List.map(typ_of_dhexp(ctx, m)) |> OptUtil.sequence;
    Typ.Prod(typ_list);
  | Prj(dh, i) =>
    let* ty = typ_of_dhexp(ctx, m, dh);
    switch (ty) {
    | Prod(l) when List.length(l) > i => Some(List.nth(l, i))
    | _ => None
    };
  | Constructor(_, typ) => Some(typ)
  | ConsistentCase(Case(d_scrut, d_rules, _)) =>
    let* ty' = typ_of_dhexp(ctx, m, d_scrut);
    let* typ_cases: list(Typ.t) =
      d_rules
      |> List.map((DHExp.Rule(dhp, de)) => {
           let* ctx = dhpat_extend_ctx(dhp, ty', ctx);
           typ_of_dhexp(ctx, m, de);
         })
      |> OptUtil.sequence;
    equal_typ_list(typ_cases);
  | Cast(d, ty1, ty2) =>
    let* _ = Typ.join(~fix=true, ctx, ty1, ty2);
    let* tyd = typ_of_dhexp(ctx, m, d);
    Typ.eq(tyd, ty1) ? Some(ty2) : None;
  | FailedCast(d, ty1, ty2) =>
    if (ground(ty1) && ground(ty2) && !Typ.eq(ty1, ty2)) {
      let* tyd = typ_of_dhexp(ctx, m, d);
      Typ.eq(tyd, ty1) ? Some(ty2) : None;
    } else {
      None;
    }
  | InvalidOperation(d, _) => typ_of_dhexp(ctx, m, d)
  | IfThenElse(ConsistentIf, d_scrut, d1, d2) =>
    let* ty = typ_of_dhexp(ctx, m, d_scrut);
    if (Typ.eq(ty, Bool)) {
      let* ty1 = typ_of_dhexp(ctx, m, d1);
      let* ty2 = typ_of_dhexp(ctx, m, d2);
      equal_typ_list([ty1, ty2]);
    } else {
      None;
    };
  | IfThenElse(InconsistentIf, d_scrut, d1, d2) =>
    let* ty = typ_of_dhexp(ctx, m, d_scrut);
    if (Typ.eq(ty, Bool)) {
      let* _ = typ_of_dhexp(ctx, m, d1);
      let* _ = typ_of_dhexp(ctx, m, d2);
      Some(Typ.Unknown(Internal));
    } else {
      None;
    };
  };
};

let property_test = (uexp_typ: Typ.t, dhexp: DHExp.t, m: Statics.Map.t): bool => {
  let dhexp_typ = typ_of_dhexp(Builtins.ctx_init, m, dhexp);

  switch (dhexp_typ) {
  | None => false
  | Some(dh_typ) => Typ.eq(dh_typ, uexp_typ)
  };
};
