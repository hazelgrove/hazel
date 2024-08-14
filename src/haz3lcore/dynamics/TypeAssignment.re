open Util;
open OptUtil.Syntax;

// let equal_typ_list = (l: list(Typ.t)): option(Typ.t) => {
//   switch (l) {
//   | [] => None
//   | [ty, ..._] =>
//     List.fold_left((acc, t) => {acc && Typ.eq(t, ty)}, true, l)
//       ? Some(ty) : None
//   };
// };

// let delta_ty = (id: MetaVar.t, m: Statics.Map.t): option(Typ.t) => {
//   switch (Id.Map.find_opt(id, m)) {
//   | Some(InfoExp({mode, ctx, _})) =>
//     switch (mode) {
//     | Syn
//     | SynTypFun
//     | SynFun => Some(Unknown(Internal))
//     | Ana(ana_ty) => Some(Typ.normalize(ctx, ana_ty))
//     }
//   | _ => None
//   };
// };

let ground = (ty: Typ.t): bool => {
  switch (Casts.ground_cases_of(ty)) {
  | Casts.Ground => true
  | _ => false
  };
};

let dhpat_extend_ctx = (dhpat: DHPat.t, ty: Typ.t, ctx: Ctx.t): option(Ctx.t) => {
  let rec dhpat_var_entry =
          (dhpat: DHPat.t, ty: Typ.t): option(list(Ctx.entry)) => {
    switch (dhpat |> Pat.term_of) {
    | Var(name) =>
      let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty});
      Some([entry]);
    | Tuple(l1) =>
      let* ts = Typ.matched_prod_strict(ctx, List.length(l1), ty);
      let* l =
        List.map2((dhp, typ) => {dhpat_var_entry(dhp, typ)}, l1, ts)
        |> OptUtil.sequence;
      Some(List.concat(l));
    | Cons(dhp1, dhp2) =>
      let* t = Typ.matched_list_strict(ctx, ty);
      let* l1 = dhpat_var_entry(dhp1, t);
      let* l2 = dhpat_var_entry(dhp2, List(t) |> Typ.temp);
      Some(l1 @ l2);
    | ListLit(l) =>
      let* t = Typ.matched_list_strict(ctx, ty);
      let* l =
        List.map(dhp => {dhpat_var_entry(dhp, t)}, l) |> OptUtil.sequence;
      Some(List.concat(l));
    | Ap({term: Constructor(name, _), _}, dhp) =>
      // TODO: make this stricter
      let* ctrs = Typ.get_sum_constructors(ctx, ty);
      let* typ = ConstructorMap.get_entry(name, ctrs);
      let* (ty1, ty2) = Typ.matched_arrow_strict(ctx, typ);
      Typ.eq(ty2, ty) ? dhpat_var_entry(dhp, ty1) : None;
    | Ap(_) => None
    | EmptyHole
    | Wild
    | Invalid(_)
    | MultiHole(_) => Some([])
    | Parens(dhp) => dhpat_var_entry(dhp, ty)
    | Int(_) => Typ.eq(ty, Int |> Typ.temp) ? Some([]) : None
    | Float(_) => Typ.eq(ty, Float |> Typ.temp) ? Some([]) : None
    | Bool(_) => Typ.eq(ty, Bool |> Typ.temp) ? Some([]) : None
    | String(_) => Typ.eq(ty, String |> Typ.temp) ? Some([]) : None
    | Constructor(_) => Some([]) // TODO: make this stricter
    | Cast(dhp, ty1, ty2) =>
      Typ.eq(ty, ty2) ? dhpat_var_entry(dhp, ty1) : None
    };
  };
  let+ l = dhpat_var_entry(dhpat, ty);
  List.fold_left((ctx, entry) => Ctx.extend(ctx, entry), ctx, l);
};

/* patterns in functions and fixpoints must have a synthesizable type */
let rec dhpat_synthesize = (dhpat: DHPat.t, ctx: Ctx.t): option(Typ.t) => {
  switch (dhpat |> Pat.term_of) {
  | Var(_)
  | Constructor(_)
  | Ap(_) => None
  | Tuple(dhs) =>
    let* l = List.map(dhpat_synthesize(_, ctx), dhs) |> OptUtil.sequence;
    Some(Prod(l) |> Typ.temp);
  | Cons(dhp1, _) =>
    let* t = dhpat_synthesize(dhp1, ctx);
    Some(List(t) |> Typ.temp);
  | ListLit([]) => Some(List(Unknown(Internal) |> Typ.temp) |> Typ.temp)
  | ListLit([x, ..._]) =>
    let* t_x = dhpat_synthesize(x, ctx);
    Some(List(t_x) |> Typ.temp);
  | EmptyHole => Some(Unknown(Internal) |> Typ.temp)
  | Wild => Some(Unknown(Internal) |> Typ.temp)
  | Invalid(_)
  | MultiHole(_) => Some(Unknown(Internal) |> Typ.temp)
  | Parens(dhp) => dhpat_synthesize(dhp, ctx)
  | Int(_) => Some(Int |> Typ.temp)
  | Float(_) => Some(Float |> Typ.temp)
  | Bool(_) => Some(Bool |> Typ.temp)
  | String(_) => Some(String |> Typ.temp)
  | Cast(_, _, ty) => Some(ty)
  };
};

let rec env_extend_ctx =
        (env: ClosureEnvironment.t, m: Statics.Map.t, ctx: Ctx.t)
        : option(Ctx.t) => {
  let+ l =
    env
    |> ClosureEnvironment.to_list
    |> List.map(((name, de)) => {
         let+ ty = typ_of_dhexp(ctx, m, de);
         Ctx.VarEntry({name, id: Id.invalid, typ: ty});
       })
    |> OptUtil.sequence;
  List.fold_left((ctx, var_entry) => Ctx.extend(ctx, var_entry), ctx, l);
}

and typ_of_dhexp =
    (ctx: Ctx.t, m: Statics.Map.t, dh: DHExp.t(list(Id.t))): option(Typ.t) => {
  switch (dh |> DHExp.term_of) {
  | Invalid(_)
  | MultiHole(_)
  | EmptyHole
  | Deferral(_)
  | Undefined => Some(Unknown(Internal) |> Typ.temp)
  | DynamicErrorHole(e, _) => typ_of_dhexp(ctx, m, e)
  | Closure(env, d) =>
    let* ctx' = env_extend_ctx(env, m, ctx);
    typ_of_dhexp(ctx', m, d);
  | Filter(_, d) => typ_of_dhexp(ctx, m, d)
  | Var(name) =>
    let* var = Ctx.lookup_var(ctx, name);
    Some(var.typ);
  | Seq(d1, d2) =>
    let* _ = typ_of_dhexp(ctx, m, d1);
    typ_of_dhexp(ctx, m, d2);
  | Let(dhp, de, db) =>
    let* ty1 = typ_of_dhexp(ctx, m, de);
    let* ctx = dhpat_extend_ctx(dhp, ty1, ctx);
    typ_of_dhexp(ctx, m, db);
  | FixF(dhp, d, env) =>
    let* ty_p = dhpat_synthesize(dhp, ctx);
    let* ctx =
      switch (env) {
      | None => Some(ctx)
      | Some(env) => env_extend_ctx(env, m, ctx)
      };
    let* ctx = dhpat_extend_ctx(dhp, ty_p, ctx);
    typ_of_dhexp(ctx, m, d);
  | Fun(dhp, d, env, _) =>
    let* ty_p = dhpat_synthesize(dhp, ctx);
    let* ctx =
      switch (env) {
      | None => Some(ctx)
      | Some(env) => env_extend_ctx(env, m, ctx)
      };
    let* ctx = dhpat_extend_ctx(dhp, ty_p, ctx);
    let* ty2 = typ_of_dhexp(ctx, m, d);
    Some(Typ.Arrow(ty_p, ty2) |> Typ.temp);
  | TypFun({term: Var(name), _} as utpat, d, _)
      when !Ctx.shadows_typ(ctx, name) =>
    let ctx =
      Ctx.extend_tvar(ctx, {name, id: TPat.rep_id(utpat), kind: Abstract});
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(Typ.Forall(utpat, ty) |> Typ.temp);
  | TypFun(_, d, _) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(Typ.Forall(Var("?") |> TPat.fresh, ty) |> Typ.temp);
  | TypAp(d, ty1) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    let* (name, ty2) = Typ.matched_forall_strict(ctx, ty);
    switch (name) {
    | Some(name) => Some(Typ.subst(ty1, name, ty2))
    | None => Some(ty2)
    };
  | Ap(_, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    let* (tyl, tyr) = Typ.matched_arrow_strict(ctx, ty1);
    Typ.eq(tyl, ty2) ? Some(tyr) : None;
  | DeferredAp(d1, d2s) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* tys = List.map(typ_of_dhexp(ctx, m), d2s) |> OptUtil.sequence;
    let* (tyl, tyr) = Typ.matched_arrow_strict(ctx, ty1);
    // TODO: make strict
    let tyls = Typ.matched_args(ctx, List.length(tys), tyl);
    let* combined = ListUtil.combine_opt(tyls, d2s);
    let without_deferrals =
      List.filter(((_, d)) => !DHExp.is_deferral(d), combined);
    if (List.for_all(
          ((t, d)) => {
            let ty = typ_of_dhexp(ctx, m, d);
            switch (ty) {
            | Some(ty) => Typ.eq(t, ty)
            | None => false
            };
          },
          without_deferrals,
        )) {
      let with_deferrals =
        List.filter(((_, d)) => DHExp.is_deferral(d), combined);
      let* tys =
        List.map(((_, d)) => typ_of_dhexp(ctx, m, d), with_deferrals)
        |> OptUtil.sequence;
      switch (tys) {
      | [] => Some(tyr)
      | [ty] => Some(Typ.Arrow(ty, tyr) |> Typ.temp)
      | tys => Some(Typ.Arrow(Prod(tys) |> Typ.temp, tyr) |> Typ.temp)
      };
    } else {
      None;
    };

  | BuiltinFun(name) =>
    let* var = Ctx.lookup_var(ctx, name);
    Some(var.typ);
  | Test(dtest) =>
    let* ty = typ_of_dhexp(ctx, m, dtest);
    Typ.eq(ty, Bool |> Typ.temp) ? Some(Typ.Prod([]) |> Typ.temp) : None;
  | Bool(_) => Some(Bool |> Typ.temp)
  | Int(_) => Some(Int |> Typ.temp)
  | Float(_) => Some(Float |> Typ.temp)
  | String(_) => Some(String |> Typ.temp)
  | BinOp(Bool(_), d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    Typ.eq(ty1, Bool |> Typ.temp) && Typ.eq(ty2, Bool |> Typ.temp)
      ? Some(Bool |> Typ.temp) : None;
  | BinOp(Int(op), d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Int |> Typ.temp) && Typ.eq(ty2, Int |> Typ.temp)) {
      switch (op) {
      | Minus
      | Plus
      | Times
      | Power
      | Divide => Some(Int |> Typ.temp)
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals
      | NotEquals => Some(Bool |> Typ.temp)
      };
    } else {
      None;
    };
  | BinOp(Float(op), d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Float |> Typ.temp) && Typ.eq(ty2, Float |> Typ.temp)) {
      switch (op) {
      | Minus
      | Plus
      | Times
      | Power
      | Divide => Some(Float |> Typ.temp)
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals
      | NotEquals => Some(Bool |> Typ.temp)
      };
    } else {
      None;
    };
  | BinOp(String(op), d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, String |> Typ.temp) && Typ.eq(ty2, String |> Typ.temp)) {
      switch (op) {
      | Concat => Some(String |> Typ.temp)
      | Equals => Some(Bool |> Typ.temp)
      };
    } else {
      None;
    };
  | UnOp(Int(Minus), d) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Typ.eq(ty, Int |> Typ.temp) ? Some(Int |> Typ.temp) : None;
  | UnOp(Bool(Not), d) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Typ.eq(ty, Bool |> Typ.temp) ? Some(Bool |> Typ.temp) : None;
  | UnOp(Meta(Unquote), d) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(ty);
  | ListLit([]) => Some(List(Unknown(Internal) |> Typ.temp) |> Typ.temp)
  | ListLit([x, ...xs]) =>
    let* t_x = typ_of_dhexp(ctx, m, x);
    let* t_xs = List.map(typ_of_dhexp(ctx, m), xs) |> OptUtil.sequence;
    List.for_all(t => Typ.eq(t, t_x), t_xs)
      ? Some(List(t_x) |> Typ.temp) : None;
  | Cons(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    let* ty3 = Typ.matched_list_strict(ctx, ty2);
    Typ.eq(ty1, ty3) ? Some(ty2) : None;
  | ListConcat(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty1l = Typ.matched_list_strict(ctx, ty1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    let* ty2l = Typ.matched_list_strict(ctx, ty2);
    Typ.eq(ty1l, ty2l) ? Some(ty1) : None;
  | Tuple(dhs) =>
    let+ typ_list =
      dhs |> List.map(typ_of_dhexp(ctx, m)) |> OptUtil.sequence;
    Prod(typ_list) |> Typ.temp;
  | Constructor(_) => None // Constructors should always be surrounded by casts
  | Match(_, []) => Some(Unknown(Internal) |> Typ.temp)
  | Match(d_scrut, [rule, ...rules]) =>
    let* ty' = typ_of_dhexp(ctx, m, d_scrut);
    let rule_to_ty = ((dhpat, dhexp): (Pat.t, Exp.t(list(Id.t)))) => {
      let* ctx = dhpat_extend_ctx(dhpat, ty', ctx);
      typ_of_dhexp(ctx, m, dhexp);
    };
    let* rule_ty = rule_to_ty(rule);
    let* rules_ty = List.map(rule_to_ty, rules) |> OptUtil.sequence;
    List.for_all(Typ.eq(rule_ty, _), rules_ty) ? Some(rule_ty) : None;
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
  | If(d_scrut, d1, d2) =>
    let* ty = typ_of_dhexp(ctx, m, d_scrut);
    if (Typ.eq(ty, Bool |> Typ.temp)) {
      let* ty1 = typ_of_dhexp(ctx, m, d1);
      let* ty2 = typ_of_dhexp(ctx, m, d2);
      Typ.eq(ty1, ty2) ? Some(ty1) : None;
    } else {
      None;
    };
  | TyAlias(_, _, d) => typ_of_dhexp(ctx, m, d)
  | Parens(d) => typ_of_dhexp(ctx, m, d)
  };
};

let property_test =
    (uexp_typ: Typ.t, dhexp: DHExp.t(list(Id.t)), m: Statics.Map.t): bool => {
  let dhexp_typ = typ_of_dhexp(Builtins.ctx_init, m, dhexp);

  switch (dhexp_typ) {
  | None => false
  | Some(dh_typ) => Typ.eq(dh_typ, uexp_typ)
  };
};
