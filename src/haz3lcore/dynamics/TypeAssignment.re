open Util;
open OptUtil.Syntax;

// let equal_typ_list = (l: list(Typ.t)): option(Typ.t) => {
//   switch (l) {
//   | [] => None
//   | [ty, ..._] =>
//     List.fold_left((acc, t) => {acc && Typ.equal(t, ty)}, true, l)
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
    let ty' = ty;
    let ty =
      switch (ty.term) {
      | TupLabel(_, ty) => ty
      | _ => ty
      };
    switch (dhpat |> Pat.term_of) {
    | Var(name) =>
      let entry =
        Ctx.VarEntry({
          name,
          id: Id.invalid,
          typ: ty |> TypSlice.t_of_typ_t,
        });
      Some([entry]);
    | Label(name) =>
      Typ.equal(ty, Label(name) |> Typ.temp) ? Some([]) : None
    | TupLabel(_, dp1) =>
      switch (ty'.term) {
      | TupLabel(_, ty2)
          when
            LabeledTuple.has_same_labels(
              DHPat.match_tup_label(dhpat),
              Typ.match_tup_label(ty'),
            ) =>
        dhpat_var_entry(dp1, ty2)
      | TupLabel(_, _) => None
      | _ => dhpat_var_entry(dp1, ty)
      }
    | Tuple(l1) =>
      let (l1, ts) =
        Typ.matched_prod(ctx, l1, Pat.match_tup_label, ty, (name, b) =>
          TupLabel(Label(name) |> Pat.fresh, b) |> Pat.fresh
        );
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
      let* ctrs = Typ.get_sum_constructors(ctx, ty);
      let* typ = ConstructorMap.get_entry(name, ctrs);
      let* typ' = typ;
      dhpat_var_entry(dhp, typ');
    | Ap(_) => None
    | EmptyHole
    | Wild
    | Invalid(_)
    | MultiHole(_) => Some([])
    | Parens(dhp)
    | Probe(dhp, _) => dhpat_var_entry(dhp, ty)
    | Atom(c) =>
      Typ.equal(ty, Atom(Atom.cls_of_t(c)) |> Typ.temp) ? Some([]) : None
    | Constructor(_) => Some([]) // TODO: make this stricter
    | Cast(dhp, ty1, ty2) =>
      Typ.equal(ty, ty2 |> TypSlice.typ_of)
        ? dhpat_var_entry(dhp, ty1 |> TypSlice.typ_of) : None
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
  | Label(name) => Some(Label(name) |> Typ.temp)
  | TupLabel(dlab, d) =>
    let* tlab = dhpat_synthesize(dlab, ctx);
    let* ty = dhpat_synthesize(d, ctx);
    Some(TupLabel(tlab, ty) |> Typ.temp);
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
  | Parens(dhp)
  | Probe(dhp, _) => dhpat_synthesize(dhp, ctx)
  | Atom(c) => Some(Atom(Atom.cls_of_t(c)) |> Typ.temp)
  | Cast(_, _, ty) => Some(ty |> TypSlice.typ_of)
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
         Ctx.VarEntry({
           name,
           id: Id.invalid,
           typ: ty |> TypSlice.t_of_typ_t,
         });
       })
    |> OptUtil.sequence;
  List.fold_left((ctx, var_entry) => Ctx.extend(ctx, var_entry), ctx, l);
}

and typ_of_dhexp = (ctx: Ctx.t, m: Statics.Map.t, dh: DHExp.t): option(Typ.t) => {
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
    Some(var.typ |> TypSlice.typ_of);
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
  | Fun(dhp, d, ty, _) =>
    let* ty_p =
      switch (ty) {
      | None => dhpat_synthesize(dhp, ctx)
      | Some(t) => Some(t |> TypSlice.typ_of)
      };

    let* ctx = dhpat_extend_ctx(dhp, ty_p, ctx);
    let* ty2 = typ_of_dhexp(ctx, m, d);
    Some(Arrow(ty_p, ty2) |> Typ.temp);
  | TypFun({term: Var(name), _} as utpat, d, _)
      when !Ctx.shadows_typ(ctx, name) =>
    let ctx =
      Ctx.extend_tvar(
        ctx,
        {
          name,
          id: TPat.rep_id(utpat),
          kind: Abstract,
        },
      );
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(Forall(utpat, ty) |> Typ.temp);
  | TypFun(_, d, _) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(Forall(Var("?") |> TPat.fresh, ty) |> Typ.temp);
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
    Typ.equal(tyl, ty2) ? Some(tyr) : None;
  | DeferredAp(d1, d2s) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* tys = List.map(typ_of_dhexp(ctx, m), d2s) |> OptUtil.sequence;
    let* (tyl, tyr) = Typ.matched_arrow_strict(ctx, ty1);
    let* tyls =
      Typ.matched_args_strict(ctx, tyl, List.length(tys))
      |> (
        fun
        | L(x) => Some(x)
        | R(_) => None
      );
    let* combined = ListUtil.combine_opt(tyls, d2s);
    let without_deferrals =
      List.filter(((_, d)) => !DHExp.is_deferral(d), combined);
    if (List.for_all(
          ((t, d)) => {
            let ty = typ_of_dhexp(ctx, m, d);
            switch (ty) {
            | Some(ty) => Typ.equal(t, ty)
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
      | [ty] => Some(Arrow(ty, tyr) |> Typ.temp)
      | tys => Some(Arrow(Prod(tys) |> Typ.temp, tyr) |> Typ.temp)
      };
    } else {
      None;
    };

  | BuiltinFun(name) =>
    let* var = Ctx.lookup_var(ctx, name);
    Some(var.typ |> TypSlice.typ_of);
  | Test(dtest) =>
    let* ty = typ_of_dhexp(ctx, m, dtest);
    Typ.equal(ty, Atom(Bool) |> Typ.temp)
      ? Some(Prod([]) |> Typ.temp) : None;
  | Atom(c) => Some(Atom(c |> Atom.cls_of_t) |> Typ.temp)
  | BinOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    let semantics = Operators.semantics_of_bin_op(op);
    switch (semantics) {
    | Undefined(_) =>
      Typ.equal(ty1, Unknown(Internal) |> Typ.temp)
      && Typ.equal(ty2, Unknown(Internal) |> Typ.temp)
        ? Some(Unknown(Internal) |> Typ.temp) : None
    | Defined(ty1', ty2', ty_out, _) =>
      let ty1' = Atom(Atom.cls_of_kind(ty1')) |> Typ.temp;
      let ty2' = Atom(Atom.cls_of_kind(ty2')) |> Typ.temp;
      let ty_out = Atom(Atom.cls_of_kind(ty_out)) |> Typ.temp;
      Typ.equal(ty1, ty1') && Typ.equal(ty2, ty2') ? Some(ty_out) : None;
    };
  | UnOp(Int(Minus) | Nat(Minus) | Float(Minus) | SInt(Minus), d) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Typ.equal(ty, Atom(Int) |> Typ.temp)
      ? Some(Atom(Int) |> Typ.temp) : None;
  | UnOp(Bool(Not), d) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Typ.equal(ty, Atom(Bool) |> Typ.temp)
      ? Some(Atom(Bool) |> Typ.temp) : None;
  | UnOp(Meta(Unquote), d) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(ty);
  | ListLit([]) => Some(List(Unknown(Internal) |> Typ.temp) |> Typ.temp)
  | ListLit([x, ...xs]) =>
    let* t_x = typ_of_dhexp(ctx, m, x);
    let* t_xs = List.map(typ_of_dhexp(ctx, m), xs) |> OptUtil.sequence;
    List.for_all(t => Typ.equal(t, t_x), t_xs)
      ? Some(List(t_x) |> Typ.temp) : None;
  | Cons(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    let* ty3 = Typ.matched_list_strict(ctx, ty2);
    Typ.equal(ty1, ty3) ? Some(ty2) : None;
  | ListConcat(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty1l = Typ.matched_list_strict(ctx, ty1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    let* ty2l = Typ.matched_list_strict(ctx, ty2);
    Typ.equal(ty1l, ty2l) ? Some(ty1) : None;
  | Label(name) => Some(Label(name) |> Typ.temp)
  | TupLabel(dlab, d) =>
    let* tlab = typ_of_dhexp(ctx, m, dlab);
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(TupLabel(tlab, ty) |> Typ.temp);
  | Dot(d1, d2) =>
    switch (d1.term, d2.term) {
    | (Tuple(ds), Label(name)) =>
      let element = LabeledTuple.find_label(Exp.match_tup_label, ds, name);
      switch (element) {
      | Some({term: TupLabel(_, exp), _}) => typ_of_dhexp(ctx, m, exp)
      | _ => None
      };
    | (TupLabel(_, de), Label(name))
        when
          LabeledTuple.has_same_labels(
            Exp.match_tup_label(d1),
            Some((name, d2)),
          ) =>
      typ_of_dhexp(ctx, m, de)
    | _ => None
    }
  | Tuple(dhs) =>
    let+ typ_list =
      dhs |> List.map(typ_of_dhexp(ctx, m)) |> OptUtil.sequence;
    Prod(typ_list) |> Typ.temp;
  | Constructor(_) => None // Constructors should always be surrounded by casts
  | Match(_, []) => Some(Unknown(Internal) |> Typ.temp)
  | Match(d_scrut, [rule, ...rules]) =>
    let* ty' = typ_of_dhexp(ctx, m, d_scrut);
    let rule_to_ty = ((dhpat, dhexp): (Pat.t, Exp.t)) => {
      let* ctx = dhpat_extend_ctx(dhpat, ty', ctx);
      typ_of_dhexp(ctx, m, dhexp);
    };
    let* rule_ty = rule_to_ty(rule);
    let* rules_ty = List.map(rule_to_ty, rules) |> OptUtil.sequence;
    List.for_all(Typ.equal(rule_ty, _), rules_ty) ? Some(rule_ty) : None;
  | Cast(d, ty1, ty2) =>
    let* _ = Typ.join(ctx, ty1 |> TypSlice.typ_of, ty2 |> TypSlice.typ_of);
    let* tyd = typ_of_dhexp(ctx, m, d);
    Typ.equal(tyd, ty1 |> TypSlice.typ_of)
      ? Some(ty2 |> TypSlice.typ_of) : None;
  | FailedCast(d, ty1, ty2) =>
    if (ground(ty1 |> TypSlice.typ_of)
        && ground(ty2 |> TypSlice.typ_of)
        && !Typ.equal(ty1 |> TypSlice.typ_of, ty2 |> TypSlice.typ_of)) {
      let* tyd = typ_of_dhexp(ctx, m, d);
      Typ.equal(tyd, ty1 |> TypSlice.typ_of)
        ? Some(ty2 |> TypSlice.typ_of) : None;
    } else {
      None;
    }
  | If(d_scrut, d1, d2) =>
    let* ty = typ_of_dhexp(ctx, m, d_scrut);
    if (Typ.equal(ty, Atom(Bool) |> Typ.temp)) {
      let* ty1 = typ_of_dhexp(ctx, m, d1);
      let* ty2 = typ_of_dhexp(ctx, m, d2);
      Typ.equal(ty1, ty2) ? Some(ty1) : None;
    } else {
      None;
    };
  | Use(_, d)
  | TyAlias(_, _, d) => typ_of_dhexp(ctx, m, d)
  | Parens(d)
  | Probe(d, _) => typ_of_dhexp(ctx, m, d)
  };
};

let property_test = (uexp_typ: Typ.t, dhexp: DHExp.t, m: Statics.Map.t): bool => {
  let dhexp_typ = typ_of_dhexp(Builtins.ctx_init(None), m, dhexp);

  switch (dhexp_typ) {
  | None => false
  | Some(dh_typ) => Typ.equal(dh_typ, uexp_typ)
  };
};
