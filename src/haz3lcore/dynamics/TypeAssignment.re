open Util;
open OptUtil.Syntax;

let equal_typ_case = (l: list(Typ.t), ty: Typ.t): bool => {
  List.fold_left((acc, t) => {acc && Typ.eq(t, ty)}, true, l);
};

let rule_prj = (dr: DHExp.rule): (DHPat.t, DHExp.t) => {
  switch (dr) {
  | Rule(dhp, dh) => (dhp, dh)
  };
};

let arrow_aux = (ty: Typ.t): Typ.t => {
  switch (ty) {
  | Unknown(Internal) => Arrow(Unknown(Internal), Unknown(Internal))
  | _ => ty
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
  switch (ty) {
  | Bool
  | Int
  | Float
  | String
  | Prod([])
  | Arrow(Unknown(Internal), Unknown(Internal)) => true
  | _ => false
  };
};

let rec dhpat_extend_ctx = (dhpat: DHPat.t, ty: Typ.t, ctx: Ctx.t): Ctx.t => {
  switch (dhpat, ty) {
  | (Var(name), _) =>
    let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty});
    Ctx.extend(ctx, entry);
  | (Tuple(l1), Prod(l2)) =>
    if (List.length(l1) == List.length(l2)) {
      List.fold_left2(
        (acc, dhp, typ) => {dhpat_extend_ctx(dhp, typ, acc)},
        ctx,
        l1,
        l2,
      );
    } else {
      ctx;
    }
  | (Cons(dhp1, dhp2), List(typ)) =>
    ctx |> dhpat_extend_ctx(dhp1, typ) |> dhpat_extend_ctx(dhp2, ty)
  | (ListLit(typ1, l), List(typ2)) =>
    if (Typ.eq(typ1, typ2)) {
      List.fold_left(
        (acc, dhp) => {dhpat_extend_ctx(dhp, typ1, acc)},
        ctx,
        l,
      );
    } else {
      ctx;
    }
  | (Ap(Constructor(_, typ), dhp), _) =>
    let (ty1, ty2) = Typ.matched_arrow(ctx, typ);
    if (Typ.eq(ty2, ty)) {
      ctx |> dhpat_extend_ctx(dhp, ty1);
    } else {
      ctx;
    };
  | _ => ctx
  };
};

let rec typ_of_dhexp =
        (ctx: Ctx.t, m: Statics.Map.t, dh: DHExp.t): option(Typ.t) => {
  switch (dh) {
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | ListLit(_, _, ty, _) => Some(List(ty))
  | BoundVar(name) =>
    let+ var = Ctx.lookup_var(ctx, name);
    var.typ;
  | FreeVar(_) => Some(Unknown(Internal))
  | Fun(dhp, ty1, d, _) =>
    let+ ty2 = typ_of_dhexp(dhpat_extend_ctx(dhp, ty1, ctx), m, d);
    Typ.Arrow(ty1, ty2);
  | Let(dhp, de, db) =>
    let* ty1 = typ_of_dhexp(ctx, m, de);
    typ_of_dhexp(dhpat_extend_ctx(dhp, ty1, ctx), m, db);
  | FixF(name, ty1, d) =>
    let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty1});
    typ_of_dhexp(Ctx.extend(ctx, entry), m, d);
  | Ap(TestLit(_), dtest) =>
    let* ty = typ_of_dhexp(ctx, m, dtest);
    if (Typ.eq(ty, Bool)) {
      Some(Typ.Prod([]));
    } else {
      None;
    };
  | Ap(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    print_endline("AP");
    print_endline(Typ.show(ty1));
    print_endline(Typ.show(ty2));
    switch (arrow_aux(ty1)) {
    | Arrow(tyl, tyr) =>
      if (Typ.eq(tyl, ty2)) {
        if (Typ.eq(tyl, Unknown(Internal))) {
          Some(Typ.Unknown(Internal));
        } else {
          Some(tyr);
        };
      } else {
        None;
      }
    | _ => None
    };
  | EmptyHole(id, _) =>
    //Find id in delta and get the type
    delta_ty(id, m)
  | NonEmptyHole(_, id, _, d) =>
    switch (typ_of_dhexp(ctx, m, d)) {
    | None => None
    | Some(_) => delta_ty(id, m)
    }
  | Cast(d, ty1, ty2) =>
    switch (Typ.join(~fix=true, ctx, ty1, ty2)) {
    | None => None
    | Some(_) =>
      let* tyd = typ_of_dhexp(ctx, m, d);
      if (Typ.eq(tyd, ty1)) {
        Some(ty2);
      } else {
        None;
      };
    }
  | FailedCast(d, ty1, ty2) =>
    if (ground(ty1) && ground(ty2) && ty1 != ty2) {
      let* tyd = typ_of_dhexp(ctx, m, d);
      if (Typ.eq(tyd, ty1)) {
        Some(ty2);
      } else {
        None;
      };
    } else {
      None;
    }
  | InconsistentBranches(_, _, Case(d_scrut, d_rules, _)) =>
    switch (typ_of_dhexp(ctx, m, d_scrut)) {
    | Some(ty') =>
      let typ_cases =
        d_rules
        |> List.map(rule_prj)
        |> List.map(((dhp, de)) => {
             typ_of_dhexp(dhpat_extend_ctx(dhp, ty', ctx), m, de)
           })
        |> OptUtil.sequence;

      switch (typ_cases) {
      | None => None
      | Some(_) => Some(Unknown(Internal))
      };
    | _ => None
    }
  | ConsistentCase(Case(d_scrut, d_rules, _)) =>
    switch (typ_of_dhexp(ctx, m, d_scrut)) {
    | Some(ty') =>
      let* typ_cases =
        d_rules
        |> List.map(rule_prj)
        |> List.map(((dhp, de)) => {
             typ_of_dhexp(dhpat_extend_ctx(dhp, ty', ctx), m, de)
           })
        |> OptUtil.sequence;
      let hd = List.hd(typ_cases);
      if (equal_typ_case(typ_cases, hd)) {
        Some(hd);
      } else {
        None;
      };
    | _ => None
    }
  | Sequence(d1, d2) =>
    let* _ = typ_of_dhexp(ctx, m, d1);
    typ_of_dhexp(ctx, m, d2);
  | BinBoolOp(_, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Bool) && Typ.eq(ty2, Bool)) {
      Some(Typ.Bool);
    } else {
      None;
    };
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
  | Cons(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (ty2) {
    | List(Unknown(Internal)) => Some(Typ.List(ty1))
    | List(ty3) =>
      if (Typ.eq(ty3, ty1)) {
        Some(ty2);
      } else {
        None;
      }
    | _ => None
    };
  | Tuple(dhs) =>
    let+ typ_list =
      dhs |> List.map(typ_of_dhexp(ctx, m)) |> OptUtil.sequence;
    Typ.Prod(typ_list);
  | Constructor(_, typ) => Some(typ)
  | Closure(_, d1) => typ_of_dhexp(ctx, m, d1)
  | Prj(_) => None
  | _ => None
  };
};

let property_test = (uexp_typ: Typ.t, dhexp: DHExp.t, m: Statics.Map.t): unit => {
  let dhexp_typ = typ_of_dhexp(Builtins.ctx_init, m, dhexp);

  let dh_typ =
    switch (dhexp_typ) {
    | None => Typ.Unknown(Internal)
    | Some(ty) => ty
    };

  print_endline(Typ.show(dh_typ));
  print_endline(Typ.show(uexp_typ));

  if (dhexp_typ == None) {
    print_endline("DHEXP type got none");
  } else if (dh_typ == uexp_typ) {
    print_endline("Types equal");
  } else {
    print_endline("Types not equal");
  };
};
