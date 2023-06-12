open Util;
open OptUtil.Syntax;

let rec equal_typ = (l: list(Typ.t), ty: Typ.t): bool => {
  switch (l) {
  | [] => true
  | [hd, ...tl] =>
    if (hd != ty) {
      false;
    } else {
      equal_typ(tl, ty);
    }
  };
};

let rule_prj = (dr: DHExp.rule): (DHPat.t, DHExp.t) => {
  switch (dr) {
  | Rule(dhp, dh) => (dhp, dh)
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
    let entry = Ctx.VarEntry({name, id: 0, typ: ty});
    Ctx.extend(entry, ctx);
  | (Tuple(l1), Prod(l2)) =>
    List.fold_left2(
      (acc, dhp, typ) => {dhpat_extend_ctx(dhp, typ, acc)},
      ctx,
      l1,
      l2,
    )
  | (Cons(dhp1, dhp2), List(typ)) =>
    ctx |> dhpat_extend_ctx(dhp1, typ) |> dhpat_extend_ctx(dhp2, ty)
  | (ListLit(typ1, l), List(typ2)) =>
    if (typ1 == typ2) {
      List.fold_left(
        (acc, dhp) => {dhpat_extend_ctx(dhp, typ1, acc)},
        ctx,
        l,
      );
    } else {
      ctx;
    }
  | _ => ctx
  };
};

let rec typ_of_dhexp = (ctx: Ctx.t, dl: Delta.t, dh: DHExp.t): option(Typ.t) => {
  switch (dh) {
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | ListLit(_, _, _, ty, _) => Some(List(ty))
  | BoundVar(name) =>
    let+ var = Ctx.lookup_var(ctx, name);
    var.typ;
  | FreeVar(_, _, _) => Some(Unknown(Internal))
  | Fun(dhp, ty1, d, _) =>
    let+ ty2 = typ_of_dhexp(dhpat_extend_ctx(dhp, ty1, ctx), dl, d);
    Typ.Arrow(ty1, ty2);
  | Let(dhp, de, db) =>
    let* ty1 = typ_of_dhexp(ctx, dl, de);
    typ_of_dhexp(dhpat_extend_ctx(dhp, ty1, ctx), dl, db);
  | FixF(name, ty1, d) =>
    let entry = Ctx.VarEntry({name, id: 0, typ: ty1});
    typ_of_dhexp(Ctx.extend(entry, ctx), dl, d);
  | Ap(d1, d2) =>
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    let* ty = typ_of_dhexp(ctx, dl, d1);
    switch (ty) {
    | Arrow(tyl, tyr) =>
      if (tyl == ty2) {
        Some(tyr);
      } else {
        None;
      }
    | _ => None
    };
  | EmptyHole(id, _) =>
    //Find id in delta and get the type
    switch (Delta.find_opt(id, dl)) {
    | None => None
    | Some((_, ty, _)) => Some(ty)
    }
  | NonEmptyHole(_, id, _, d) =>
    switch (typ_of_dhexp(ctx, dl, d)) {
    | None => None
    | Some(_) =>
      switch (Delta.find_opt(id, dl)) {
      | Some((_, ty, _)) => Some(ty)
      | None => None
      }
    }
  | Cast(d, ty1, ty2) =>
    switch (Typ.join(ty1, ty2)) {
    | None => None
    | Some(_) =>
      let* tyd = typ_of_dhexp(ctx, dl, d);
      if (tyd == ty1) {
        Some(ty2);
      } else {
        None;
      };
    }
  | FailedCast(d, ty1, ty2) =>
    if (ground(ty1) && ground(ty2) && ty1 != ty2) {
      let* tyd = typ_of_dhexp(ctx, dl, d);
      if (tyd == ty1) {
        Some(ty2);
      } else {
        None;
      };
    } else {
      None;
    }
  | InconsistentBranches(id, _, Case(d_scrut, d_rules, _)) =>
    switch (typ_of_dhexp(ctx, dl, d_scrut)) {
    | Some(ty') =>
      let typ_cases =
        d_rules
        |> List.map(rule_prj)
        |> List.map(((dhp, de)) => {
             typ_of_dhexp(dhpat_extend_ctx(dhp, ty', ctx), dl, de)
           })
        |> OptUtil.sequence;

      switch (typ_cases) {
      | None => None
      | Some(_) =>
        switch (Delta.find_opt(id, dl)) {
        | Some((_, ty, _)) => Some(ty)
        | None => None
        }
      };
    | _ => None
    }
  | ConsistentCase(Case(d_scrut, d_rules, _)) =>
    switch (typ_of_dhexp(ctx, dl, d_scrut)) {
    | Some(ty') =>
      let* typ_cases =
        d_rules
        |> List.map(rule_prj)
        |> List.map(((dhp, de)) => {
             typ_of_dhexp(dhpat_extend_ctx(dhp, ty', ctx), dl, de)
           })
        |> OptUtil.sequence;
      let hd = List.hd(typ_cases);
      if (equal_typ(typ_cases, hd)) {
        Some(hd);
      } else {
        None;
      };
    | _ => None
    }
  | Sequence(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, dl, d1);
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    if (ty1 == Prod([])) {
      Some(ty2);
    } else {
      None;
    };
  | BinBoolOp(_, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, dl, d1);
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    if (ty1 == Bool && ty2 == Bool) {
      Some(Typ.Bool);
    } else {
      None;
    };
  | BinIntOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, dl, d1);
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    if (ty1 == Int && ty2 == Int) {
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
      | Equals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | BinFloatOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, dl, d1);
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    if (ty1 == Float && ty2 == Float) {
      switch (op) {
      | FMinus
      | FPlus
      | FTimes
      | FPower
      | FDivide => Some(Typ.Int)
      | FLessThan
      | FLessThanOrEqual
      | FGreaterThan
      | FGreaterThanOrEqual
      | FEquals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | BinStringOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, dl, d1);
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    if (ty1 == String && ty2 == String) {
      //In case a new string operation comes
      switch (op) {
      | SEquals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | Cons(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, dl, d1);
    let* ty2 = typ_of_dhexp(ctx, dl, d2);
    switch (ty2) {
    | List(Unknown(Internal)) => Some(Typ.List(ty1))
    | List(ty3) =>
      if (ty3 == ty1) {
        Some(Typ.List(ty1));
      } else {
        None;
      }
    | _ => None
    };
  | Tuple(dhs) =>
    let+ typ_list =
      dhs |> List.map(typ_of_dhexp(ctx, dl)) |> OptUtil.sequence;
    Typ.Prod(typ_list);
  | Prj(_) => None
  | Inj(_) => None
  | Tag(_) => None
  | _ => None
  };
};
