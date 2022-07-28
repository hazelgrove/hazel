let rec htyp_of_typ: Typ.t => HTyp.t =
  fun
  | Unknown(_) => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | Arrow(t1, t2) => Arrow(htyp_of_typ(t1), htyp_of_typ(t2))
  | Prod(t1, t2) => Prod([htyp_of_typ(t1), htyp_of_typ(t2)]);

let ctx_to_varctx = (ctx: Ctx.t): VarCtx.t =>
  List.map(((k, {typ, _}: Ctx.entry)) => (k, htyp_of_typ(typ)), ctx);

let pat_htyp = (m: Statics.info_map, pat: Term.UPat.t) =>
  switch (Id.Map.find_opt(pat.id, m)) {
  | Some(InfoPat({mode, self, _})) =>
    Some(Typ.reconcile(mode, self) |> htyp_of_typ)
  | _ => None
  };

let exp_htyp = (m: Statics.info_map, exp: Term.UExp.t) =>
  switch (Id.Map.find_opt(exp.id, m)) {
  | Some(InfoExp({mode, self, _})) =>
    Some(Typ.reconcile(mode, self) |> htyp_of_typ)
  | _ => None
  };

let int_op_of: Term.UExp.exp_op_int => DHExp.BinIntOp.t =
  fun
  | Plus => Plus
  | Lt => LessThan;

let float_op_of: Term.UExp.exp_op_float => DHExp.BinFloatOp.t =
  fun
  | Plus => FPlus;

let bool_op_of: Term.UExp.exp_op_bool => DHExp.BinBoolOp.t =
  fun
  | And => And;

[@warning "-32"]
let rec dhexp_of_uexp =
        (m: Statics.info_map, uexp: Term.UExp.t): option(DHExp.t) => {
  /*
    simplifications:
    1. leave out delta for now
   */
  switch (Id.Map.find_opt(uexp.id, m)) {
  | Some(InfoExp({ctx, self, _}) as ci) =>
    open OptUtil.Syntax;
    let err_status = Statics.error_status(ci);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | AtLeast(_) => None
      | NotInHole => None
      | InHole => Some(TypeInconsistent)
      };
    let u = uexp.id; //NOTE: using term uids for hole ids
    let gamma = ctx_to_varctx(ctx);
    let sigma = Environment.id_env(gamma);
    //let delta = MetaVarMap.add(u, (Delta.ExpressionHole, ty, gamma), delta);
    let wrap = (d: DHExp.t): option(DHExp.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, sigma, d))
      };
    switch (uexp.term) {
    | Invalid(_) // NOTE: treating invalid as a hole for now
    | EmptyHole => Some(EmptyHole(u, 0, sigma))
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | Fun(p, body)
    | FunAnn(p, _, body) =>
      // TODO: annotated ty should already be incl in map ty; check this
      let* dp = dhpat_of_upat(m, p);
      let* d1 = dhexp_of_uexp(m, body);
      let* ty1 = pat_htyp(m, p);
      wrap(DHExp.Fun(dp, ty1, d1));
    | Pair(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      wrap(Pair(d1, d2));
    | Var(name) =>
      switch (self) {
      | Free => Some(FreeVar(u, 0, sigma, name))
      | _ => wrap(BoundVar(name))
      }
    | Let(p, def, body)
    | LetAnn(p, _, def, body) =>
      //TODO: recursive def
      let* dp = dhpat_of_upat(m, p);
      let* ddef = dhexp_of_uexp(m, def);
      let* dbody = dhexp_of_uexp(m, body);
      wrap(Let(dp, ddef, dbody));
    | Ap(fn, arg) =>
      let* d_fn = dhexp_of_uexp(m, fn);
      let* d_arg = dhexp_of_uexp(m, arg);
      let* ty_fn = exp_htyp(m, fn);
      let* ty_arg = exp_htyp(m, arg);
      let* (ty_in, ty_out) = HTyp.matched_arrow(ty_fn);
      let c_fn = DHExp.cast(d_fn, ty_fn, HTyp.Arrow(ty_in, ty_out));
      let c_arg = DHExp.cast(d_arg, ty_arg, ty_in);
      wrap(Ap(c_fn, c_arg));
    | If(cond, e1, e2) =>
      let* d_cond = dhexp_of_uexp(m, cond);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let d =
        DHExp.Case(
          d_cond,
          [Rule(BoolLit(true), d1), Rule(BoolLit(false), d2)],
          0,
        );
      switch (self) {
      | Joined([ty1, ty2]) when Typ.join(ty1, ty2) == None =>
        Some(DHExp.InconsistentBranches(u, 0, sigma, d))
      | _ => wrap(ConsistentCase(d))
      };
    | OpInt(op, e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let* ty1 = exp_htyp(m, e1);
      let* ty2 = exp_htyp(m, e2);
      let dc1 = DHExp.cast(d1, ty1, Int);
      let dc2 = DHExp.cast(d2, ty2, Int);
      wrap(BinIntOp(int_op_of(op), dc1, dc2));
    | OpFloat(op, e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let* ty1 = exp_htyp(m, e1);
      let* ty2 = exp_htyp(m, e2);
      let dc1 = DHExp.cast(d1, ty1, Int);
      let dc2 = DHExp.cast(d2, ty2, Int);
      wrap(BinFloatOp(float_op_of(op), dc1, dc2));
    | OpBool(op, e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let* ty1 = exp_htyp(m, e1);
      let* ty2 = exp_htyp(m, e2);
      let dc1 = DHExp.cast(d1, ty1, Bool);
      let dc2 = DHExp.cast(d2, ty2, Bool);
      wrap(BinBoolOp(bool_op_of(op), dc1, dc2));
    };
  | Some(InfoPat(_) | InfoTyp(_) | Invalid)
  | None => None
  };
}
[@warning "-32"]
and dhpat_of_upat = (m: Statics.info_map, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(upat.id, m)) {
  | Some(InfoPat(_) as ci) =>
    open OptUtil.Syntax;
    let err_status = Statics.error_status(ci);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | AtLeast(_) => None
      | NotInHole => None
      | InHole => Some(TypeInconsistent)
      };
    let u = upat.id; //NOTE: using term uids for hole ids
    let wrap = (d: DHPat.t): option(DHPat.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, d))
      };
    switch (upat.term) {
    | Invalid(_) // NOTE: treating invalid as a hole for now
    | EmptyHole => Some(EmptyHole(u, 0))
    | Wild => wrap(Wild)
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | Bool(b) => Some(BoolLit(b))
    | Var(name) => Some(Var(name))
    | Pair(p1, p2) =>
      let* d1 = dhpat_of_upat(m, p1);
      let* d2 = dhpat_of_upat(m, p2);
      wrap(Pair(d1, d2));
    };
  | Some(InfoExp(_) | InfoTyp(_) | Invalid)
  | None => None
  };
};

[@warning "-32"]
let uexp_elab =
    (m: Statics.info_map, uexp: Term.UExp.t)
    : Elaborator_Exp.ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp)) {
  | None => DoesNotElaborate
  | Some(d) => Elaborates(d, HTyp.Hole, Delta.empty) //TODO: get type from ci
  };
