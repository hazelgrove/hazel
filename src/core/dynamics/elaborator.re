open OptUtil.Syntax;

/* TODO: (eric) let eric z know what's changing */
let rec htyp_of_typ: Typ.t => HTyp.t =
  ty =>
    switch (Typ.to_syntax(ty)) {
    | Unknown(_) => Hole
    | Int => Int
    | Float => Float
    | Bool => Bool
    | List(t) => List(htyp_of_typ(Typ.of_syntax(t)))
    | Arrow(t1, t2) =>
      Arrow(
        htyp_of_typ(Typ.of_syntax(t1)),
        htyp_of_typ(Typ.of_syntax(t2)),
      )
    | Prod(tys) =>
      Prod(List.map(ty => htyp_of_typ(Typ.of_syntax(ty)), tys))
    | TyVar(_) => assert(false)
    };

let exp_htyp = (ctx, m, e) => htyp_of_typ(Statics.exp_typ(ctx, m, e));
let pat_htyp = (ctx, m, p) => htyp_of_typ(Statics.pat_typ(ctx, m, p));

let ctx_to_varctx = (ctx: Typ.Ctx.t): VarCtx.t =>
  Typ.Ctx.vars(ctx)
  |> List.map(((_, entry: Typ.Ctx.var_entry)) =>
       (entry.name, htyp_of_typ(entry.typ))
     );

/* List.map(((k, {typ, _}: Typ.Ctx.entry)) => (k, htyp_of_typ(typ)), ctx); */

let int_op_of: Term.UExp.op_bin_int => DHExp.BinIntOp.t =
  fun
  | Plus => Plus
  | Minus => Minus
  | Times => Times
  | Divide => Divide
  | LessThan => LessThan
  | GreaterThan => GreaterThan
  | Equals => Equals;

let float_op_of: Term.UExp.op_bin_float => DHExp.BinFloatOp.t =
  fun
  | Plus => FPlus
  | Minus => FMinus
  | Times => FTimes
  | Divide => FDivide
  | LessThan => FLessThan
  | GreaterThan => FGreaterThan
  | Equals => FEquals;

let bool_op_of: Term.UExp.op_bin_bool => DHExp.BinBoolOp.t =
  fun
  | And => And
  | Or => Or;

let exp_binop_of: Term.UExp.op_bin => (HTyp.t, (_, _) => DHExp.t) =
  fun
  | Int(op) => (Int, ((e1, e2) => BinIntOp(int_op_of(op), e1, e2)))
  | Float(op) => (Float, ((e1, e2) => BinFloatOp(float_op_of(op), e1, e2)))
  | Bool(op) => (Bool, ((e1, e2) => BinBoolOp(bool_op_of(op), e1, e2)));

let rec dhexp_of_uexp = (m: Statics.map, uexp: Term.UExp.t): option(DHExp.t) => {
  /* NOTE: Left out delta for now */
  switch (Id.Map.find_opt(uexp.id, m)) {
  | Some(InfoExp({ctx, mode, self, _})) =>
    let err_status = Statics.error_status(ctx, mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = uexp.id; /* NOTE: using term uids for hole ids */
    let gamma = ctx_to_varctx(ctx);
    let sigma = Environment.id_env(gamma);
    let wrap = (d: DHExp.t): option(DHExp.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, sigma, d))
      };
    switch (uexp.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some(EmptyHole(u, 0, sigma))
    | MultiHole(_, []) =>
      // TODO: dhexp, eval for multiholes
      Some(EmptyHole(u, 0, sigma))
    | MultiHole(_, [e0, ...es]) =>
      // TODO: dhexp, eval for multiholes
      // placeholder logic: sequence
      let* ds =
        List.fold_left(
          (acc, e) => {
            let* acc = acc;
            let+ d = dhexp_of_uexp(m, e);
            DHExp.Sequence(d, acc);
          },
          dhexp_of_uexp(m, e0),
          es,
        );
      wrap(ds);
    | Triv => wrap(Triv)
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | ListLit(_) =>
      //TODO: list literals. below is just placeholder
      wrap(ListNil(Hole))
    | Fun(p, body)
    | FunAnn(p, _, body) =>
      let* dp = dhpat_of_upat(ctx, m, p);
      let* d1 = dhexp_of_uexp(m, body);
      let ty1 = pat_htyp(ctx, m, p);
      wrap(DHExp.Lam(dp, ty1, d1));
    | Tuple(_ids, es) =>
      //TODO(andrew): review below
      switch (List.rev(es)) {
      | [] => wrap(Triv)
      | [_] => failwith("ERROR: Tuple with one element")
      | [e0, ...es] =>
        let* ds =
          List.fold_left(
            (acc, e) => {
              let* acc = acc;
              let+ d = dhexp_of_uexp(m, e);
              DHExp.Pair(d, acc);
            },
            dhexp_of_uexp(m, e0),
            es,
          );
        wrap(ds);
      }
    | Cons(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      wrap(Cons(d1, d2));
    | UnOp(Int(Minus), e) =>
      let* d = dhexp_of_uexp(m, e);
      let ty = exp_htyp(m, e);
      let dc = DHExp.cast(d, ty, Int);
      wrap(BinIntOp(Minus, IntLit(0), dc));
    | BinOp(op, e1, e2) =>
      let (ty, cons) = exp_binop_of(op);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let ty1 = exp_htyp(ctx, m, e1);
      let ty2 = exp_htyp(ctx, m, e2);
      let dc1 = DHExp.cast(d1, ty1, ty);
      let dc2 = DHExp.cast(d2, ty2, ty);
      wrap(cons(dc1, dc2));
    | Parens(e) => dhexp_of_uexp(m, e)
    | Seq(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      wrap(Sequence(d1, d2));
    | Test(test) =>
      let* dtest = dhexp_of_uexp(m, test);
      wrap(Ap(TestLit(u), dtest));
    | Var(name) =>
      switch (err_status) {
      | InHole(FreeVariable) => Some(FreeVar(u, 0, sigma, name))
      | _ => wrap(BoundVar(name))
      }
    | TyAlias(tpat, def, body) => (??)
    | Let(
        {term: TypeAnn({term: Var(x), _}, {term: Arrow(_), _}), _} as p,
        {term: Fun(_) | FunAnn(_), _} as def,
        body,
      )
    | LetAnn(
        {term: Var(x), _} as p,
        {term: Arrow(_), _},
        {term: Fun(_) | FunAnn(_), _} as def,
        body,
      ) =>
      /* NOTE: recursive case */
      let pat_typ = pat_htyp(ctx, m, p);
      let def_typ = exp_htyp(ctx, m, def);
      let* p = dhpat_of_upat(ctx, m, p);
      let* def = dhexp_of_uexp(m, def);
      let* body = dhexp_of_uexp(m, body);
      let cast_var = DHExp.cast(BoundVar(x), def_typ, pat_typ);
      let def_subst = Evaluator.subst_var(cast_var, x, def);
      wrap(Let(p, FixF(x, def_typ, def_subst), body));
    | Let(p, def, body)
    | LetAnn(p, _, def, body) =>
      let* dp = dhpat_of_upat(ctx, m, p);
      let* ddef = dhexp_of_uexp(m, def);
      let* dbody = dhexp_of_uexp(m, body);
      wrap(Let(dp, ddef, dbody));
    | Ap(fn, arg) =>
      let* d_fn = dhexp_of_uexp(m, fn);
      let* d_arg = dhexp_of_uexp(m, arg);
      let ty_fn = exp_htyp(ctx, m, fn);
      let ty_arg = exp_htyp(ctx, m, arg);
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
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(DHExp.InconsistentBranches(u, 0, sigma, d))
      | _ => wrap(ConsistentCase(d))
      };
    };
  | Some(InfoPat(_) | InfoTyp(_) | InfoTPat(_) | Invalid(_))
  | None => None
  };
}
and dhpat_of_upat =
    (ctx: Typ.Ctx.t, m: Statics.map, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(upat.id, m)) {
  | Some(InfoPat({mode, self, _})) =>
    let err_status = Statics.error_status(ctx, mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = upat.id; /* NOTE: using term uids for hole ids */
    let wrap = (d: DHPat.t): option(DHPat.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, d))
      };
    switch (upat.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some(EmptyHole(u, 0))
    | MultiHole(_) =>
      // TODO: dhexp, eval for multiholes
      Some(EmptyHole(u, 0))
    | Wild => wrap(Wild)
    | Triv => wrap(Triv)
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | ListNil => wrap(ListNil)
    | Tuple(_ids, ps) =>
      //TODO(andrew): review below
      switch (List.rev(ps)) {
      | [] => wrap(Triv)
      | [_] => failwith("ERROR: Tuple with one element")
      | [p0, ...ps] =>
        let* ds =
          List.fold_left(
            (acc, p) => {
              let* acc = acc;
              let+ d = dhpat_of_upat(ctx, m, p);
              DHPat.Pair(d, acc);
            },
            dhpat_of_upat(ctx, m, p0),
            ps,
          );
        wrap(ds);
      }
    | Var(name) => Some(Var(name))
    | Parens(p) => dhpat_of_upat(ctx, m, p)
    | TypeAnn(p, _ty) =>
      let* dp = dhpat_of_upat(ctx, m, p);
      wrap(dp);
    };
  | Some(InfoExp(_) | InfoTyp(_) | Invalid(_))
  | None => None
  };
};

let uexp_elab =
    (m: Statics.map, uexp: Term.UExp.t): Elaborator_Exp.ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp)) {
  | None => DoesNotElaborate
  | Some(d) => Elaborates(d, HTyp.Hole, Delta.empty) //TODO: get type from ci
  };
