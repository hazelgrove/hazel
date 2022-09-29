open Util;
open OptUtil.Syntax;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

let ctx_to_varctx = (ctx: Ctx.t): VarCtx.t =>
  List.map(((k, {typ, _}: Ctx.entry)) => (k, typ), ctx);

let int_op_of: Term.UExp.op_bin_int => DHExp.BinIntOp.t =
  fun
  | Plus => Plus
  | Minus => Minus
  | Times => Times
  | Divide => Divide
  | LessThan => LessThan
  | LessThanOrEqual => LessThanOrEqual
  | GreaterThan => GreaterThan
  | GreaterThanOrEqual => GreaterThanOrEqual
  | Equals => Equals;

let float_op_of: Term.UExp.op_bin_float => DHExp.BinFloatOp.t =
  fun
  | Plus => FPlus
  | Minus => FMinus
  | Times => FTimes
  | Divide => FDivide
  | LessThan => FLessThan
  | LessThanOrEqual => FLessThanOrEqual
  | GreaterThan => FGreaterThan
  | GreaterThanOrEqual => FGreaterThanOrEqual
  | Equals => FEquals;

let string_op_of: Term.UExp.op_bin_string => DHExp.BinStringOp.t =
  fun
  | Equals => SEquals;

let bool_op_of: Term.UExp.op_bin_bool => DHExp.BinBoolOp.t =
  fun
  | And => And
  | Or => Or;

let exp_binop_of: Term.UExp.op_bin => (Typ.t, (_, _) => DHExp.t) =
  fun
  | Int(op) => (Int, ((e1, e2) => BinIntOp(int_op_of(op), e1, e2)))
  | Float(op) => (Float, ((e1, e2) => BinFloatOp(float_op_of(op), e1, e2)))
  | Bool(op) => (Bool, ((e1, e2) => BinBoolOp(bool_op_of(op), e1, e2)))
  | String(op) => (
      String,
      ((e1, e2) => BinStringOp(string_op_of(op), e1, e2)),
    );

let rec dhexp_of_uexp = (m: Statics.map, uexp: Term.UExp.t): option(DHExp.t) => {
  /* NOTE: Left out delta for now */
  switch (Id.Map.find_opt(Term.UExp.rep_id(uexp), m)) {
  | Some(InfoExp({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = Term.UExp.rep_id(uexp); /* NOTE: using term uids for hole ids */
    let wrap = (d: DHExp.t): option(DHExp.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, d))
      };
    switch (uexp.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some(EmptyHole(u, 0))
    | MultiHole(tms) =>
      // TODO: dhexp, eval for multiholes
      let* ds =
        tms
        |> List.map(
             fun
             | Term.Exp(e) => dhexp_of_uexp(m, e)
             | tm => Some(EmptyHole(Term.rep_id(tm), 0)),
           )
        |> OptUtil.sequence;
      switch (ds) {
      | [] => Some(DHExp.EmptyHole(u, 0))
      | [hd, ...tl] =>
        // placeholder logic: sequence
        tl |> List.fold_left((acc, d) => DHExp.Sequence(d, acc), hd) |> wrap
      };
    | Triv => wrap(Triv)
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | String(s) => wrap(StringLit(s))
    | ListLit(es) =>
      //TODO: rewrite this whole case
      switch (Statics.exp_mode(m, uexp)) {
      | Syn =>
        let ty = Typ.matched_list(Statics.exp_self_typ(m, uexp));
        let* ds =
          List.fold_left(
            (acc, e) => {
              let* acc = acc;
              let e_ty = Statics.exp_self_typ(m, e);
              let+ d = dhexp_of_uexp(m, e);
              let dc = DHExp.cast(d, e_ty, ty);
              acc @ [dc];
            },
            Some([]),
            es,
          );
        wrap(DHExp.ListLit(u, 0, StandardErrStatus(NotInHole), Int, ds));
      | Ana(ana_ty) =>
        let ty = Typ.matched_list(ana_ty);
        let* ds =
          List.fold_left(
            (acc, e) => {
              let* acc = acc;
              let e_ty = Statics.exp_self_typ(m, e);
              let+ d = dhexp_of_uexp(m, e);
              let dc = DHExp.cast(d, e_ty, ty);
              acc @ [dc];
            },
            Some([]),
            es,
          );
        wrap(ListLit(u, 0, StandardErrStatus(NotInHole), Int, ds));
      }
    | Fun(p, body) =>
      let* dp = dhpat_of_upat(m, p);
      let* d1 = dhexp_of_uexp(m, body);
      let ty1 = Statics.pat_typ(m, p);
      wrap(DHExp.Fun(dp, ty1, d1));
    | Tuple(es) =>
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
    | Tag(name) => wrap(Tag(name))
    | Cons(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let ty1 = Statics.exp_self_typ(m, e1);
      let ty2 = Statics.exp_self_typ(m, e2);
      let dc1 =
        switch (Statics.exp_mode(m, uexp)) {
        | Syn => d1
        | Ana(ty_ana) =>
          let ty = Typ.matched_list(ty_ana);
          DHExp.cast(d1, ty1, ty);
        };
      let ty_hd = Typ.matched_list(Statics.exp_self_typ(m, uexp));
      let dc2 = DHExp.cast(d2, ty2, List(ty_hd));
      wrap(Cons(dc1, dc2));
    | UnOp(Int(Minus), e) =>
      let* d = dhexp_of_uexp(m, e);
      let ty = Statics.exp_self_typ(m, e);
      let dc = DHExp.cast(d, ty, Int);
      wrap(BinIntOp(Minus, IntLit(0), dc));
    | BinOp(op, e1, e2) =>
      let (ty, cons) = exp_binop_of(op);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let ty1 = Statics.exp_self_typ(m, e1);
      let ty2 = Statics.exp_self_typ(m, e2);
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
      | InHole(Free(Variable)) => Some(FreeVar(u, 0, name))
      | _ => wrap(BoundVar(name))
      }
    | Let(
        {term: TypeAnn({term: Var(x), _}, {term: Arrow(_), _}), _} as p,
        {term: Fun(_), _} as def,
        body,
      ) =>
      /* NOTE: recursive case */
      let pat_typ = Statics.pat_self_typ(m, p);
      let def_typ = Statics.exp_self_typ(m, def);
      let* p = dhpat_of_upat(m, p);
      let* def = dhexp_of_uexp(m, def);
      let* body = dhexp_of_uexp(m, body);
      let cast_var = DHExp.cast(BoundVar(x), def_typ, pat_typ);
      let def_subst = Substitution.subst_var(cast_var, x, def);
      wrap(Let(p, FixF(x, def_typ, def_subst), body));
    | Let(p, def, body) =>
      let* dp = dhpat_of_upat(m, p);
      let* ddef = dhexp_of_uexp(m, def);
      let* dbody = dhexp_of_uexp(m, body);
      wrap(Let(dp, ddef, dbody));
    | Ap(fn, arg) =>
      let* d_fn = dhexp_of_uexp(m, fn);
      let* d_arg = dhexp_of_uexp(m, arg);
      let ty_fn = Statics.exp_self_typ(m, fn);
      let ty_arg = Statics.exp_self_typ(m, arg);
      let (ty_in, ty_out) = Typ.matched_arrow(ty_fn);
      let c_fn = DHExp.cast(d_fn, ty_fn, Typ.Arrow(ty_in, ty_out));
      let c_arg = DHExp.cast(d_arg, ty_arg, ty_in);
      wrap(Ap(c_fn, c_arg));
    | If(scrut, e1, e2) =>
      let* d_scrut = dhexp_of_uexp(m, scrut);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let d_rules =
        DHExp.[Rule(BoolLit(true), d1), Rule(BoolLit(false), d2)];
      let d = DHExp.Case(d_scrut, d_rules, 0);
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(DHExp.InconsistentBranches(u, 0, d))
      | _ => wrap(ConsistentCase(d))
      };
    | Match(scrut, rules) =>
      let* d_scrut = dhexp_of_uexp(m, scrut);
      let* d_rules =
        List.map(
          ((p, e)) => {
            let* d_p = dhpat_of_upat(m, p);
            let+ d_e = dhexp_of_uexp(m, e);
            DHExp.Rule(d_p, d_e);
          },
          rules,
        )
        |> OptUtil.sequence;
      let d = DHExp.Case(d_scrut, d_rules, 0);
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(DHExp.InconsistentBranches(u, 0, d))
      | _ => wrap(ConsistentCase(d))
      };
    };
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => None
  };
}
and dhpat_of_upat = (m: Statics.map, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(Term.UPat.rep_id(upat), m)) {
  | Some(InfoPat({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = Term.UPat.rep_id(upat); /* NOTE: using term uids for hole ids */
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
    | String(s) => wrap(StringLit(s))
    | ListLit(ps) =>
      let ty = Typ.matched_list(Statics.pat_self_typ(m, upat));
      let* ds =
        List.fold_left(
          (acc, p) => {
            let* acc = acc;
            let+ d = dhpat_of_upat(m, p);
            acc @ [d];
          },
          Some([]),
          ps,
        );
      wrap(ListLit(ty, ds));
    | Tag(name) => wrap(Tag(name))
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap(Cons(d_hd, d_tl));
    | Tuple(ps) =>
      //TODO(andrew): review below
      switch (List.rev(ps)) {
      | [] => wrap(Triv)
      | [_] => failwith("ERROR: Tuple with one element")
      | [p0, ...ps] =>
        let* ds =
          List.fold_left(
            (acc, p) => {
              let* acc = acc;
              let+ d = dhpat_of_upat(m, p);
              DHPat.Pair(d, acc);
            },
            dhpat_of_upat(m, p0),
            ps,
          );
        wrap(ds);
      }
    | Var(name) => Some(Var(name))
    | Parens(p) => dhpat_of_upat(m, p)
    | Ap(p1, p2) =>
      let* d_p1 = dhpat_of_upat(m, p1);
      let* d_p2 = dhpat_of_upat(m, p2);
      wrap(Ap(d_p1, d_p2));
    | TypeAnn(p, _ty) =>
      let* dp = dhpat_of_upat(m, p);
      wrap(dp);
    };
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => None
  };
};

let uexp_elab = (m: Statics.map, uexp: Term.UExp.t): ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp)) {
  | None => DoesNotElaborate
  | Some(d) => Elaborates(d, Typ.Unknown(Internal), Delta.empty) //TODO: get type from ci
  };
