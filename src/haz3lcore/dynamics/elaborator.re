open Util;
open OptUtil.Syntax;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

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
  | Equals => Equals
  | Power => Power;

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
  | Equals => FEquals
  | Power => FPower;

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

/* Wrap: Handles cast insertion and non-empty-hole wrapping
   for elaborated expressions */
let wrap = (ctx, u, mode, self, d: DHExp.t): option(DHExp.t) =>
  switch (Statics.error_status(ctx, mode, self)) {
  | NotInHole(_) =>
    switch (mode) {
    | Syn => Some(d)
    | SynFun =>
      /* Things in function position get cast to the matched arrow type */
      switch (self) {
      | Just(Unknown(prov)) =>
        Some(
          DHExp.cast(
            d,
            Unknown(prov),
            Arrow(Unknown(prov), Unknown(prov)),
          ),
        )
      | Just(Arrow(_)) => Some(d)
      | _ => failwith("Elaborator.wrap: SynFun non-arrow-type")
      }
    | Ana(ana_ty) =>
     //TODO(andrew):ADTS this resolution necessary?
      let ana_ty =
        switch (Ctx.resolve_typ(ctx, ana_ty)) {
        | Some(ty) => ty
        | None => ana_ty
        };
      let self_ty =
        switch (self) {
        | Just(ty) =>
          switch (Ctx.resolve_typ(ctx, ty)) {
          | Some(ty) => ty
          | None => ty
          }
        | _ => Unknown(Internal)
        };
      /* Forms with special ana rules get cast from their appropriate Matched types */
      switch (d) {
      | ListLit(_)
      | Cons(_) =>
        switch (ana_ty) {
        | Unknown(prov) =>
          Some(DHExp.cast(d, List(Unknown(prov)), ana_ty))
        | _ => Some(d)
        }
      | Fun(_) =>
        switch (ana_ty) {
        | Unknown(prov) =>
          Some(DHExp.cast(d, Arrow(Unknown(prov), Unknown(prov)), ana_ty))
        | _ => Some(d)
        }
      | Tuple(ds) =>
        switch (ana_ty) {
        | Unknown(prov) =>
          let us = List.init(List.length(ds), _ => Typ.Unknown(prov));
          Some(DHExp.cast(d, Prod(us), ana_ty));
        | _ => Some(d)
        }
      | Inj(_) =>
        switch (ana_ty) {
        | Unknown(prov) =>
          Some(DHExp.cast(d, Sum(Unknown(prov), Unknown(prov)), ana_ty))
        | _ => Some(d)
        }
      | Ap(_, _)
      | Tag(_) =>
       //TODO(andrew):ADTS rec types?
        switch (ana_ty, self_ty) {
        | (Unknown(prov), TSum(tymap)) =>
          let tymap' =
            tymap |> TagMap.map(Option.map(_ => Typ.Unknown(prov)));
          Some(DHExp.cast(d, TSum(tymap'), ana_ty));
        | _ => Some(d)
        }
      /* Forms with special ana rules but no particular typing requirements */
      | ConsistentCase(_)
      | InconsistentBranches(_)
      | Sequence(_)
      | Let(_)
      | FixF(_) => Some(d)
      /* Hole-like forms: Don't cast */
      | InvalidText(_)
      | FreeVar(_)
      | ExpandingKeyword(_)
      | EmptyHole(_)
      | NonEmptyHole(_) => Some(d)
      /* DHExp-specific forms: Don't cast */
      | Cast(_)
      | Closure(_)
      | FailedCast(_)
      | InvalidOperation(_) => Some(d)
      /* Normal cases: wrap */
      | BoundVar(_)
      | ApBuiltin(_)
      | Prj(_)
      //| Ap(_)
      | BoolLit(_)
      | IntLit(_)
      | FloatLit(_)
      | StringLit(_)
      | BinBoolOp(_)
      | BinIntOp(_)
      | BinFloatOp(_)
      | BinStringOp(_)
      | TestLit(_) => Some(DHExp.cast(d, self_ty, ana_ty))
      };
    }
  | InHole(_) => Some(NonEmptyHole(TypeInconsistent, u, 0, d))
  };

let rec dhexp_of_uexp = (m: Statics.map, uexp: Term.UExp.t): option(DHExp.t) => {
  /* NOTE: Left out delta for now */
  switch (Id.Map.find_opt(Term.UExp.rep_id(uexp), m)) {
  | Some(InfoExp({mode, self, ctx, _})) =>
    let err_status = Statics.error_status(ctx, mode, self);
    let id = Term.UExp.rep_id(uexp); /* NOTE: using term uids for hole ids */
    let* d: DHExp.t =
      switch (uexp.term) {
      | Invalid(_) /* NOTE: treating invalid as a hole for now */
      | EmptyHole => Some(DHExp.EmptyHole(id, 0))
      | MultiHole(_tms) =>
        /* TODO: add a dhexp case and eval logic for multiholes.
           Make sure new dhexp form is properly considered Indet
           to avoid casting issues. */
        /*let+ ds =
            tms
            |> List.map(
                 fun
                 | Term.Exp(e) => dhexp_of_uexp(m, e)
                 | tm => Some(EmptyHole(Term.rep_id(tm), 0)),
               )
            |> OptUtil.sequence;
          switch (ds) {
          | [] => DHExp.EmptyHole(id, 0)
          | [hd, ...tl] =>
            // TODO: placeholder logic: sequence
            tl |> List.fold_left((acc, d) => DHExp.Sequence(d, acc), hd)
          };*/
        Some(EmptyHole(id, 0))
      | Triv => Some(Tuple([]))
      | Bool(b) => Some(BoolLit(b))
      | Int(n) => Some(IntLit(n))
      | Float(n) => Some(FloatLit(n))
      | String(s) => Some(StringLit(s))
      | ListLit(es) =>
        let+ ds = es |> List.map(dhexp_of_uexp(m)) |> OptUtil.sequence;
        let ty = Statics.exp_typ(ctx, m, uexp) |> Typ.matched_list;
        //TODO: why is there an err status on below?
        DHExp.ListLit(id, 0, StandardErrStatus(NotInHole), ty, ds);
      | Fun(p, body) =>
        let* dp = dhpat_of_upat(m, p);
        let+ d1 = dhexp_of_uexp(m, body);
        DHExp.Fun(dp, Statics.pat_typ(ctx, m, p), d1, None);
      | Tuple(es) =>
        let+ ds =
          List.fold_right(
            (e, ds_opt) => {
              let* ds = ds_opt;
              let+ d = dhexp_of_uexp(m, e);
              [d, ...ds];
            },
            es,
            Some([]),
          );
        DHExp.Tuple(ds);
      | Tag(name) => Some(Tag(name))
      | Cons(e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.Cons(dc1, dc2);
      | UnOp(Int(Minus), e) =>
        let+ dc = dhexp_of_uexp(m, e);
        DHExp.BinIntOp(Minus, IntLit(0), dc);
      | BinOp(op, e1, e2) =>
        let (_, cons) = exp_binop_of(op);
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        cons(dc1, dc2);
      | Parens(e) => dhexp_of_uexp(m, e)
      | Seq(e1, e2) =>
        let* d1 = dhexp_of_uexp(m, e1);
        let+ d2 = dhexp_of_uexp(m, e2);
        DHExp.Sequence(d1, d2);
      | Test(test) =>
        let+ dtest = dhexp_of_uexp(m, test);
        DHExp.Ap(TestLit(id), dtest);
      | Var(name) =>
        switch (err_status) {
        | InHole(Self(Free)) => Some(FreeVar(id, 0, name))
        | _ => Some(BoundVar(name))
        }
      | Let(p, def, body) =>
        let add_name: (option(string), DHExp.t) => DHExp.t = (
          name =>
            fun
            | Fun(p, ty, e, _) => DHExp.Fun(p, ty, e, name)
            | d => d
        );
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let+ dbody = dhexp_of_uexp(m, body);
        let ty = Statics.pat_self_typ(ctx, m, p);
        switch (Term.UPat.get_recursive_bindings(p)) {
        | None =>
          /* not recursive */
          DHExp.Let(dp, add_name(Term.UPat.get_var(p), ddef), dbody)
        | Some([f]) =>
          /* simple recursion */
          Let(dp, FixF(f, ty, add_name(Some(f), ddef)), dbody)
        | Some(fs) =>
          /* mutual recursion */
          let ddef =
            switch (ddef) {
            | Tuple(a) =>
              DHExp.Tuple(List.map2(s => add_name(Some(s)), fs, a))
            | _ => ddef
            };
          let uniq_id = List.nth(def.ids, 0);
          let self_id = "__mutual__" ++ string_of_int(uniq_id);
          let self_var = DHExp.BoundVar(self_id);
          let (_, substituted_def) =
            fs
            |> List.fold_left(
                 ((i, ddef), f) => {
                   let ddef =
                     Substitution.subst_var(DHExp.Prj(self_var, i), f, ddef);
                   (i + 1, ddef);
                 },
                 (0, ddef),
               );
          Let(dp, FixF(self_id, ty, substituted_def), dbody);
        };
      | Ap(fn, arg) =>
        let* c_fn = dhexp_of_uexp(m, fn);
        let+ c_arg = dhexp_of_uexp(m, arg);
        DHExp.Ap(c_fn, c_arg);
      | If(scrut, e1, e2) =>
        let* d_scrut = dhexp_of_uexp(m, scrut);
        let* d1 = dhexp_of_uexp(m, e1);
        let+ d2 = dhexp_of_uexp(m, e2);
        let d_rules =
          DHExp.[Rule(BoolLit(true), d1), Rule(BoolLit(false), d2)];
        let d = DHExp.Case(d_scrut, d_rules, 0);
        switch (err_status) {
        | InHole(SynInconsistentBranches(_)) =>
          DHExp.InconsistentBranches(id, 0, d)
        | _ => ConsistentCase(d)
        };
      | Match(scrut, rules) =>
        let* d_scrut = dhexp_of_uexp(m, scrut);
        let+ d_rules =
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
          DHExp.InconsistentBranches(id, 0, d)
        | _ => ConsistentCase(d)
        };
      | TyAlias(_, _, e) => dhexp_of_uexp(m, e)
      };
    wrap(ctx, id, mode, self, d);
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_) | InfoTPat(_))
  | None => None
  };
}
and dhpat_of_upat = (m: Statics.map, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(Term.UPat.rep_id(upat), m)) {
  | Some(InfoPat({mode, self, ctx, _})) =>
    let err_status = Statics.error_status(ctx, mode, self);
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
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | String(s) => wrap(StringLit(s))
    | Triv => wrap(Tuple([]))
    | ListLit(ps) =>
      let* ds = ps |> List.map(dhpat_of_upat(m)) |> OptUtil.sequence;
      let ty = Statics.pat_typ(ctx, m, upat) |> Typ.matched_list;
      wrap(ListLit(ty, ds));
    | Tag(name) => wrap(Tag(name))
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap(Cons(d_hd, d_tl));
    | Tuple(ps) =>
      let dps =
        List.fold_right(
          (p, dps_opt) => {
            switch (dps_opt) {
            | None => None
            | Some(dps) =>
              switch (dhpat_of_upat(m, p)) {
              | None => None
              | Some(dp) => Some([dp, ...dps])
              }
            }
          },
          ps,
          Some([]),
        );
      dps |> Option.map(ds => DHPat.Tuple(ds));
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
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | InfoTPat(_) | Invalid(_))
  | None => None
  };
};

let uexp_elab_wrap_builtins = (d: DHExp.t): DHExp.t =>
  List.fold_left(
    (d', (ident, (elab, _))) => DHExp.Let(Var(ident), elab, d'),
    d,
    Builtins.forms(Builtins.Pervasives.builtins),
  );

let uexp_elab = (m: Statics.map, uexp: Term.UExp.t): ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp)) {
  | None => DoesNotElaborate
  | Some(d) =>
    let d = uexp_elab_wrap_builtins(d);
    Elaborates(d, Typ.Unknown(Internal), Delta.empty); //TODO: get type from ci
  };
