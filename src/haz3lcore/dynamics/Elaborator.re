open Util;
open OptUtil.Syntax;

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

let exp_binop_of: Term.UExp.op_bin => (Typ.t, (_, _) => DHExp.t) =
  fun
  | Int(op) => (Int, ((e1, e2) => BinIntOp(op, e1, e2)))
  | Float(op) => (Float, ((e1, e2) => BinFloatOp(op, e1, e2)))
  | Bool(op) => (Bool, ((e1, e2) => BinBoolOp(op, e1, e2)))
  | String(op) => (String, ((e1, e2) => BinStringOp(op, e1, e2)))
  | Prop(op) => (Prop, ((e1, e2) => BinPropOp(op, e1, e2)));

let fixed_exp_typ = (m: Statics.Map.t, e: Term.UExp.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Term.UExp.rep_id(e), m)) {
  | Some(InfoExp({ty, _})) => Some(ty)
  | _ => None
  };

let fixed_pat_typ = (m: Statics.Map.t, p: Term.UPat.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Term.UPat.rep_id(p), m)) {
  | Some(InfoPat({ty, _})) => Some(ty)
  | _ => None
  };

let cast = (ctx: Ctx.t, mode: Mode.t, self_ty: Typ.t, d: DHExp.t) =>
  switch (mode) {
  | Syn => d
  | SynFun =>
    switch (self_ty) {
    | Unknown(prov) =>
      DHExp.cast(d, Unknown(prov), Arrow(Unknown(prov), Unknown(prov)))
    | Arrow(_) => d
    | _ => failwith("Elaborator.wrap: SynFun non-arrow-type")
    }
  | SynTypFun =>
    switch (self_ty) {
    | Unknown(prov) =>
      /* ? |> forall _. ? */
      DHExp.cast(d, Unknown(prov), Forall("_", Unknown(prov)))
    | Forall(_) => d
    | _ => failwith("Elaborator.wrap: SynTypFun non-forall-type")
    }
  | Ana(ana_ty) =>
    let ana_ty = Typ.normalize(ctx, ana_ty);
    /* Forms with special ana rules get cast from their appropriate Matched types */
    switch (d) {
    | ListLit(_)
    | ListConcat(_)
    | Cons(_) =>
      switch (ana_ty) {
      | Unknown(prov) => DHExp.cast(d, List(Unknown(prov)), Unknown(prov))
      | _ => d
      }
    | Fun(_) =>
      /* See regression tests in Documentation/Dynamics */
      let (_, ana_out) = Typ.matched_arrow(ctx, ana_ty);
      let (self_in, _) = Typ.matched_arrow(ctx, self_ty);
      DHExp.cast(d, Arrow(self_in, ana_out), ana_ty);
    | TypFun(_) =>
      switch (ana_ty) {
      | Unknown(prov) =>
        DHExp.cast(d, Forall("grounded_forall", Unknown(prov)), ana_ty)
      | _ => d
      }
    | Tuple(ds) =>
      switch (ana_ty) {
      | Unknown(prov) =>
        let us = List.init(List.length(ds), _ => Typ.Unknown(prov));
        DHExp.cast(d, Prod(us), Unknown(prov));
      | _ => d
      }
    | Ap(NonEmptyHole(_, _, _, Constructor(_)), _)
    | Ap(Constructor(_), _)
    | TypAp(Constructor(_), _)
    | Constructor(_) =>
      switch (ana_ty, self_ty) {
      | (Unknown(prov), Rec(_, Sum(_)))
      | (Unknown(prov), Sum(_)) => DHExp.cast(d, self_ty, Unknown(prov))
      | _ => d
      }
    /* Forms with special ana rules but no particular typing requirements */
    | ConsistentCase(_)
    | InconsistentBranches(_)
    | IfThenElse(_)
    | Sequence(_)
    | Let(_)
    | FixF(_) => d
    /* Hole-like forms: Don't cast */
    | InvalidText(_)
    | FreeVar(_)
    | EmptyHole(_)
    | NonEmptyHole(_) => d
    /* DHExp-specific forms: Don't cast */
    | Cast(_)
    | Closure(_)
    | Filter(_)
    | FailedCast(_)
    | InvalidOperation(_)
    | InvalidDerivation(_) => d
    /* Normal cases: wrap */
    | BoundVar(_)
    | Ap(_)
    | Derive(_)
    | ApBuiltin(_)
    | BuiltinFun(_)
    | Prj(_)
    | BoolLit(_)
    | IntLit(_)
    | FloatLit(_)
    | StringLit(_)
    | PropLit(_)
    | JudgementLit(_)
    | BinBoolOp(_)
    | BinIntOp(_)
    | BinFloatOp(_)
    | BinStringOp(_)
    | BinPropOp(_)
    | Entail(_)
    | Test(_) => DHExp.cast(d, self_ty, ana_ty)
    | TypAp(_) =>
      // TODO: check with andrew
      DHExp.cast(d, self_ty, ana_ty)
    };
  };

/* Handles cast insertion and non-empty-hole wrapping
   for elaborated expressions */
let wrap = (ctx: Ctx.t, u: Id.t, mode: Mode.t, self, d: DHExp.t): DHExp.t =>
  switch (Info.status_exp(ctx, mode, self)) {
  | NotInHole(_) =>
    let self_ty =
      switch (Self.typ_of_exp(ctx, self)) {
      | Some(self_ty) => Typ.normalize(ctx, self_ty)
      | None => Unknown(Internal)
      };
    cast(ctx, mode, self_ty, d);
  | InHole(_) => NonEmptyHole(TypeInconsistent, u, 0, d)
  };

let rec dhexp_of_uexp =
        (m: Statics.Map.t, uexp: Term.UExp.t, in_filter: bool)
        : option(DHExp.t) => {
  let dhexp_of_uexp = (~in_filter=in_filter, m, uexp) => {
    dhexp_of_uexp(m, uexp, in_filter);
  };
  switch (Id.Map.find_opt(Term.UExp.rep_id(uexp), m)) {
  | Some(InfoExp({mode, self, ctx, ancestors, co_ctx, _})) =>
    let err_status = Info.status_exp(ctx, mode, self);
    let id = Term.UExp.rep_id(uexp); /* NOTE: using term uids for hole ids */
    let+ d: DHExp.t =
      switch (uexp.term) {
      | Invalid(t) => Some(DHExp.InvalidText(id, 0, t))
      | EmptyHole => Some(DHExp.EmptyHole(id, 0))
      | MultiHole(_tms) =>
        /* TODO: add a dhexp case and eval logic for multiholes.
           Make sure new dhexp form is properly considered Indet
           to avoid casting issues. */
        Some(EmptyHole(id, 0))
      | Triv => Some(Tuple([]))
      | Deferral(_) => Some(DHExp.InvalidText(id, 0, "_"))
      | Bool(b) => Some(BoolLit(b))
      | Int(n) => Some(IntLit(n))
      | Float(n) => Some(FloatLit(n))
      | String(s) => Some(StringLit(s))
      | Prop(p) => Some(PropLit(p))
      | Judgement(j) => Some(JudgementLit(j))
      | ListLit(es) =>
        let* ds = es |> List.map(dhexp_of_uexp(m)) |> OptUtil.sequence;
        let+ ty = fixed_exp_typ(m, uexp);
        let ty = Typ.matched_list(ctx, ty);
        DHExp.ListLit(id, 0, ty, ds);
      | Fun(p, body) =>
        let* dp = dhpat_of_upat(m, p);
        let* d1 = dhexp_of_uexp(m, body);
        let+ ty = fixed_pat_typ(m, p);
        DHExp.Fun(dp, ty, d1, None);
      | TypFun(tpat, body) =>
        let+ d1 = dhexp_of_uexp(m, body);
        DHExp.TypFun(tpat, d1, None);
      | Tuple(es) =>
        let+ ds = es |> List.map(dhexp_of_uexp(m)) |> OptUtil.sequence;
        DHExp.Tuple(ds);
      | Cons(e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.Cons(dc1, dc2);
      | ListConcat(e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.ListConcat(dc1, dc2);
      | Entail(e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.Entail(dc1, dc2);
      | UnOp(Meta(Unquote), e) =>
        switch (e.term) {
        | Var("e") when in_filter => Some(Constructor("$e"))
        | Var("v") when in_filter => Some(Constructor("$v"))
        | _ => Some(DHExp.EmptyHole(id, 0))
        }
      | UnOp(Int(Minus), e) =>
        let+ dc = dhexp_of_uexp(m, e);
        DHExp.BinIntOp(Minus, IntLit(0), dc);
      | UnOp(Bool(Not), e) =>
        let+ d_scrut = dhexp_of_uexp(m, e);
        let d_rules =
          DHExp.[
            Rule(BoolLit(true), BoolLit(false)),
            Rule(BoolLit(false), BoolLit(true)),
          ];
        let d = DHExp.ConsistentCase(DHExp.Case(d_scrut, d_rules, 0));
        /* Manually construct cast (case is not otherwise cast) */
        switch (mode) {
        | Ana(ana_ty) => DHExp.cast(d, Bool, ana_ty)
        | _ => d
        };
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
        DHExp.Test(id, dtest);
      | Filter(act, cond, body) =>
        let* dcond = dhexp_of_uexp(~in_filter=true, m, cond);
        let+ dbody = dhexp_of_uexp(m, body);
        DHExp.Filter(Filter(Filter.mk(dcond, act)), dbody);
      | Var(name) =>
        switch (err_status) {
        | InHole(FreeVariable(_)) => Some(FreeVar(id, 0, name))
        | _ => Some(BoundVar(name))
        }
      | Constructor(name) =>
        switch (err_status) {
        | InHole(Common(NoType(FreeConstructor(_)))) =>
          Some(FreeVar(id, 0, name))
        | _ => Some(Constructor(name))
        }
      | Let(p, def, body) =>
        let add_name: (option(string), DHExp.t) => DHExp.t = (
          name =>
            fun
            | Fun(p, ty, e, _) => DHExp.Fun(p, ty, e, name)
            | TypFun(tpat, e, _) => DHExp.TypFun(tpat, e, name)
            | d => d
        );
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let* dbody = dhexp_of_uexp(m, body);
        let+ ty = fixed_pat_typ(m, p);
        let is_recursive =
          Statics.is_recursive(ctx, p, def, ty)
          && Term.UPat.get_bindings(p)
          |> Option.get
          |> List.exists(f => VarMap.lookup(co_ctx, f) != None);
        if (!is_recursive) {
          /* not recursive */
          DHExp.Let(
            dp,
            add_name(Term.UPat.get_var(p), ddef),
            dbody,
          );
        } else {
          switch (Term.UPat.get_bindings(p) |> Option.get) {
          | [f] =>
            /* simple recursion */
            Let(dp, FixF(f, ty, add_name(Some(f ++ "+"), ddef)), dbody)
          | fs =>
            /* mutual recursion */
            let ddef =
              switch (ddef) {
              | Tuple(a) =>
                DHExp.Tuple(
                  List.map2(s => add_name(Some(s ++ "+")), fs, a),
                )
              | _ => ddef
              };
            let uniq_id = List.nth(def.ids, 0);
            let self_id = "__mutual__" ++ Id.to_string(uniq_id);
            let self_var = DHExp.BoundVar(self_id);
            let (_, substituted_def) =
              fs
              |> List.fold_left(
                   ((i, ddef), f) => {
                     let ddef =
                       Substitution.subst_var(
                         DHExp.Prj(self_var, i),
                         f,
                         ddef,
                       );
                     (i + 1, ddef);
                   },
                   (0, ddef),
                 );
            Let(dp, FixF(self_id, ty, substituted_def), dbody);
          };
        };
      | Ap(fn, arg)
      | Pipeline(arg, fn) =>
        let* c_fn = dhexp_of_uexp(m, fn);
        let+ c_arg = dhexp_of_uexp(m, arg);
        DHExp.Ap(c_fn, c_arg);
      | Derive(prems, concl, rule) =>
        let* dprems = dhexp_of_uexp(m, prems);
        let* dconcl = dhexp_of_uexp(m, concl);
        let+ drule = dhexp_of_uexp(m, rule);
        DHExp.Derive(dprems, dconcl, drule);
      | TypAp(fn, uty_arg) =>
        let+ d_fn = dhexp_of_uexp(m, fn);
        DHExp.TypAp(d_fn, Term.UTyp.to_typ(ctx, uty_arg));
      | DeferredAp(fn, args) =>
        switch (err_status) {
        | InHole(BadPartialAp(NoDeferredArgs)) => dhexp_of_uexp(m, fn)
        | InHole(BadPartialAp(ArityMismatch(_))) =>
          Some(DHExp.InvalidText(id, 0, "<invalid partial ap>"))
        | _ =>
          let mk_tuple = (ctor, xs) =>
            List.length(xs) == 1 ? List.hd(xs) : ctor(xs);
          let* ty_fn = fixed_exp_typ(m, fn);
          let (ty_arg, ty_ret) = Typ.matched_arrow(ctx, ty_fn);
          let ty_ins = Typ.matched_args(ctx, List.length(args), ty_arg);
          /* Substitute all deferrals for new variables */
          let (pats, ty_args, ap_args, ap_ctx) =
            List.combine(args, ty_ins)
            |> List.fold_left(
                 ((pats, ty_args, ap_args, ap_ctx), (e: Term.UExp.t, ty)) =>
                   if (Term.UExp.is_deferral(e)) {
                     // Internal variable name for deferrals
                     let name =
                       "__deferred__" ++ string_of_int(List.length(pats));
                     let var: Term.UExp.t = {ids: e.ids, term: Var(name)};
                     let var_entry =
                       Ctx.VarEntry({
                         name,
                         id: Term.UExp.rep_id(e),
                         typ: ty,
                       });
                     (
                       pats @ [DHPat.Var(name)],
                       ty_args @ [ty],
                       ap_args @ [var],
                       Ctx.extend(ap_ctx, var_entry),
                     );
                   } else {
                     (pats, ty_args, ap_args @ [e], ap_ctx);
                   },
                 ([], [], [], ctx),
               );
          let (pat, ty_arg) = (
            mk_tuple(x => DHPat.Tuple(x), pats),
            mk_tuple(x => Typ.Prod(x), ty_args),
          );
          let arg: Term.UExp.t = {ids: [Id.mk()], term: Tuple(ap_args)};
          let body: Term.UExp.t = {ids: [Id.mk()], term: Ap(fn, arg)};
          let (_info, m) =
            Statics.uexp_to_info_map(
              ~ctx=ap_ctx,
              ~mode=Ana(ty_ret),
              ~ancestors,
              body,
              m,
            );
          let+ dbody = dhexp_of_uexp(m, body);
          DHExp.Fun(pat, Arrow(ty_arg, ty_ret), dbody, None);
        }
      | If(c, e1, e2) =>
        let* c' = dhexp_of_uexp(m, c);
        let* d1 = dhexp_of_uexp(m, e1);
        let+ d2 = dhexp_of_uexp(m, e2);
        // Use tag to mark inconsistent branches
        switch (err_status) {
        | InHole(Common(Inconsistent(Internal(_)))) =>
          DHExp.IfThenElse(DH.InconsistentIf, c', d1, d2)
        | _ => DHExp.IfThenElse(DH.ConsistentIf, c', d1, d2)
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
        | InHole(Common(Inconsistent(Internal(_))))
        | InHole(InexhaustiveMatch(Some(Inconsistent(Internal(_))))) =>
          DHExp.InconsistentBranches(id, 0, d)
        | _ => ConsistentCase(d)
        };
      | TyAlias(_, _, e) => dhexp_of_uexp(m, e)
      };
    switch (uexp.term) {
    | Parens(_) => d
    | _ => wrap(ctx, id, mode, self, d)
    };
  | Some(InfoPat(_) | InfoTyp(_) | InfoTPat(_) | Secondary(_))
  | None => None
  };
}
and dhpat_of_upat = (m: Statics.Map.t, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(Term.UPat.rep_id(upat), m)) {
  | Some(InfoPat({mode, self, ctx, _})) =>
    // NOTE: for the current implementation, redundancy is considered a static error
    // but not a runtime error.
    let self =
      switch (self) {
      | Redundant(self) => self
      | _ => self
      };
    let err_status = Info.status_pat(ctx, mode, self);
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
    | Invalid(t) => Some(DHPat.InvalidText(u, 0, t))
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
      let* ty = fixed_pat_typ(m, upat);
      wrap(ListLit(Typ.matched_list(ctx, ty), ds));
    | Constructor(name) =>
      switch (err_status) {
      | InHole(Common(NoType(FreeConstructor(_)))) =>
        Some(BadConstructor(u, 0, name))
      | _ => wrap(Constructor(name))
      }
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap(Cons(d_hd, d_tl));
    | Tuple(ps) =>
      let* ds = ps |> List.map(dhpat_of_upat(m)) |> OptUtil.sequence;
      wrap(DHPat.Tuple(ds));
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
  | Some(InfoExp(_) | InfoTyp(_) | InfoTPat(_) | Secondary(_))
  | None => None
  };
};

//let dhexp_of_uexp = Core.Memo.general(~cache_size_bound=1000, dhexp_of_uexp);

let uexp_elab = (m: Statics.Map.t, uexp: Term.UExp.t): ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp, false)) {
  | None => DoesNotElaborate
  | Some(d) =>
    //let d = uexp_elab_wrap_builtins(d);
    let ty =
      switch (fixed_exp_typ(m, uexp)) {
      | Some(ty) => ty
      | None => Typ.Unknown(Internal)
      };
    Elaborates(d, ty, Delta.empty);
  };
