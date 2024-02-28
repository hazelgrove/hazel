open Util;
open OptUtil.Syntax;

/*
 Currently, Elaboration does the following things:

  - Insert casts
  - Insert non-empty holes
  - Remove TyAlias
  - Annotate functions with types, and names
  */

module Elaboration = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    d: DHExp.t,
    info_map: Statics.Map.t,
  };
};

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Typ.t, Delta.t)
    | DoesNotElaborate;
};

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
      DHExp.fresh_cast(
        d,
        Unknown(prov),
        Arrow(Unknown(prov), Unknown(prov)),
      )
    | Arrow(_) => d
    | _ => failwith("Elaborator.wrap: SynFun non-arrow-type")
    }
  | Ana(ana_ty) =>
    let ana_ty = Typ.normalize(ctx, ana_ty);
    /* Forms with special ana rules get cast from their appropriate Matched types */
    switch (DHExp.term_of(d)) {
    | ListLit(_)
    | ListConcat(_)
    | Cons(_) =>
      switch (ana_ty) {
      | Unknown(prov) =>
        DHExp.fresh_cast(d, List(Unknown(prov)), Unknown(prov))
      | _ => d
      }
    | Fun(_) =>
      /* See regression tests in Documentation/Dynamics */
      let (_, ana_out) = Typ.matched_arrow(ctx, ana_ty);
      let (self_in, _) = Typ.matched_arrow(ctx, self_ty);
      DHExp.fresh_cast(d, Arrow(self_in, ana_out), ana_ty);
    | Tuple(ds) =>
      switch (ana_ty) {
      | Unknown(prov) =>
        let us = List.init(List.length(ds), _ => Typ.Unknown(prov));
        DHExp.fresh_cast(d, Prod(us), Unknown(prov));
      | _ => d
      }
    | Constructor(_) =>
      switch (ana_ty, self_ty) {
      | (Unknown(prov), Rec(_, Sum(_)))
      | (Unknown(prov), Sum(_)) =>
        DHExp.fresh_cast(d, self_ty, Unknown(prov))
      | _ => d
      }
    | Ap(_, f, _) =>
      switch (DHExp.term_of(f)) {
      | Constructor(_) =>
        switch (ana_ty, self_ty) {
        | (Unknown(prov), Rec(_, Sum(_)))
        | (Unknown(prov), Sum(_)) =>
          DHExp.fresh_cast(d, self_ty, Unknown(prov))
        | _ => d
        }
      | StaticErrorHole(_, g) =>
        switch (DHExp.term_of(g)) {
        | Constructor(_) =>
          switch (ana_ty, self_ty) {
          | (Unknown(prov), Rec(_, Sum(_)))
          | (Unknown(prov), Sum(_)) =>
            DHExp.fresh_cast(d, self_ty, Unknown(prov))
          | _ => d
          }
        | _ => DHExp.fresh_cast(d, self_ty, ana_ty)
        }
      | _ => DHExp.fresh_cast(d, self_ty, ana_ty)
      }
    /* Forms with special ana rules but no particular typing requirements */
    | Match(_)
    | If(_)
    | Seq(_)
    | Let(_)
    | FixF(_) => d
    /* Hole-like forms: Don't cast */
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | StaticErrorHole(_) => d
    /* DHExp-specific forms: Don't cast */
    | Cast(_)
    | Closure(_)
    | Filter(_)
    | FailedCast(_)
    | DynamicErrorHole(_) => d
    /* Normal cases: wrap */
    | Var(_)
    | BuiltinFun(_)
    | Bool(_)
    | Int(_)
    | Float(_)
    | String(_)
    | UnOp(_)
    | BinOp(_)
    | TyAlias(_)
    | Test(_) => DHExp.fresh_cast(d, self_ty, ana_ty)
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
  | InHole(
      FreeVariable(_) | Common(NoType(_)) |
      Common(Inconsistent(Internal(_))),
    ) => d
  | InHole(Common(Inconsistent(Expectation(_) | WithArrow(_)))) =>
    DHExp.fresh(StaticErrorHole(u, d))
  };

let rec dhexp_of_uexp =
        (m: Statics.Map.t, uexp: Term.UExp.t, in_filter: bool)
        : option(DHExp.t) => {
  let dhexp_of_uexp = (~in_filter=in_filter, m, uexp) => {
    dhexp_of_uexp(m, uexp, in_filter);
  };
  switch (Id.Map.find_opt(Term.UExp.rep_id(uexp), m)) {
  | Some(InfoExp({mode, self, ctx, _})) =>
    let err_status = Info.status_exp(ctx, mode, self);
    let id = Term.UExp.rep_id(uexp); /* NOTE: using term uids for hole ids */
    let rewrap = DHExp.mk(uexp.ids);
    let+ d: DHExp.t =
      switch (uexp.term) {
      | Invalid(t) => Some(DHExp.Invalid(t) |> rewrap)
      | EmptyHole => Some(DHExp.EmptyHole |> rewrap)
      | MultiHole(us: list(TermBase.Any.t)) =>
        switch (
          us
          |> List.filter_map(
               fun
               | TermBase.Any.Exp(x) => Some(x)
               | _ => None,
             )
        ) {
        | [] => Some(DHExp.EmptyHole |> rewrap)
        | us =>
          let+ ds = us |> List.map(dhexp_of_uexp(m)) |> OptUtil.sequence;
          DHExp.MultiHole(ds) |> rewrap;
        }

      /* TODO: add a dhexp case and eval logic for multiholes.
         Make sure new dhexp form is properly considered Indet
         to avoid casting issues. */
      | Bool(b) => Some(Bool(b) |> rewrap)
      | Int(n) => Some(Int(n) |> rewrap)
      | Float(n) => Some(Float(n) |> rewrap)
      | String(s) => Some(String(s) |> rewrap)
      | ListLit(es) =>
        let* ds = es |> List.map(dhexp_of_uexp(m)) |> OptUtil.sequence;
        let+ ty = fixed_exp_typ(m, uexp);
        let ty = Typ.matched_list(ctx, ty);
        DHExp.ListLit(ty, ds) |> rewrap;
      | Fun(p, body) =>
        let* dp = dhpat_of_upat(m, p);
        let* d1 = dhexp_of_uexp(m, body);
        let+ ty = fixed_pat_typ(m, p);
        DHExp.Fun(dp, ty, d1, None, None) |> rewrap;
      | Tuple(es) =>
        let+ ds = es |> List.map(dhexp_of_uexp(m)) |> OptUtil.sequence;
        DHExp.Tuple(ds) |> rewrap;
      | Cons(e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.Cons(dc1, dc2) |> rewrap;
      | ListConcat(e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.ListConcat(dc1, dc2) |> rewrap;
      | UnOp(Meta(Unquote), e) =>
        switch (e.term) {
        | Var("e") when in_filter => Some(Constructor("$e") |> DHExp.fresh)
        | Var("v") when in_filter => Some(Constructor("$v") |> DHExp.fresh)
        | _ => Some(DHExp.EmptyHole |> rewrap)
        }
      | UnOp(Int(Minus), e) =>
        let+ dc = dhexp_of_uexp(m, e);
        DHExp.UnOp(Int(Minus), dc) |> rewrap;
      | UnOp(Bool(Not), e) =>
        let+ dc = dhexp_of_uexp(m, e);
        DHExp.UnOp(Bool(Not), dc) |> rewrap;
      | BinOp(op, e1, e2) =>
        let* dc1 = dhexp_of_uexp(m, e1);
        let+ dc2 = dhexp_of_uexp(m, e2);
        DHExp.BinOp(op, dc1, dc2) |> rewrap;
      | Parens(e) => dhexp_of_uexp(m, e)
      | Seq(e1, e2) =>
        let* d1 = dhexp_of_uexp(m, e1);
        let+ d2 = dhexp_of_uexp(m, e2);
        DHExp.Seq(d1, d2) |> rewrap;
      | Test(test) =>
        let+ dtest = dhexp_of_uexp(m, test);
        DHExp.Test(id, dtest) |> rewrap;
      | Filter(act, cond, body) =>
        let* dcond = dhexp_of_uexp(~in_filter=true, m, cond);
        let+ dbody = dhexp_of_uexp(m, body);
        DHExp.Filter(Filter(Filter.mk(dcond, act)), dbody) |> rewrap;
      | Var(name) => Some(Var(name) |> rewrap)
      | Constructor(name) => Some(Constructor(name) |> rewrap)
      | Let(p, def, body) =>
        let add_name: (option(string), DHExp.t) => DHExp.t = (
          (name, d) => {
            let (term, rewrap) = DHExp.unwrap(d);
            switch (term) {
            | Fun(p, ty, e, ctx, _) =>
              DHExp.Fun(p, ty, e, ctx, name) |> rewrap
            | _ => d
            };
          }
        );
        let* dp = dhpat_of_upat(m, p);
        let* ddef = dhexp_of_uexp(m, def);
        let* dbody = dhexp_of_uexp(m, body);
        let+ ty = fixed_pat_typ(m, p);
        switch (Term.UPat.get_recursive_bindings(p)) {
        | None =>
          /* not recursive */
          DHExp.Let(dp, add_name(Term.UPat.get_var(p), ddef), dbody)
          |> rewrap
        | Some(b) =>
          DHExp.Let(
            dp,
            FixF(dp, ty, add_name(Some(String.concat(",", b)), ddef))
            |> DHExp.fresh,
            dbody,
          )
          |> rewrap
        };
      | FixF(p, e) =>
        let* dp = dhpat_of_upat(m, p);
        let* de = dhexp_of_uexp(m, e);
        let+ ty = fixed_pat_typ(m, p);
        DHExp.FixF(dp, ty, de) |> rewrap;
      | Ap(dir, fn, arg) =>
        let* c_fn = dhexp_of_uexp(m, fn);
        let+ c_arg = dhexp_of_uexp(m, arg);
        DHExp.Ap(dir, c_fn, c_arg) |> rewrap;
      | If(c, e1, e2) =>
        let* c' = dhexp_of_uexp(m, c);
        let* d1 = dhexp_of_uexp(m, e1);
        let+ d2 = dhexp_of_uexp(m, e2);
        // Use tag to mark inconsistent branches
        switch (err_status) {
        | InHole(Common(Inconsistent(Internal(_)))) =>
          DHExp.If(c', d1, d2) |> rewrap
        | _ => DHExp.If(c', d1, d2) |> rewrap
        };
      | Match(scrut, rules) =>
        let* d_scrut = dhexp_of_uexp(m, scrut);
        let+ d_rules =
          List.map(
            ((p, e)) => {
              let* d_p = dhpat_of_upat(m, p);
              let+ d_e = dhexp_of_uexp(m, e);
              (d_p, d_e);
            },
            rules,
          )
          |> OptUtil.sequence;
        switch (err_status) {
        | InHole(Common(Inconsistent(Internal(_)))) =>
          DHExp.Match(d_scrut, d_rules) |> rewrap
        | _ => DHExp.Match(d_scrut, d_rules) |> rewrap
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
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, d) |> DHPat.fresh)
      };
    let (pterm, prewrap) = (
      upat.term,
      ((term) => ({ids: upat.ids, copied: false, term}: DHPat.t)),
    );
    switch (pterm) {
    | Invalid(t) => Some(DHPat.Invalid(t) |> prewrap)
    | EmptyHole => Some(EmptyHole |> prewrap)
    | MultiHole(_) =>
      // TODO: dhexp, eval for multiholes
      Some(EmptyHole |> prewrap)
    | Wild => wrap(Wild |> prewrap)
    | Bool(b) => wrap(Bool(b) |> prewrap)
    | Int(n) => wrap(Int(n) |> prewrap)
    | Float(n) => wrap(Float(n) |> prewrap)
    | String(s) => wrap(String(s) |> prewrap)
    | ListLit(ps) =>
      let* ds = ps |> List.map(dhpat_of_upat(m)) |> OptUtil.sequence;
      wrap(ListLit(ds) |> prewrap);
    | Constructor(name) =>
      switch (err_status) {
      | InHole(Common(NoType(FreeConstructor(_)))) =>
        Some(BadConstructor(u, 0, name) |> prewrap)
      | _ => wrap(Constructor(name) |> prewrap)
      }
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap(Cons(d_hd, d_tl) |> prewrap);
    | Tuple(ps) =>
      let* ds = ps |> List.map(dhpat_of_upat(m)) |> OptUtil.sequence;
      wrap(DHPat.Tuple(ds) |> prewrap);
    | Var(name) => Some(Var(name) |> prewrap)
    | Parens(p) => dhpat_of_upat(m, p)
    | Ap(p1, p2) =>
      let* d_p1 = dhpat_of_upat(m, p1);
      let* d_p2 = dhpat_of_upat(m, p2);
      wrap(Ap(d_p1, d_p2) |> prewrap);
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
