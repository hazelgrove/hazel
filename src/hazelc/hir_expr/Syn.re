open Util.ResultSexp;

open Holes;

exception NotImplemented;

[@deriving sexp]
type syn_types = ExprLabel.Map.t(Typ.t);

[@deriving sexp]
type syn_ok = {types: syn_types};

[@deriving sexp]
type syn_error =
  | CaseEmptyRules(ExprLabel.t)
  | UnboundVar(ExprLabel.t)
  | UnboundHole(ExprLabel.t)
  | WrongHoleSort(ExprLabel.t)
  | SigmaUnboundVar(ExprLabel.t, Ident.t)
  | TypesNotEqual(ExprLabel.t, Typ.t, Typ.t)
  | TypesEqual(ExprLabel.t, Typ.t, Typ.t)
  | TypesInconsistent(ExprLabel.t, Typ.t, Typ.t)
  | PatTypesNotEqual(PatLabel.t, Typ.t, Typ.t);

[@deriving sexp]
type syn_result = result(syn_ok, syn_error);

module SynMonad = {
  module State = {
    [@deriving sexp]
    type t = syn_ok;

    let init = {types: ExprLabel.Map.empty};
  };

  module StateMonad = Util.StateMonad.Make(State);
  include Util.Monads.Make_Monad_B({
    type t('a) = StateMonad.t(result('a, syn_error));

    let return = (v: 'a): t('a) => Ok(v) |> StateMonad.return;

    let bind = (m: t('a), f: 'a => t('b)): t('b) =>
      StateMonad.bind(
        m,
        fun
        | Ok(v) => f(v)
        | Error(err) => StateMonad.return(Error(err)),
      );
  });

  let fail = (err, s) => (s, Error(err));
  let get = s => (s, Ok(s));
  let put = (x, _) => (x, Ok());
  let update = f => bind(get, s => put(f(s)));

  let init = State.init;
};

include SynMonad;
include SynMonad.Syntax;
type m('a) = SynMonad.t('a);

let extend = (l, ty) => {
  let+ () =
    update(({types}) => {
      let types = ExprLabel.Map.add(l, ty, types);
      {types: types};
    });
  ty;
};

let rec ana_pat = (ctx, {kind, label: l}: Pat.t, ty: Typ.t): m(TypContext.t) => {
  switch (kind) {
  | PEmptyHole(_u, _i) => ctx |> return
  | PNonEmptyHole(_reason, _u, _i, p') => ana_pat(ctx, p', THole)
  | PKeyword(_u, _i, _k) => ctx |> return
  | PInvalidText(_u, _i, _text) => ctx |> return

  | PAp(_fn, _arg) => raise(NotImplemented)

  | PPair(p1, p2) =>
    switch (ty) {
    | TPair(ty1, ty2) =>
      let* ctx = ana_pat(ctx, p1, ty1);
      ana_pat(ctx, p2, ty2);
    /* FIXME: THole is just a placeholder. */
    | ty => PatTypesNotEqual(l, ty, TPair(THole, THole)) |> fail
    }

  | PCons(p1, p2) =>
    switch (ty) {
    | TList(ty') =>
      let* ctx = ana_pat(ctx, p1, ty);
      ana_pat(ctx, p2, TList(ty'));
    /* FIXME: THole is just a placeholder. */
    | ty => PatTypesNotEqual(l, ty, TList(THole)) |> fail
    }

  | PInj(side, p') =>
    switch (side, ty) {
    | (L, TSum(ty, _))
    | (R, TSum(_, ty)) => ana_pat(ctx, p', ty)
    /* FIXME: THole is just a placeholder. */
    | (L, ty) => PatTypesNotEqual(l, ty, TSum(THole, THole)) |> fail
    | (R, ty) => PatTypesNotEqual(l, ty, TSum(THole, THole)) |> fail
    }

  | PWild => ctx |> return
  | PVar(x) => TypContext.add(x, ty, ctx) |> return

  | PBoolLit(_b) =>
    switch (ty) {
    | TBool => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TBool) |> fail
    }

  | PIntLit(_n) =>
    switch (ty) {
    | TInt => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TInt) |> fail
    }

  | PFloatLit(_f) =>
    switch (ty) {
    | TFloat => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TFloat) |> fail
    }

  | PNil =>
    switch (ty) {
    | TList(_) => ctx |> return
    /* FIXME: THole is just a placeholder. */
    | _ => PatTypesNotEqual(l, ty, TList(THole)) |> fail
    }

  | PTriv =>
    switch (ty) {
    | TUnit => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TUnit) |> fail
    }
  };
};

let rec ana = (ctx, delta, e: Expr.t, ty: Typ.t): m(unit) => {
  let* e_ty = syn(ctx, delta, e);
  if (Typ.equal(e_ty, ty)) {
    () |> return;
  } else {
    TypesNotEqual(e.label, e_ty, ty) |> fail;
  };
}

and syn = (ctx, delta, {kind, label: l}: Expr.t): m(Typ.t) =>
  switch (kind) {
  /* Holes */
  | EEmptyHole(u, _i, sigma) => syn_hole(ctx, delta, l, u, sigma)
  | ENonEmptyHole(_reason, u, _i, sigma, e') =>
    let* _ = syn(ctx, delta, e');
    syn_hole(ctx, delta, l, u, sigma);
  | EKeyword(u, _i, sigma, _k) => syn_hole(ctx, delta, l, u, sigma)
  | EFreeVar(u, _i, sigma, _x) => syn_hole(ctx, delta, l, u, sigma)
  | EInvalidText(u, _i, sigma, _text) => syn_hole(ctx, delta, l, u, sigma)
  | EInvalidOperation(e', _error) => syn(ctx, delta, e')

  | ECast({label: l', _} as e', ty1, ty2) =>
    let* ty' = syn(ctx, delta, e');
    if (Typ.consistent(ty', ty1)) {
      extend(l, ty2);
    } else {
      TypesInconsistent(l', ty', ty1) |> fail;
    };

  | EFailedCast(e', ty1, ty2) =>
    let* ty' = syn(ctx, delta, e');
    if (!Typ.equal(ty', ty1)) {
      extend(l, ty2);
    } else {
      TypesEqual(e'.label, ty', ty1) |> fail;
    };

  | EConsistentCase(case) =>
    let* ty = syn_case(ctx, delta, case, l);
    extend(l, ty);

  /* FIXME: Refer to Statics_DHExp in livelits branch. */
  | EInconsistentBranches(u, _i, sigma, case) =>
    let* ty = syn_case(ctx, delta, case, l);
    Typ.equal(ty, THole)
      ? syn_hole(ctx, delta, l, u, sigma)
      : TypesNotEqual(l, ty, THole) |> fail;

  | ELet(p, e1, e2) =>
    let* ty1 = syn(ctx, delta, e1);
    let* ctx = ana_pat(ctx, p, ty1);
    let* ty2 = syn(ctx, delta, e2);
    extend(l, TArrow(ty1, ty2));

  | ELetRec(_x, _p, _p_ty, _body, _e') => raise(NotImplemented)

  | EFun(p, p_ty, body) =>
    let* ctx = ana_pat(ctx, p, p_ty);
    let* body_ty = syn(ctx, delta, body);
    extend(l, TArrow(p_ty, body_ty));

  /* Application */
  | EAp(fn, arg) =>
    let* fn_ty = syn(ctx, delta, fn);
    let* arg_ty = syn(ctx, delta, arg);
    switch (fn_ty) {
    | TArrow(ty1, ty2) when Typ.equal(arg_ty, ty1) => extend(l, ty2)
    | TArrow(ty1, _ty2) => TypesNotEqual(arg.label, arg_ty, ty1) |> fail
    /* FIXME: THole here is just a placeholder for unknown. */
    | _ => TypesNotEqual(fn.label, fn_ty, TArrow(arg_ty, THole)) |> fail
    };

  | EApBuiltin(name, args) =>
    let* fn_ty =
      switch (TypContext.find_opt(name, ctx)) {
      | Some(fn_ty) => fn_ty |> return
      | None => UnboundVar(l) |> fail
      };
    let* arg_tys =
      args
      |> List.map(arg => syn(ctx, delta, arg) >>| (ty => (arg.label, ty)))
      |> sequence;
    let rec syn_builtin = (fn_ty: Typ.t, arg_tys) =>
      switch (arg_tys, fn_ty) {
      | ([], fn_ty) => fn_ty |> return
      | ([(arg_l, arg_ty), ...arg_tys], TArrow(ty1, ty2)) =>
        Typ.equal(arg_ty, ty1)
          ? syn_builtin(ty2, arg_tys)
          : TypesNotEqual(arg_l, arg_ty, ty1) |> fail
      /* FIXME: THole here is just a placeholder. */
      /* FIXME: Label here is wrong. */
      | (_, fn_ty) => TypesNotEqual(l, fn_ty, TArrow(THole, THole)) |> fail
      };

    let* ty = syn_builtin(fn_ty, arg_tys);
    extend(l, ty);

  /* Binary operations */
  | EBinBoolOp(_op, e1, e2) =>
    let* () = ana(ctx, delta, e1, TBool);
    let* () = ana(ctx, delta, e2, TBool);
    extend(l, TBool);

  | EBinIntOp(_op, e1, e2) =>
    let* () = ana(ctx, delta, e1, TInt);
    let* () = ana(ctx, delta, e2, TInt);
    extend(l, TInt);

  | EBinFloatOp(_op, e1, e2) =>
    let* () = ana(ctx, delta, e1, TFloat);
    let* () = ana(ctx, delta, e2, TFloat);
    extend(l, TBool);

  /* Pair */
  | EPair(e1, e2) =>
    let* ty1 = syn(ctx, delta, e1);
    let* ty2 = syn(ctx, delta, e2);
    extend(l, TPair(ty1, ty2));

  /* Cons */
  | ECons(e1, e2) =>
    let* ty1 = syn(ctx, delta, e1);
    let* ty2 = syn(ctx, delta, e2);
    switch (ty2) {
    | TList(ty2') when Typ.equal(ty1, ty2') => extend(l, TList(ty1))
    | TList(ty2') => TypesNotEqual(e1.label, ty1, ty2') |> fail
    | ty2 => TypesNotEqual(e2.label, ty2, TList(ty1)) |> fail
    };

  /* TSum injection */
  | EInj(other_ty, side, e') =>
    let* this_ty = syn(ctx, delta, e');
    switch (side) {
    | L => extend(l, TSum(this_ty, other_ty))
    | R => extend(l, TSum(other_ty, this_ty))
    };

  | EBoundVar(x) =>
    switch (TypContext.find_opt(x, ctx)) {
    | Some(ty) => extend(l, ty)
    | None => UnboundVar(l) |> fail
    }

  | EBoolLit(_) => extend(l, TBool)
  | EIntLit(_) => extend(l, TInt)
  | EFloatLit(_) => extend(l, TFloat)
  | ENil(ty') => extend(l, TList(ty'))
  | ETriv => extend(l, TUnit)
  }

and syn_case =
    (ctx, delta, {case_kind}: Expr.case, l: ExprLabel.t): m(Typ.t) =>
  switch (case_kind) {
  | ECase(scrut, rules) =>
    let* scrut_ty = syn(ctx, delta, scrut);
    let syn_rule = (ctx, {rule_kind, rule_label: _}: Expr.rule): m(Typ.t) =>
      switch (rule_kind) {
      | ERule(p, body) =>
        let* ctx = ana_pat(ctx, p, scrut_ty);
        syn(ctx, delta, body);
      };

    let* ty =
      switch (rules) {
      | [] => CaseEmptyRules(l) |> fail
      | [rule, ...rules] =>
        rules
        |> List.fold_left(
             (acc, Expr.{rule_kind: ERule(p, body), rule_label: _}) => {
               let* acc_ty = acc;

               let* ctx' = ana_pat(ctx, p, scrut_ty);
               let* body_ty = syn(ctx', delta, body);
               Typ.equal(acc_ty, body_ty)
                 ? acc_ty |> return
                 : TypesNotEqual(body.label, body_ty, acc_ty) |> fail;
             },
             syn_rule(ctx, rule),
           )
      };

    extend(l, ty);
  }

and syn_hole =
    (ctx, delta, l: ExprLabel.t, u: MetaVar.t, sigma: Sigma.t): m(Typ.t) =>
  switch (MetaVarMap.find_opt(u, delta)) {
  | Some((sort, hole_ty, gamma')) =>
    switch (sort) {
    | Delta.ExpressionHole =>
      let* () = ana_hole_sigma(ctx, delta, l, sigma, gamma');
      extend(l, hole_ty);
    | Delta.PatternHole => WrongHoleSort(l) |> fail
    }
  | None => UnboundHole(l) |> fail
  }

and ana_hole_sigma =
    (ctx, delta, l: ExprLabel.t, sigma: Sigma.t, gamma': TypContext.t)
    : m(unit) =>
  gamma'
  |> TypContext.bindings
  |> List.fold_left(
       (acc, (x, gamma_ty)) => {
         let* () = acc;

         switch (Sigma.find_opt(x, sigma)) {
         | None => SigmaUnboundVar(l, x) |> fail
         | Some(e) =>
           let* sigma_ty = syn(ctx, delta, e);
           Typ.equal(sigma_ty, gamma_ty)
             ? () |> return
             : TypesNotEqual(e.label, sigma_ty, gamma_ty) |> fail;
         };
       },
       return(),
     );

let syn = (ctx, delta, e) => {
  let (ok, r) = syn(ctx, delta, e, SynMonad.init);
  r |> Result.map(_ => ok);
};
