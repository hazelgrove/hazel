open Util.ResultSexp;

open Holes;

[@deriving sexp]
type syn_types = ExprLabel.Map.t(Typ.t);
[@deriving sexp]
type syn_idents = Ident.Set.t;
[@deriving sexp]
type syn_labels = {
  expr: ExprLabel.Set.t,
  stmt: StmtLabel.Set.t,
  rule: RuleLabel.Set.t,
  pat: PatLabel.Set.t,
};

[@deriving sexp]
type syn_ok = {
  types: syn_types,
  idents: syn_idents,
  labels: syn_labels,
};

[@deriving sexp]
type syn_error =
  | DuplicateIdent(Ident.t)
  | DuplicateExprLabel(ExprLabel.t)
  | DuplicateStmtLabel(StmtLabel.t)
  | DuplicateRuleLabel(RuleLabel.t)
  | DuplicatePatLabel(PatLabel.t)
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

    let init = {
      types: ExprLabel.Map.empty,
      idents: Ident.Set.empty,
      labels: {
        expr: ExprLabel.Set.empty,
        stmt: StmtLabel.Set.empty,
        rule: RuleLabel.Set.empty,
        pat: PatLabel.Set.empty,
      },
    };
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

type uniq('l) =
  | U('l);

let record_typ = (U(l), ty) => {
  let+ () =
    update(({types, _} as s) => {
      let types = ExprLabel.Map.add(l, ty, types);
      {...s, types};
    });

  ty;
};

let record_ident = x => {
  /* Ensure that identifier. is unique. */
  let* () =
    get
    >>= (
      ({idents, _}) =>
        Ident.Set.mem(x, idents) ? DuplicateIdent(x) |> fail : () |> return
    );

  /* Add identifier. */
  let+ () =
    update(({idents, _} as s) => {
      let idents = Ident.Set.add(x, idents);
      {...s, idents};
    });

  x;
};

let record_expr_label = l => {
  let* () =
    get
    >>= (
      ({labels: {expr, _}, _}) =>
        ExprLabel.Set.mem(l, expr)
          ? DuplicateExprLabel(l) |> fail : () |> return
    );

  let+ () =
    update(({labels: {expr, _} as labels, _} as s) => {
      let expr = ExprLabel.Set.add(l, expr);
      {
        ...s,
        labels: {
          ...labels,
          expr,
        },
      };
    });

  U(l);
};

let record_stmt_label = l => {
  let* () =
    get
    >>= (
      ({labels: {stmt, _}, _}) =>
        StmtLabel.Set.mem(l, stmt)
          ? DuplicateStmtLabel(l) |> fail : () |> return
    );

  let+ () =
    update(({labels: {stmt, _} as labels, _} as s) => {
      let stmt = StmtLabel.Set.add(l, stmt);
      {
        ...s,
        labels: {
          ...labels,
          stmt,
        },
      };
    });

  U(l);
};

let record_rule_label = l => {
  let* () =
    get
    >>= (
      ({labels: {rule, _}, _}) =>
        RuleLabel.Set.mem(l, rule)
          ? DuplicateRuleLabel(l) |> fail : () |> return
    );

  let+ () =
    update(({labels: {rule, _} as labels, _} as s) => {
      let rule = RuleLabel.Set.add(l, rule);
      {
        ...s,
        labels: {
          ...labels,
          rule,
        },
      };
    });

  U(l);
};

let record_pat_label = l => {
  let* () =
    get
    >>= (
      ({labels: {pat, _}, _}) =>
        PatLabel.Set.mem(l, pat)
          ? DuplicatePatLabel(l) |> fail : () |> return
    );

  let+ () =
    update(({labels: {pat, _} as labels, _} as s) => {
      let pat = PatLabel.Set.add(l, pat);
      {
        ...s,
        labels: {
          ...labels,
          pat,
        },
      };
    });

  U(l);
};

let rec ana_pat =
        (ctx, {kind, label: l, complete: _}: Pat.t, ty: Typ.t)
        : m(TypContext.t) => {
  let* U(l) = record_pat_label(l);

  switch (kind) {
  | PPair(p1, p2) =>
    switch (ty) {
    | TPair(ty1, ty2) =>
      let* ctx = ana_pat(ctx, p1, ty1);
      ana_pat(ctx, p2, ty2);
    /* FIXME: Hole is just a placeholder. */
    | ty => PatTypesNotEqual(l, ty, TPair(THole, THole)) |> fail
    }

  | PCons(p1, p2) =>
    switch (ty) {
    | TList(ty') =>
      let* ctx = ana_pat(ctx, p1, ty);
      ana_pat(ctx, p2, TList(ty'));
    /* FIXME: Hole is just a placeholder. */
    | ty => PatTypesNotEqual(l, ty, TList(THole)) |> fail
    }

  | PInj(side, p') =>
    switch (side, ty) {
    | (PInjL, TSum(ty, _))
    | (PInjR, TSum(_, ty)) => ana_pat(ctx, p', ty)
    /* FIXME: Hole is just a placeholder. */
    | (PInjL, ty) => PatTypesNotEqual(l, ty, TSum(THole, THole)) |> fail
    | (PInjR, ty) => PatTypesNotEqual(l, ty, TSum(THole, THole)) |> fail
    }

  | PWild => ctx |> return
  | PVar(x) =>
    let* _ = record_ident(x);
    TypContext.add(x, ty, ctx) |> return;

  | PBool(_b) =>
    switch (ty) {
    | TBool => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TBool) |> fail
    }

  | PInt(_n) =>
    switch (ty) {
    | TInt => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TInt) |> fail
    }

  | PFloat(_f) =>
    switch (ty) {
    | TFloat => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TFloat) |> fail
    }

  | PNil =>
    switch (ty) {
    | TList(_) => ctx |> return
    /* FIXME: Hole is just a placeholder. */
    | _ => PatTypesNotEqual(l, ty, TList(THole)) |> fail
    }

  | PTriv =>
    switch (ty) {
    | TUnit => ctx |> return
    | _ => PatTypesNotEqual(l, ty, TUnit) |> fail
    }
  };
};

let rec ana' = (label, actual_ty, expected_ty) =>
  if (Typ.equal(actual_ty, expected_ty)) {
    () |> return;
  } else {
    TypesNotEqual(label, actual_ty, expected_ty) |> fail;
  }

and ana_block = (ctx, delta, block: Anf.block, ty): m(unit) => {
  let* block_ty = syn_block(ctx, delta, block);
  ana'(block.block_label, block_ty, ty);
}

and ana_imm = (ctx, delta, im: Anf.imm, ty): m(unit) => {
  let* im_ty = syn_imm(ctx, delta, im);
  ana'(im.imm_label, im_ty, ty);
}

and syn_block =
    (ctx, delta, {block_body: (stmts, im), block_label: l, _}: Anf.block) => {
  let* l = record_expr_label(l);
  let* ctx = syn_stmts(ctx, delta, stmts);
  let* ty = syn_imm(ctx, delta, im);
  record_typ(l, ty);
}

and syn_stmts = (ctx: TypContext.t, delta, stmts) =>
  switch (stmts) {
  | [] => ctx |> return
  | [stmt, ...stmts] =>
    stmts
    |> List.fold_left(
         (acc, stmt) => {
           let* ctx = acc;
           syn_stmt(ctx, delta, stmt);
         },
         syn_stmt(ctx, delta, stmt),
       )
  }

and syn_stmt = (ctx, delta, {stmt_kind, stmt_label: l, _}: Anf.stmt) => {
  let* _ = record_stmt_label(l);
  switch (stmt_kind) {
  | SLet(x, c) =>
    let* _ = record_ident(x);

    let+ ty = syn_comp(ctx, delta, c);
    let ctx = TypContext.add(x, ty, ctx);
    ctx;

  | SLetRec(x, param, param_ty, o_ty, body) =>
    let* _ = record_ident(x);
    let* _ = record_ident(param);

    let ty = Typ.TArrow(param_ty, o_ty);
    let ctx = TypContext.add(x, ty, ctx);

    let ctx' = TypContext.add(param, param_ty, ctx);
    let+ () = ana_block(ctx', delta, body, o_ty);

    ctx;
  };
}

and syn_comp = (ctx, delta, {comp_kind, comp_label: l, _}: Anf.comp) => {
  let* U(l') as l = record_expr_label(l);

  switch (comp_kind) {
  /* Holes */
  | CEmptyHole(u, _i, sigma) => syn_hole(ctx, delta, l, u, sigma)
  | CNonEmptyHole(_reason, u, _i, sigma, im) =>
    let* _ = syn_imm(ctx, delta, im);
    syn_hole(ctx, delta, l, u, sigma);

  | CCast({imm_label: l', _} as im, ty1, ty2) =>
    let* ty' = syn_imm(ctx, delta, im);
    if (Typ.equal(ty', ty1)) {
      if (Typ.consistent(ty1, ty2)) {
        record_typ(l, ty2);
      } else {
        TypesInconsistent(l', ty1, ty2) |> fail;
      };
    } else {
      TypesNotEqual(l', ty', ty1) |> fail;
    };

  | CCase(scrut, rules) =>
    let* scrut_ty = syn_imm(ctx, delta, scrut);
    let syn_rule =
        (ctx, delta, {rule_pat, rule_branch, rule_label: l, _}: Anf.rule)
        : m(Typ.t) => {
      let* _ = record_rule_label(l);
      let* ctx = ana_pat(ctx, rule_pat, scrut_ty);
      syn_block(ctx, delta, rule_branch);
    };

    let* ty =
      switch (rules) {
      | [] => CaseEmptyRules(l') |> fail
      | [rule, ...rules] =>
        rules
        |> List.fold_left(
             (acc, rule: Anf.rule) => {
               let* acc_ty = acc;

               let* branch_ty = syn_rule(ctx, delta, rule);
               Typ.equal(acc_ty, branch_ty)
                 ? acc_ty |> return
                 : TypesNotEqual(
                     rule.rule_branch.block_label,
                     branch_ty,
                     acc_ty,
                   )
                   |> fail;
             },
             syn_rule(ctx, delta, rule),
           )
      };

    record_typ(l, ty);

  | CFun(param, param_ty, body) =>
    let ctx' = TypContext.add(param, param_ty, ctx);
    let* body_ty = syn_block(ctx', delta, body);
    record_typ(l, TArrow(param_ty, body_ty));

  /* Application */
  | CAp(fn, arg) =>
    let* fn_ty = syn_imm(ctx, delta, fn);
    let* arg_ty = syn_imm(ctx, delta, arg);
    switch (fn_ty) {
    | TArrow(ty1, ty2) when Typ.equal(arg_ty, ty1) => record_typ(l, ty2)
    | TArrow(ty1, _ty2) => TypesNotEqual(arg.imm_label, arg_ty, ty1) |> fail
    /* FIXME: Hole here is just a placeholder for unknown. */
    | _ => TypesNotEqual(fn.imm_label, fn_ty, TArrow(arg_ty, THole)) |> fail
    };

  /* Binary operations */
  | CBinOp(op, im1, im2) =>
    let ana_ty: Typ.t =
      switch (op) {
      | OpAnd
      | OpOr => TBool
      | OpPlus
      | OpMinus
      | OpTimes
      | OpDivide
      | OpLessThan
      | OpGreaterThan
      | OpEquals => TInt
      | OpFPlus
      | OpFMinus
      | OpFTimes
      | OpFDivide
      | OpFLessThan
      | OpFGreaterThan
      | OpFEquals => TFloat
      };
    let* () = ana_imm(ctx, delta, im1, ana_ty);
    let* () = ana_imm(ctx, delta, im2, ana_ty);
    record_typ(l, ana_ty);

  /* Pair */
  | CPair(im1, im2) =>
    let* ty1 = syn_imm(ctx, delta, im1);
    let* ty2 = syn_imm(ctx, delta, im2);
    record_typ(l, TPair(ty1, ty2));

  /* Cons */
  | CCons(im1, im2) =>
    let* ty1 = syn_imm(ctx, delta, im1);
    let* ty2 = syn_imm(ctx, delta, im2);
    switch (ty2) {
    | TList(ty2') when Typ.equal(ty1, ty2') => record_typ(l, TList(ty1))
    | TList(ty2') => TypesNotEqual(im1.imm_label, ty1, ty2') |> fail
    | ty2 => TypesNotEqual(im2.imm_label, ty2, TList(ty1)) |> fail
    };

  /* Sum injection */
  | CInj(other_ty, side, im') =>
    let* this_ty = syn_imm(ctx, delta, im');
    let ty: Typ.t =
      switch (side) {
      | CInjL => TSum(this_ty, other_ty)
      | CInjR => TSum(other_ty, this_ty)
      };
    record_typ(l, ty);

  | CImm(im) =>
    let* ty = syn_imm(ctx, delta, im);
    record_typ(l, ty);
  };
}

and syn_imm = (ctx, _delta, {imm_kind, imm_label: l, _}: Anf.imm) => {
  let* U(l') as l = record_expr_label(l);

  switch (imm_kind) {
  | IVar(x) =>
    switch (TypContext.find_opt(x, ctx)) {
    | Some(ty) => record_typ(l, ty)
    | None => UnboundVar(l') |> fail
    }

  | IConst(const) =>
    let ty: Typ.t =
      switch (const) {
      | ConstBool(_) => TBool
      | ConstInt(_) => TInt
      | ConstFloat(_) => TFloat
      | ConstNil(ty') => TList(ty')
      | ConstTriv => TUnit
      };
    record_typ(l, ty);
  };
}

and syn_hole =
    (ctx, delta, U(l') as l: uniq(ExprLabel.t), u: MetaVar.t, sigma: Sigma.t)
    : m(Typ.t) =>
  switch (MetaVarMap.find_opt(u, delta)) {
  | Some((sort, hole_ty, gamma')) =>
    switch (sort) {
    | Delta.ExpressionHole =>
      let* () = ana_hole_sigma(ctx, delta, l, sigma, gamma');
      record_typ(l, hole_ty);
    | Delta.PatternHole => WrongHoleSort(l') |> fail
    }
  | None => UnboundHole(l') |> fail
  }

and ana_hole_sigma =
    (
      ctx,
      delta,
      U(l'): uniq(ExprLabel.t),
      sigma: Sigma.t,
      gamma': TypContext.t,
    )
    : m(unit) =>
  gamma'
  |> TypContext.bindings
  |> List.fold_left(
       (acc, (x, gamma_ty)) => {
         let* () = acc;

         switch (Sigma.find_opt(x, sigma)) {
         | None => SigmaUnboundVar(l', x) |> fail
         | Some(im) =>
           let* sigma_ty = syn_imm(ctx, delta, im);
           Typ.equal(sigma_ty, gamma_ty)
             ? () |> return
             : TypesNotEqual(im.imm_label, sigma_ty, gamma_ty) |> fail;
         };
       },
       return(),
     );

let syn = (ctx, delta, e) => {
  let (ok, r) = syn_block(ctx, delta, e, SynMonad.init);
  r |> Result.map(_ => ok);
};
