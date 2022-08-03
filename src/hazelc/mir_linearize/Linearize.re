open Mir_anf;
open LinearizeMonad;
open LinearizeMonad.Syntax;

[@deriving sexp]
type bind = stmt_kind;

let default_completeness = Complete.IndeterminatelyIncomplete;

let mk_bind_var = (x: Ident.t, c: comp): t((imm, bind)) => {
  let+ imm_label = next_expr_label;
  (
    {
      imm_kind: IVar(x),
      imm_ty: c.comp_ty,
      imm_complete: default_completeness,
      imm_label,
    },
    SLet(x, c),
  );
};

let mk_bind_var_named = (x: Ident.t, c: comp) => {
  let* x' = next_tmp_named(x);
  let+ (var, bind) = mk_bind_var(x', c);
  (x', var, bind);
};

let mk_bind_var_tmp = (c: comp) => {
  let* x = next_tmp;
  let+ (var, bind) = mk_bind_var(x, c);
  (var, bind);
};

let convert_bind = (stmt_kind: bind): t(stmt) => {
  let+ l = next_stmt_label;
  {stmt_kind, stmt_complete: default_completeness, stmt_label: l};
};

module Renamings = {
  include Ident.Map;

  /**
    Context of variable remappings (e.g. x -> t124_x).
   */
  [@deriving sexp]
  type t = Ident.Map.t(Ident.t);
};

let rec linearize_typ = (ty: Hir_expr.typ): typ =>
  switch (ty) {
  | THole => Hole
  | TInt => Int
  | TFloat => Float
  | TBool => Bool
  | TArrow(t1, t2) => Arrow(linearize_typ(t1), linearize_typ(t2))
  | TSum(t1, t2) => Sum(linearize_typ(t1), linearize_typ(t2))
  | TProd(ts) => Prod(ts |> List.map(linearize_typ))
  | TList(t') => List(linearize_typ(t'))
  };

let rec linearize_block = (d: Hir_expr.expr, renamings): t(block) => {
  let* (im, im_binds) = linearize_exp(d, renamings);
  let* body = im_binds |> List.map(convert_bind) |> sequence;

  let* l = next_expr_label;
  {
    block_body: (body, im),
    block_ty: im.imm_ty,
    block_complete: default_completeness,
    block_label: l,
  }
  |> return;
}

and linearize_exp = (d: Hir_expr.expr, renamings): t((imm, list(bind))) => {
  switch (d.kind) {
  | EBoundVar(ty, x) =>
    let ty = linearize_typ(ty);
    let x' =
      switch (Renamings.find_opt(x, renamings)) {
      | Some(x') => x'
      | None => failwith("bad free variable " ++ Ident.to_string(x))
      };

    let* l = next_expr_label;
    (
      {
        imm_kind: IVar(x'),
        imm_ty: ty,
        imm_complete: default_completeness,
        imm_label: l,
      },
      [],
    )
    |> return;

  /* Linearize let with variable pattern into plain let. */
  | ELet({kind: PVar(x), label: _}, d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, renamings);

    let* comp_label = next_expr_label;
    let* (bind_x, _, bind) =
      mk_bind_var_named(
        x,
        {
          comp_kind: CImm(im1),
          comp_complete: default_completeness,
          comp_ty: im1.imm_ty,
          comp_label,
        },
      );

    let renamings' = Renamings.add(x, bind_x, renamings);
    let* (im2, im2_binds) = linearize_exp(d2, renamings');

    let binds = im1_binds @ [bind] @ im2_binds;
    (im2, binds) |> return;

  /* Linearize let with arbitrary pattern into case. */
  | ELet(dp, d1, d2) =>
    let* rule_label = next_hir_rule_label;
    let rules = Hir_expr.Expr.[{rule_kind: ERule(dp, d2), rule_label}];
    linearize_case(
      Hir_expr.Expr.{case_kind: ECase(d1, rules, 1)},
      renamings,
    );

  | ELetRec(x, {kind: PVar(param), label: _}, _p_ty, body, e') =>
    /* Rename bound variable. */
    let* x' = next_tmp_named(x);
    let renamings' = Renamings.add(x, x', renamings);

    /* Create temporary for parameter variable. */
    let* param = next_tmp_named(param);
    /* Linearize body. */
    let* body = linearize_block(body, renamings');
    /* Create binding for function. */
    let fn_bind = SLetRec(x', param, body);

    /* Linearize rest of let. */
    let+ (im', im'_binds) = linearize_exp(e', renamings');
    (im', [fn_bind] @ im'_binds);

  /* Recursive let with arbitrary pattern becomes a recursive let of a function
   * wrapping a case. */
  | ELetRec(x, p, p_ty, body, e') =>
    /* Generate named temporary for bound variable. */
    let* x' = next_tmp_named(x);
    let renamings' = Renamings.add(x, x', renamings);

    /* Create temporary for parameter identifier. */
    let* param = next_tmp;
    /* Wrap body in case. */
    let* body =
      Hir_expr.Expr.(
        {
          let* scrut_label = next_hir_expr_label;
          let scrut = {kind: EBoundVar(p_ty, param), label: scrut_label};

          let* rule_label = next_hir_rule_label;
          let rules = [{rule_kind: ERule(p, body), rule_label}];

          let* label = next_hir_expr_label;
          let case = {case_kind: ECase(scrut, rules, 1)};
          linearize_block(
            Hir_expr.Expr.{kind: EConsistentCase(case), label},
            renamings',
          );
        }
      );

    /* Create binding for function. */
    let fn_bind = SLetRec(x', param, body);

    /* Linearize rest of let. */
    let+ (im', im'_binds) = linearize_exp(e', renamings');
    (im', [fn_bind] @ im'_binds);

  /* Transform function with variable pattern into plain function. */
  | EFun({kind: PVar(x), label: _}, p_ty, body) =>
    let p_ty = linearize_typ(p_ty);
    let* body = linearize_block(body, renamings);

    let* fn_label = next_expr_label;
    let fn = {
      comp_kind: CFun(x, body),
      comp_ty: Arrow(p_ty, body.block_ty),
      comp_complete: default_completeness,
      comp_label: fn_label,
    };

    let+ (fn_var, fn_bind) = mk_bind_var_tmp(fn);
    (fn_var, [fn_bind]);

  /* Transform function with arbitrary pattern into function wrapping a case. */
  | EFun(p, p_ty, body) =>
    let* param = next_tmp;
    let* case =
      Hir_expr.Expr.(
        {
          let* scrut_label = next_hir_expr_label;
          let scrut = {kind: EBoundVar(p_ty, param), label: scrut_label};

          let* rule_label = next_hir_rule_label;
          let rules = [{rule_kind: ERule(p, body), rule_label}];

          let case = {case_kind: ECase(scrut, rules, 1)};

          let+ label = next_hir_expr_label;
          {kind: EConsistentCase(case), label};
        }
      );

    let* param_label = next_hir_pat_label;
    let* fn_label = next_hir_expr_label;
    let fn =
      Hir_expr.Expr.{
        kind: EFun({kind: PVar(param), label: param_label}, p_ty, case),
        label: fn_label,
      };

    linearize_exp(fn, renamings);

  | EAp(fn, arg) =>
    let* (fn, fn_binds) = linearize_exp(fn, renamings);
    let* (arg, arg_binds) = linearize_exp(arg, renamings);

    let* ap_label = next_expr_label;
    let ap_ty =
      switch (fn.imm_ty) {
      | Arrow(_, ty') => ty'
      | _ => failwith("EAp calling non-function type")
      };
    let ap = {
      comp_kind: CAp(fn, arg),
      comp_ty: ap_ty,
      comp_complete: default_completeness,
      comp_label: ap_label,
    };

    let* (ap_var, ap_bind) = mk_bind_var_tmp(ap);
    let binds = fn_binds @ arg_binds @ [ap_bind];
    (ap_var, binds) |> return;

  | EBoolLit(b) =>
    let* l = next_expr_label;
    (
      {
        imm_kind: IConst(ConstBool(b)),
        imm_ty: Bool,
        imm_complete: default_completeness,
        imm_label: l,
      },
      [],
    )
    |> return;

  | EIntLit(i) =>
    let* l = next_expr_label;
    (
      {
        imm_kind: IConst(ConstInt(i)),
        imm_ty: Int,
        imm_complete: default_completeness,
        imm_label: l,
      },
      [],
    )
    |> return;

  | EFloatLit(f) =>
    let* l = next_expr_label;
    (
      {
        imm_kind: IConst(ConstFloat(f)),
        imm_ty: Float,
        imm_complete: default_completeness,
        imm_label: l,
      },
      [],
    )
    |> return;

  | ENil(ty) =>
    let ty = linearize_typ(ty);
    let* l = next_expr_label;
    (
      {
        imm_kind: IConst(ConstNil(ty)),
        imm_ty: List(ty),
        imm_complete: default_completeness,
        imm_label: l,
      },
      [],
    )
    |> return;

  | ETriv =>
    let* l = next_expr_label;
    (
      {
        imm_kind: IConst(ConstTriv),
        imm_ty: Prod([]),
        imm_complete: default_completeness,
        imm_label: l,
      },
      [],
    )
    |> return;

  | EBinBoolOp(op, d1, d2) =>
    let op: bin_op =
      switch (op) {
      | OpAnd => OpAnd
      | OpOr => OpOr
      };
    linearize_bin_op(op, Typ.Bool, d1, d2, renamings);

  | EBinIntOp(op, d1, d2) =>
    let op: bin_op =
      switch (op) {
      | OpPlus => OpPlus
      | OpMinus => OpMinus
      | OpTimes => OpTimes
      | OpDivide => OpDivide
      | OpLessThan => OpLessThan
      | OpGreaterThan => OpGreaterThan
      | OpEquals => OpEquals
      };
    linearize_bin_op(op, Typ.Int, d1, d2, renamings);

  | EBinFloatOp(op, d1, d2) =>
    let op: bin_op =
      switch (op) {
      | OpFPlus => OpFPlus
      | OpFMinus => OpFMinus
      | OpFTimes => OpFTimes
      | OpFDivide => OpFDivide
      | OpFLessThan => OpFLessThan
      | OpFGreaterThan => OpFGreaterThan
      | OpFEquals => OpFEquals
      };
    linearize_bin_op(op, Typ.Float, d1, d2, renamings);

  | EPair(d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, renamings);
    let* (im2, im2_binds) = linearize_exp(d2, renamings);

    let* pair_label = next_expr_label;
    let pair = {
      comp_kind: CPair(im1, im2),
      comp_ty: Prod([im1.imm_ty, im2.imm_ty]),
      comp_complete: default_completeness,
      comp_label: pair_label,
    };

    let* (pair_var, pair_bind) = mk_bind_var_tmp(pair);
    let binds = im1_binds @ im2_binds @ [pair_bind];
    (pair_var, binds) |> return;

  | ECons(d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, renamings);
    let* (im2, im2_binds) = linearize_exp(d2, renamings);

    let* cons_label = next_expr_label;
    let cons = {
      comp_kind: CCons(im1, im2),
      comp_ty: im2.imm_ty,
      comp_complete: default_completeness,
      comp_label: cons_label,
    };

    let* (cons_var, cons_bind) = mk_bind_var_tmp(cons);
    let binds = im1_binds @ im2_binds @ [cons_bind];

    (cons_var, binds) |> return;

  | EInj(other_ty, side, d) =>
    let* (im, im_binds) = linearize_exp(d, renamings);
    let side =
      switch (side) {
      | L => CInjL
      | R => CInjR
      };

    let other_ty = linearize_typ(other_ty);
    let inj_ty: typ =
      switch (side) {
      | CInjL => Sum(im.imm_ty, other_ty)
      | CInjR => Sum(other_ty, im.imm_ty)
      };

    let* inj_label = next_expr_label;
    let inj = {
      comp_kind: CInj(side, im),
      comp_ty: inj_ty,
      comp_complete: default_completeness,
      comp_label: inj_label,
    };

    let* (inj_var, inj_bind) = mk_bind_var_tmp(inj);
    let binds = im_binds @ [inj_bind];
    (inj_var, binds) |> return;

  | EConsistentCase(case) => linearize_case(case, renamings)

  | EEmptyHole(u, i, sigma) =>
    let* (sigma, sigma_binds) = linearize_sigma(sigma, renamings);

    let* hole_label = next_expr_label;
    let hole = {
      comp_kind: CEmptyHole(u, i, sigma),
      comp_ty: Hole,
      comp_complete: default_completeness,
      comp_label: hole_label,
    };

    let* (hole_var, hole_bind) = mk_bind_var_tmp(hole);
    let binds = sigma_binds @ [hole_bind];
    (hole_var, binds) |> return;

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let* (sigma, sigma_binds) = linearize_sigma(sigma, renamings);
    let* (im, im_binds) = linearize_exp(d', renamings);

    let* hole_label = next_expr_label;
    let hole = {
      comp_kind: CNonEmptyHole(reason, u, i, sigma, im),
      comp_ty: Hole,
      comp_complete: default_completeness,
      comp_label: hole_label,
    };

    let* (hole_var, hole_bind) = mk_bind_var_tmp(hole);
    let binds = sigma_binds @ im_binds @ [hole_bind];
    (hole_var, binds) |> return;

  | ECast(d', ty1, ty2) =>
    let* (im, im_binds) = linearize_exp(d', renamings);
    let ty1 = linearize_typ(ty1);
    let ty2 = linearize_typ(ty2);

    let* cast_label = next_expr_label;
    let cast = {
      comp_kind: CCast(im, ty1, ty2),
      comp_ty: ty2,
      comp_complete: default_completeness,
      comp_label: cast_label,
    };

    let* (cast_var, cast_bind) = mk_bind_var_tmp(cast);
    let binds = im_binds @ [cast_bind];
    (cast_var, binds) |> return;

  | EKeyword(_, _, _, _)
  | EFreeVar(_, _, _, _)
  | EInvalidText(_, _, _, _)
  | EInvalidOperation(_, _)
  | EFailedCast(_, _, _)
  | EInconsistentBranches(_, _, _, _)
  | EApBuiltin(_, _) => failwith("not implemented")
  };
}

and linearize_sigma =
    (sigma: Hir_expr.sigma, renamings): t((sigma, list(bind))) => {
  let+ bindings =
    sigma
    |> Hir_expr.Sigma.bindings
    |> List.map(((x, d)) => {
         let+ (im, im_binds) = linearize_exp(d, renamings);
         ((x, im), im_binds);
       })
    |> sequence;

  let binds =
    bindings
    |> List.fold_left((binds, (_, im_binds)) => binds @ im_binds, []);
  let sigma = bindings |> List.map(fst) |> List.to_seq |> Sigma.of_seq;
  (sigma, binds);
}

and linearize_bin_op =
    (op: bin_op, ty: typ, d1: Hir_expr.expr, d2: Hir_expr.expr, renamings) => {
  let* (im1, im1_binds) = linearize_exp(d1, renamings);
  let* (im2, im2_binds) = linearize_exp(d2, renamings);

  let* bin_label = next_expr_label;
  let bin = {
    comp_kind: CBinOp(op, im1, im2),
    comp_ty: ty,
    comp_complete: default_completeness,
    comp_label: bin_label,
  };

  let* (bin_var, bin_bind) = mk_bind_var_tmp(bin);
  let binds = im1_binds @ im2_binds @ [bin_bind];
  (bin_var, binds) |> return;
}

and linearize_case = (case: Hir_expr.case, renamings): t((imm, list(bind))) => {
  switch (case.case_kind) {
  | ECase(scrut, rules, _) =>
    let* (scrut_imm, scrut_binds) = linearize_exp(scrut, renamings);
    /* FIXME: Add wildcard rule that returns entire case expression as final
     * catch-all. */
    let* rules = linearize_rules(rules, renamings);

    let rules_ty = List.hd(rules).rule_branch.block_ty;
    let* case_label = next_expr_label;
    let case = {
      comp_kind: CCase(scrut_imm, rules),
      comp_ty: rules_ty,
      comp_complete: default_completeness,
      comp_label: case_label,
    };

    let* (case_var, case_bind) = mk_bind_var_tmp(case);
    let binds = scrut_binds @ [case_bind];
    (case_var, binds) |> return;
  };
}

and linearize_rules =
    (rules: list(Hir_expr.rule), renamings): t(list(rule)) =>
  rules |> List.map(rule => linearize_rule(rule, renamings)) |> sequence

and linearize_rule = (rule: Hir_expr.rule, renamings): t(rule) => {
  switch (rule.rule_kind) {
  | ERule(p, branch) =>
    let* (p, renamings) = linearize_pat(p, renamings);
    let* branch = linearize_block(branch, renamings);

    let* l = next_rule_label;
    {
      rule_pat: p,
      rule_branch: branch,
      rule_complete: default_completeness,
      rule_label: l,
    }
    |> return;
  };
}

/* Transform Hir.pat into pat.
 *
 * Note that the pat_complete property of pat indicates the indet-ness of the
 * pattern alone, irrespective of the matchee.
 */
and linearize_pat = (p: Hir_expr.pat, renamings): t((pat, Renamings.t)) =>
  linearize_pat_hole(p, renamings)

and linearize_pat_hole = (p: Hir_expr.pat, renamings): t((pat, Renamings.t)) => {
  Pat.(
    switch (p.kind) {
    | PPair(p1, p2) =>
      let* (p1, renamings) = linearize_pat_hole(p1, renamings);
      let* (p2, renamings) = linearize_pat_hole(p2, renamings);

      let* l = next_pat_label;
      (
        {kind: PPair(p1, p2), complete: default_completeness, label: l},
        renamings,
      )
      |> return;

    | PCons(p1, p2) =>
      let* (p1, renamings) = linearize_pat_hole(p1, renamings);
      let* (p2, renamings) = linearize_pat_hole(p2, renamings);

      let* l = next_pat_label;
      (
        {kind: PCons(p1, p2), complete: default_completeness, label: l},
        renamings,
      )
      |> return;

    | PInj(side, p') =>
      let side =
        switch (side) {
        | L => PInjL
        | R => PInjR
        };
      let* (p', renamings) = linearize_pat_hole(p', renamings);

      let* l = next_pat_label;
      (
        {kind: PInj(side, p'), complete: default_completeness, label: l},
        renamings,
      )
      |> return;

    | PWild =>
      let* l = next_pat_label;
      ({kind: PWild, complete: default_completeness, label: l}, renamings)
      |> return;

    | PVar(x) =>
      let* x' = next_tmp_named(x);
      let renamings = Renamings.add(x, x', renamings);

      let* l = next_pat_label;
      (
        {kind: PVar(x'), complete: default_completeness, label: l},
        renamings,
      )
      |> return;

    | PIntLit(i) =>
      let* l = next_pat_label;
      ({kind: PInt(i), complete: default_completeness, label: l}, renamings)
      |> return;

    | PFloatLit(f) =>
      let* l = next_pat_label;
      (
        {kind: PFloat(f), complete: default_completeness, label: l},
        renamings,
      )
      |> return;

    | PBoolLit(b) =>
      let* l = next_pat_label;
      (
        {kind: PBool(b), complete: default_completeness, label: l},
        renamings,
      )
      |> return;

    | PNil =>
      let* l = next_pat_label;
      ({kind: PNil, complete: default_completeness, label: l}, renamings)
      |> return;

    | PTriv =>
      let* l = next_pat_label;
      ({kind: PTriv, complete: default_completeness, label: l}, renamings)
      |> return;

    | PEmptyHole(_)
    | PNonEmptyHole(_)
    | PKeyword(_)
    | PInvalidText(_)
    | PAp(_) => failwith("not implemented")
    }
  );
};

let linearize = (e: Hir_expr.expr): block => {
  let fresh_labels = FreshLabels.fresh_labels(e);
  let state = init(fresh_labels);

  // TODO: Can't pass empty renamings once builtins are supported.
  let (_, block) = linearize_block(e, Renamings.empty, state);
  block;
};
