open Mir_anf;
open LinearizeMonad;
open LinearizeMonad.Syntax;

type bind =
  | BLet(pat, comp)
  | BLetRec(Var.t, comp);

let default_completeness = Complete.IndeterminatelyIncomplete;

let mk_bind = (p: pat, c: comp) => BLet(p, c);

let mk_bind_var = (x: Var.t, c: comp): t((imm, bind)) => {
  let* imm_label = next_label;
  let* pat_label = next_label;
  (
    {
      imm_kind: IVar(x),
      imm_ty: c.comp_ty,
      imm_complete: default_completeness,
      imm_label,
    },
    mk_bind(
      {pat_kind: PVar(x), pat_complete: default_completeness, pat_label},
      c,
    ),
  )
  |> return;
};

let mk_bind_var_tmp = (c: comp) => {
  let* x = next_tmp;
  let* (var, bind) = mk_bind_var(x, c);
  (var, bind) |> return;
};

let convert_bind = (bn: bind): t(stmt) => {
  let* l = next_label;
  (
    switch (bn) {
    | BLet(p, c) => {
        stmt_kind: SLet(p, c),
        stmt_complete: default_completeness,
        stmt_label: l,
      }
    | BLetRec(p, c) => {
        stmt_kind: SLetRec(p, c),
        stmt_complete: default_completeness,
        stmt_label: l,
      }
    }
  )
  |> return;
};

/** Context of variable remappings (e.g. x -> t124_x). */
type var_remapping = VarMap.t_(Var.t);

let rec linearize_block = (d: Hir_expr.expr, vctx): t(block) => {
  let* (im, im_binds) = linearize_exp(d, vctx);
  let* body = im_binds |> List.map(convert_bind) |> sequence;

  let* l = next_label;
  {
    block_body: (body, im),
    block_ty: im.imm_ty,
    block_complete: default_completeness,
    block_label: l,
  }
  |> return;
}

and linearize_exp = (d: Hir_expr.expr, vctx): t((imm, list(bind))) => {
  switch (d.kind) {
  | EBoundVar(ty, x) =>
    let x' =
      switch (VarMap.lookup(vctx, x)) {
      | Some(x') => x'
      | None => failwith("bad free variable " ++ x)
      };

    let* l = next_label;
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

  | ELet(dp, d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, vctx);
    let* (p, vctx') = linearize_pat(dp, vctx);
    let* (im2, im2_binds) = linearize_exp(d2, vctx');

    let* c1_label = next_label;
    let c1 = {
      comp_kind: CImm(im1),
      comp_ty: im1.imm_ty,
      comp_complete: default_completeness,
      comp_label: c1_label,
    };
    let binds = im1_binds @ [mk_bind(p, c1)] @ im2_binds;
    (im2, binds) |> return;

  | ELetRec(x, dp, dp_ty, body, d') =>
    let* x' = next_tmp;
    let vctx = VarMap.extend(vctx, (x, x'));

    let* (p, vctx) = linearize_pat(dp, vctx);
    let* body = linearize_block(body, vctx);

    let* (im, im_binds) = linearize_exp(d', vctx);

    let* fn_label = next_label;
    let fn = {
      comp_kind: CFun(p, body),
      comp_ty: Arrow(dp_ty, body.block_ty),
      comp_complete: default_completeness,
      comp_label: fn_label,
    };

    let binds = [BLetRec(x', fn), ...im_binds];
    (im, binds) |> return;

  | EFun(dp, dp_ty, body) =>
    let* (p, vctx) = linearize_pat(dp, vctx);
    let* body = linearize_block(body, vctx);

    let* fn_label = next_label;
    let fn = {
      comp_kind: CFun(p, body),
      comp_ty: Arrow(dp_ty, body.block_ty),
      comp_complete: default_completeness,
      comp_label: fn_label,
    };

    let* (fn_var, fn_bind) = mk_bind_var_tmp(fn);
    (fn_var, [fn_bind]) |> return;

  | EAp(fn, arg) =>
    let* (fn, fn_binds) = linearize_exp(fn, vctx);
    let* (arg, arg_binds) = linearize_exp(arg, vctx);

    let* ap_label = next_label;
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
    let* l = next_label;
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
    let* l = next_label;
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
    let* l = next_label;
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
    let* l = next_label;
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
    let* l = next_label;
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
    linearize_bin_op(op, HTyp.Bool, d1, d2, vctx);

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
    linearize_bin_op(op, HTyp.Int, d1, d2, vctx);

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
    linearize_bin_op(op, HTyp.Float, d1, d2, vctx);

  | EPair(d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, vctx);
    let* (im2, im2_binds) = linearize_exp(d2, vctx);

    let* pair_label = next_label;
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
    let* (im1, im1_binds) = linearize_exp(d1, vctx);
    let* (im2, im2_binds) = linearize_exp(d2, vctx);

    let* cons_label = next_label;
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
    let* (im, im_binds) = linearize_exp(d, vctx);
    let side = linearize_inj_side(side);

    let inj_ty: HTyp.t =
      switch (side) {
      | CInjL => Sum(im.imm_ty, other_ty)
      | CInjR => Sum(other_ty, im.imm_ty)
      };

    let* inj_label = next_label;
    let inj = {
      comp_kind: CInj(side, im),
      comp_ty: inj_ty,
      comp_complete: default_completeness,
      comp_label: inj_label,
    };

    let* (inj_var, inj_bind) = mk_bind_var_tmp(inj);
    let binds = im_binds @ [inj_bind];
    (inj_var, binds) |> return;

  | EConsistentCase(case) => linearize_case(case, vctx)

  | EEmptyHole(u, i, sigma) =>
    let* (sigma, sigma_binds) = linearize_sigma(sigma, vctx);

    let* hole_label = next_label;
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
    let* (sigma, sigma_binds) = linearize_sigma(sigma, vctx);
    let* (im, im_binds) = linearize_exp(d', vctx);

    let* hole_label = next_label;
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
    let* (im, im_binds) = linearize_exp(d', vctx);

    let* cast_label = next_label;
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
    (sigma: VarMap.t_(Hir_expr.expr), vctx)
    : t((VarMap.t_(imm), list(bind))) =>
  switch (sigma) {
  | [] => ([], []) |> return
  | [(x, d), ...sigma] =>
    let* (im, im_binds) = linearize_exp(d, vctx);
    let* (sigma, sigma_binds) = linearize_sigma(sigma, vctx);
    let sigma = VarMap.extend(sigma, (x, im));
    (sigma, im_binds @ sigma_binds) |> return;
  }

and linearize_bin_op =
    (op: bin_op, ty: HTyp.t, d1: Hir_expr.expr, d2: Hir_expr.expr, vctx) => {
  let* (im1, im1_binds) = linearize_exp(d1, vctx);
  let* (im2, im2_binds) = linearize_exp(d2, vctx);

  let* bin_label = next_label;
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

and linearize_case = (case: Hir_expr.case, vctx): t((imm, list(bind))) => {
  switch (case.case_kind) {
  | ECase(scrut, rules, _) =>
    let* (scrut_imm, scrut_binds) = linearize_exp(scrut, vctx);
    let* rules = linearize_rules(rules, vctx);

    let rules_ty = List.hd(rules).rule_branch.block_ty;
    let* case_label = next_label;
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

and linearize_rules = (rules: list(Hir_expr.rule), vctx): t(list(rule)) =>
  rules |> List.map(rule => linearize_rule(rule, vctx)) |> sequence

and linearize_rule = (rule: Hir_expr.rule, vctx): t(rule) => {
  switch (rule.rule_kind) {
  | ERule(p, branch) =>
    let* (p, vctx) = linearize_pat(p, vctx);
    let* branch = linearize_block(branch, vctx);

    let* l = next_label;
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
and linearize_pat = (p: Hir_expr.pat, vctx): t((pat, var_remapping)) =>
  linearize_pat_hole(p, vctx)

and linearize_pat_hole = (p: Hir_expr.pat, vctx): t((pat, var_remapping)) => {
  switch (p.kind) {
  | PPair(p1, p2) =>
    let* (p1, vctx) = linearize_pat_hole(p1, vctx);
    let* (p2, vctx) = linearize_pat_hole(p2, vctx);

    let* l = next_label;
    (
      {
        pat_kind: PPair(p1, p2),
        pat_complete: default_completeness,
        pat_label: l,
      },
      vctx,
    )
    |> return;

  | PCons(p1, p2) =>
    let* (p1, vctx) = linearize_pat_hole(p1, vctx);
    let* (p2, vctx) = linearize_pat_hole(p2, vctx);

    let* l = next_label;
    (
      {
        pat_kind: PCons(p1, p2),
        pat_complete: default_completeness,
        pat_label: l,
      },
      vctx,
    )
    |> return;

  | PInj(side, p') =>
    let side = linearize_inj_side(side);
    let* (p', vctx) = linearize_pat_hole(p', vctx);

    let* l = next_label;
    (
      {
        pat_kind: PInj(side, p'),
        pat_complete: default_completeness,
        pat_label: l,
      },
      vctx,
    )
    |> return;

  | PWild =>
    let* l = next_label;
    (
      {pat_kind: PWild, pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PVar(x) =>
    // Mark x as indet whenever the matchee is indet or the pattern is inside a
    // pattern hole.
    let* x' = next_tmp_named(x);
    let vctx = VarMap.extend(vctx, (x, x'));

    let* l = next_label;
    (
      {pat_kind: PVar(x'), pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PIntLit(i) =>
    let* l = next_label;
    (
      {pat_kind: PInt(i), pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PFloatLit(f) =>
    let* l = next_label;
    (
      {pat_kind: PFloat(f), pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PBoolLit(b) =>
    let* l = next_label;
    (
      {pat_kind: PBool(b), pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PNil =>
    let* l = next_label;
    (
      {pat_kind: PNil, pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PTriv =>
    let* l = next_label;
    (
      {pat_kind: PTriv, pat_complete: default_completeness, pat_label: l},
      vctx,
    )
    |> return;

  | PEmptyHole(_)
  | PNonEmptyHole(_)
  | PKeyword(_)
  | PInvalidText(_)
  | PAp(_) => failwith("not implemented")
  };
}

and linearize_inj_side = (side: InjSide.t): inj_side => {
  switch (side) {
  | L => CInjL
  | R => CInjR
  };
};

let linearize = (d: Hir_expr.expr): block => {
  // TODO: Can't pass empty vctx once builtins are supported.
  let (_, block) = linearize_block(d, VarMap.empty, init);
  block;
};
