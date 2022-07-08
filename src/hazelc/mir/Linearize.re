open LinearizeMonad;
open LinearizeMonad.Syntax;

type bind =
  | BLet(Anf.pat, Anf.comp)
  | BLetRec(Var.t, Anf.comp);

let default_completeness = Completeness.IndeterminatelyIncomplete;

let mk_bind = (p: Anf.pat, c: Anf.comp) => BLet(p, c);

let mk_bind_var = (x: Var.t, c: Anf.comp) => (
  Anf.Imm.mk_var(x, c),
  mk_bind({pat_kind: PVar(x), pat_complete: default_completeness}, c),
);

let mk_bind_var_tmp = (c: Anf.comp) => {
  let* x = next_tmp;
  let (var, bind) = mk_bind_var(x, c);
  (var, bind) |> return;
};

let convert_bind = (bn: bind): Anf.stmt => {
  switch (bn) {
  | BLet(p, c) => {
      stmt_kind: SLet(p, c),
      stmt_complete: default_completeness,
    }
  | BLetRec(p, c) => {
      stmt_kind: SLetRec(p, c),
      stmt_complete: default_completeness,
    }
  };
};

/** Context of variable remappings (e.g. x -> t124_x). */
type var_remapping = VarMap.t_(Var.t);

let rec linearize_prog = (d: Hir.expr, vctx): t(Anf.prog) => {
  let* (im, im_binds) = linearize_exp(d, vctx);
  let body = im_binds |> List.map(convert_bind);

  Anf.Prog.mk(body, im) |> return;
}

and linearize_exp = (d: Hir.expr, vctx): t((Anf.imm, list(bind))) => {
  switch (d.expr_kind) {
  | EBoundVar(ty, x) =>
    let x' =
      switch (VarMap.lookup(vctx, x)) {
      | Some(x') => x'
      | None => failwith("bad free variable " ++ x)
      };

    (
      Anf.{
        imm_kind: IVar(x'),
        imm_ty: ty,
        imm_complete: default_completeness,
      },
      [],
    )
    |> return;

  | ELet(dp, d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, vctx);
    let* (p, vctx') = linearize_pat(dp, vctx);
    let* (im2, im2_binds) = linearize_exp(d2, vctx');

    let binds = im1_binds @ [mk_bind(p, Anf.Comp.mk_imm(im1))] @ im2_binds;
    (im2, binds) |> return;

  | ELetRec(x, dp, dp_ty, body, d') =>
    let* x' = next_tmp;
    let vctx = VarMap.extend(vctx, (x, x'));

    let* (p, vctx) = linearize_pat(dp, vctx);
    let* body = linearize_prog(body, vctx);
    let fn =
      Anf.{
        comp_kind: CFun(p, body),
        comp_ty: Arrow(dp_ty, body.prog_ty),
        comp_complete: default_completeness,
      };

    let* (im, im_binds) = linearize_exp(d', vctx);

    let binds = [BLetRec(x', fn), ...im_binds];
    (im, binds) |> return;

  | EFun(dp, dp_ty, body) =>
    let* (p, vctx) = linearize_pat(dp, vctx);
    let* body = linearize_prog(body, vctx);

    let fn =
      Anf.{
        comp_kind: CFun(p, body),
        comp_ty: Arrow(dp_ty, body.prog_ty),
        comp_complete: default_completeness,
      };

    let* (fn_var, fn_bind) = mk_bind_var_tmp(fn);
    (fn_var, [fn_bind]) |> return;

  | EAp(fn, arg) =>
    let* (fn, fn_binds) = linearize_exp(fn, vctx);
    let* (arg, arg_binds) = linearize_exp(arg, vctx);

    let ap_ty =
      switch (fn.imm_ty) {
      | Arrow(_, ty') => ty'
      | _ => failwith("EAp calling non-function type")
      };
    let ap =
      Anf.{
        comp_kind: CAp(fn, arg),
        comp_ty: ap_ty,
        comp_complete: default_completeness,
      };

    let* (ap_var, ap_bind) = mk_bind_var_tmp(ap);
    let binds = fn_binds @ arg_binds @ [ap_bind];
    (ap_var, binds) |> return;

  | EBoolLit(b) =>
    (
      Anf.{
        imm_kind: IConst(ConstBool(b)),
        imm_ty: Bool,
        imm_complete: default_completeness,
      },
      [],
    )
    |> return

  | EIntLit(i) =>
    (
      Anf.{
        imm_kind: IConst(ConstInt(i)),
        imm_ty: Int,
        imm_complete: default_completeness,
      },
      [],
    )
    |> return

  | EFloatLit(f) =>
    (
      Anf.{
        imm_kind: IConst(ConstFloat(f)),
        imm_ty: Float,
        imm_complete: default_completeness,
      },
      [],
    )
    |> return

  | ENil(ty) =>
    (
      Anf.{
        imm_kind: IConst(ConstNil(ty)),
        imm_ty: List(ty),
        imm_complete: default_completeness,
      },
      [],
    )
    |> return

  | ETriv =>
    (
      Anf.{
        imm_kind: IConst(ConstTriv),
        imm_ty: Prod([]),
        imm_complete: default_completeness,
      },
      [],
    )
    |> return

  | EBinBoolOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | OpAnd => OpAnd
      | OpOr => OpOr
      };
    linearize_bin_op(op, HTyp.Bool, d1, d2, vctx);

  | EBinIntOp(op, d1, d2) =>
    let op: Anf.bin_op =
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
    let op: Anf.bin_op =
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
    let pair =
      Anf.{
        comp_kind: CPair(im1, im2),
        comp_ty: Prod([im1.imm_ty, im2.imm_ty]),
        comp_complete: default_completeness,
      };

    let* (pair_var, pair_bind) = mk_bind_var_tmp(pair);
    let binds = im1_binds @ im2_binds @ [pair_bind];
    (pair_var, binds) |> return;

  | ECons(d1, d2) =>
    let* (im1, im1_binds) = linearize_exp(d1, vctx);
    let* (im2, im2_binds) = linearize_exp(d2, vctx);
    let cons =
      Anf.{
        comp_kind: CCons(im1, im2),
        comp_ty: im2.imm_ty,
        comp_complete: default_completeness,
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

    let inj =
      Anf.{
        comp_kind: CInj(side, im),
        comp_ty: inj_ty,
        comp_complete: default_completeness,
      };

    let* (inj_var, inj_bind) = mk_bind_var_tmp(inj);
    let binds = im_binds @ [inj_bind];
    (inj_var, binds) |> return;

  | EConsistentCase(case) => linearize_case(case, vctx)

  | EEmptyHole(u, i, sigma) =>
    let* (sigma, sigma_binds) = linearize_sigma(sigma, vctx);
    let hole =
      Anf.{
        comp_kind: CEmptyHole(u, i, sigma),
        comp_ty: Hole,
        comp_complete: default_completeness,
      };

    let* (hole_var, hole_bind) = mk_bind_var_tmp(hole);
    let binds = sigma_binds @ [hole_bind];
    (hole_var, binds) |> return;

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let* (sigma, sigma_binds) = linearize_sigma(sigma, vctx);
    let* (im, im_binds) = linearize_exp(d', vctx);
    let hole =
      Anf.{
        comp_kind: CNonEmptyHole(reason, u, i, sigma, im),
        comp_ty: Hole,
        comp_complete: default_completeness,
      };

    let* (hole_var, hole_bind) = mk_bind_var_tmp(hole);
    let binds = sigma_binds @ im_binds @ [hole_bind];
    (hole_var, binds) |> return;

  | ECast(d', ty1, ty2) =>
    let* (im, im_binds) = linearize_exp(d', vctx);
    let cast =
      Anf.{
        comp_kind: CCast(im, ty1, ty2),
        comp_ty: ty2,
        comp_complete: default_completeness,
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
    (sigma: VarMap.t_(Hir.expr), vctx)
    : t((VarMap.t_(Anf.imm), list(bind))) =>
  switch (sigma) {
  | [] => ([], []) |> return
  | [(x, d), ...sigma] =>
    let* (im, im_binds) = linearize_exp(d, vctx);
    let* (sigma, sigma_binds) = linearize_sigma(sigma, vctx);
    let sigma = VarMap.extend(sigma, (x, im));
    (sigma, im_binds @ sigma_binds) |> return;
  }

and linearize_bin_op =
    (op: Anf.bin_op, ty: HTyp.t, d1: Hir.expr, d2: Hir.expr, vctx) => {
  let* (im1, im1_binds) = linearize_exp(d1, vctx);
  let* (im2, im2_binds) = linearize_exp(d2, vctx);
  let bin =
    Anf.{
      comp_kind: CBinOp(op, im1, im2),
      comp_ty: ty,
      comp_complete: default_completeness,
    };

  let* (bin_var, bin_bind) = mk_bind_var_tmp(bin);
  let binds = im1_binds @ im2_binds @ [bin_bind];
  (bin_var, binds) |> return;
}

and linearize_case = (case: Hir.case, vctx): t((Anf.imm, list(bind))) => {
  switch (case.case_kind) {
  | ECase(scrut, rules, _) =>
    let* (scrut_imm, scrut_binds) = linearize_exp(scrut, vctx);
    let* rules = linearize_rules(rules, vctx);

    let rules_ty = List.hd(rules).rule_branch.prog_ty;
    let case =
      Anf.{
        comp_kind: CCase(scrut_imm, rules),
        comp_ty: rules_ty,
        comp_complete: default_completeness,
      };

    let* (case_var, case_bind) = mk_bind_var_tmp(case);
    let binds = scrut_binds @ [case_bind];
    (case_var, binds) |> return;
  };
}

and linearize_rules = (rules: list(Hir.rule), vctx): t(list(Anf.rule)) => {
  switch (rules) {
  | [] => [] |> return
  | [rule, ...rules] =>
    let* rule = linearize_rule(rule, vctx);
    let* rules = linearize_rules(rules, vctx);
    [rule, ...rules] |> return;
  };
}

and linearize_rule = (rule: Hir.rule, vctx): t(Anf.rule) => {
  switch (rule.rule_kind) {
  | ERule(p, branch) =>
    let* (p, vctx) = linearize_pat(p, vctx);
    let* branch = linearize_prog(branch, vctx);

    Anf.{
      rule_pat: p,
      rule_branch: branch,
      rule_complete: default_completeness,
    }
    |> return;
  };
}

/* Transform Hir.pat into Anf.pat.
 *
 * Note that the pat_complete property of Anf.pat indicates the indet-ness of the
 * pattern alone, irrespective of the matchee.
 */
and linearize_pat = (p: Hir.pat, vctx): t((Anf.pat, var_remapping)) =>
  linearize_pat_hole(p, vctx)

and linearize_pat_hole = (p: Hir.pat, vctx): t((Anf.pat, var_remapping)) => {
  switch (p.pat_kind) {
  | PPair(p1, p2) =>
    let* (p1, vctx) = linearize_pat_hole(p1, vctx);
    let* (p2, vctx) = linearize_pat_hole(p2, vctx);
    (
      Anf.{pat_kind: PPair(p1, p2), pat_complete: default_completeness},
      vctx,
    )
    |> return;

  | PCons(p1, p2) =>
    let* (p1, vctx) = linearize_pat_hole(p1, vctx);
    let* (p2, vctx) = linearize_pat_hole(p2, vctx);
    (
      Anf.{pat_kind: PCons(p1, p2), pat_complete: default_completeness},
      vctx,
    )
    |> return;

  | PInj(side, p') =>
    let side = linearize_inj_side(side);
    let* (p', vctx) = linearize_pat_hole(p', vctx);
    (
      Anf.{pat_kind: PInj(side, p'), pat_complete: default_completeness},
      vctx,
    )
    |> return;

  | PWild =>
    (Anf.{pat_kind: PWild, pat_complete: default_completeness}, vctx)
    |> return

  | PVar(x) =>
    // Mark x as indet whenever the matchee is indet or the pattern is inside a
    // pattern hole.
    let* x' = next_tmp_named(x);
    let vctx = VarMap.extend(vctx, (x, x'));
    (Anf.{pat_kind: PVar(x'), pat_complete: default_completeness}, vctx)
    |> return;

  | PIntLit(i) =>
    (Anf.{pat_kind: PInt(i), pat_complete: default_completeness}, vctx)
    |> return

  | PFloatLit(f) =>
    (Anf.{pat_kind: PFloat(f), pat_complete: default_completeness}, vctx)
    |> return

  | PBoolLit(b) =>
    (Anf.{pat_kind: PBool(b), pat_complete: default_completeness}, vctx)
    |> return

  | PNil =>
    (Anf.{pat_kind: PNil, pat_complete: default_completeness}, vctx)
    |> return

  | PTriv =>
    (Anf.{pat_kind: PTriv, pat_complete: default_completeness}, vctx)
    |> return

  | PEmptyHole(_)
  | PNonEmptyHole(_)
  | PKeyword(_)
  | PInvalidText(_)
  | PAp(_) => failwith("not implemented")
  };
}

and linearize_inj_side = (side: InjSide.t): Anf.inj_side => {
  switch (side) {
  | L => CInjL
  | R => CInjR
  };
};

let linearize = (d: Hir.expr): Anf.prog => {
  // TODO: Can't pass empty vctx once builtins are supported.
  let (_, prog) = linearize_prog(d, VarMap.empty, init);
  prog;
};
