exception NotImplemented;
exception WrongType;
exception FreeBoundVar(Var.t);

let default_completeness = Completeness.IndeterminatelyIncomplete;

type bind =
  | BLet(Anf.pat, Anf.comp)
  | BLetRec(Var.t, Anf.comp);

let mk_bind = (p: Anf.pat, c: Anf.comp) => BLet(p, c);

let mk_bind_var = (x: Var.t, c: Anf.comp) => (
  Anf.Imm.mk_var(x, c),
  mk_bind({pat_kind: PVar(x), pat_complete: default_completeness}, c),
);

let mk_bind_var_tmp = (c: Anf.comp, t_gen: TmpVarGen.t) => {
  let (x, t_gen) = TmpVarGen.next(t_gen);
  let (var, bind) = mk_bind_var(x, c);
  (var, bind, t_gen);
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

let rec linearize_prog = (d: Hir.expr, vctx, t_gen): (Anf.prog, TmpVarGen.t) => {
  let (im, im_binds, t_gen) = linearize_exp(d, vctx, t_gen);
  let body = im_binds |> List.map(convert_bind);

  let c = Anf.Comp.mk_imm(im);
  (Anf.Prog.mk(body, c), t_gen);
}

and linearize_exp =
    (d: Hir.expr, vctx, t_gen): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d.expr_kind) {
  | EBoundVar(ty, x) =>
    let x' =
      switch (VarMap.lookup(vctx, x)) {
      | Some(x') => x'
      | None => raise(FreeBoundVar(x))
      };

    (
      {imm_kind: IVar(x'), imm_ty: ty, imm_complete: default_completeness},
      [],
      t_gen,
    );

  | ELet(dp, d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, vctx, t_gen);
    let (p, vctx', t_gen) = linearize_pat(dp, vctx, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, vctx', t_gen);

    let binds = im1_binds @ [mk_bind(p, Anf.Comp.mk_imm(im1))] @ im2_binds;
    (im2, binds, t_gen);

  | ELetRec(x, dp, dp_ty, body, d') =>
    let (x', t_gen) = TmpVarGen.next_named(x, t_gen);
    let vctx = VarMap.extend(vctx, (x, x'));

    let (p, vctx, t_gen) = linearize_pat(dp, vctx, t_gen);
    let (body, t_gen) = linearize_prog(body, vctx, t_gen);
    let lam: Anf.comp = {
      comp_kind: CLam(p, body),
      comp_ty: Arrow(dp_ty, body.prog_ty),
      comp_complete: default_completeness,
    };

    let (im, im_binds, t_gen) = linearize_exp(d', vctx, t_gen);

    let binds = [BLetRec(x', lam), ...im_binds];
    (im, binds, t_gen);

  | ELam(dp, dp_ty, body) =>
    let (p, vctx, t_gen) = linearize_pat(dp, vctx, t_gen);
    let (body, t_gen) = linearize_prog(body, vctx, t_gen);

    let lam: Anf.comp = {
      comp_kind: CLam(p, body),
      comp_ty: Arrow(dp_ty, body.prog_ty),
      comp_complete: default_completeness,
    };

    let (lam_var, lam_bind, t_gen) = mk_bind_var_tmp(lam, t_gen);
    (lam_var, [lam_bind], t_gen);

  | EAp(fn, arg) =>
    let (fn, fn_binds, t_gen) = linearize_exp(fn, vctx, t_gen);
    let (arg, arg_binds, t_gen) = linearize_exp(arg, vctx, t_gen);

    let ap_ty =
      switch (fn.imm_ty) {
      | Arrow(_, ty') => ty'
      | _ => raise(WrongType)
      };
    let ap: Anf.comp = {
      comp_kind: CAp(fn, [arg]),
      comp_ty: ap_ty,
      comp_complete: default_completeness,
    };

    let (ap_var, ap_bind, t_gen) = mk_bind_var_tmp(ap, t_gen);
    let binds = fn_binds @ arg_binds @ [ap_bind];
    (ap_var, binds, t_gen);

  | EBoolLit(b) => (
      {
        imm_kind: IConst(ConstBool(b)),
        imm_ty: Bool,
        imm_complete: default_completeness,
      },
      [],
      t_gen,
    )

  | EIntLit(i) => (
      {
        imm_kind: IConst(ConstInt(i)),
        imm_ty: Int,
        imm_complete: default_completeness,
      },
      [],
      t_gen,
    )

  | EFloatLit(f) => (
      {
        imm_kind: IConst(ConstFloat(f)),
        imm_ty: Float,
        imm_complete: default_completeness,
      },
      [],
      t_gen,
    )

  | ENil(ty) => (
      {
        imm_kind: IConst(ConstNil(ty)),
        imm_ty: List(ty),
        imm_complete: default_completeness,
      },
      [],
      t_gen,
    )

  | ETriv => (
      {
        imm_kind: IConst(ConstTriv),
        imm_ty: Prod([]),
        imm_complete: default_completeness,
      },
      [],
      t_gen,
    )

  | EBinBoolOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | OpAnd => OpAnd
      | OpOr => OpOr
      };
    linearize_bin_op(op, HTyp.Bool, d1, d2, vctx, t_gen);

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
    linearize_bin_op(op, HTyp.Int, d1, d2, vctx, t_gen);

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
    linearize_bin_op(op, HTyp.Float, d1, d2, vctx, t_gen);

  | EPair(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, vctx, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, vctx, t_gen);
    let pair: Anf.comp = {
      comp_kind: CPair(im1, im2),
      comp_ty: Prod([im1.imm_ty, im2.imm_ty]),
      comp_complete: default_completeness,
    };

    let (pair_var, pair_bind, t_gen) = mk_bind_var_tmp(pair, t_gen);
    let binds = im1_binds @ im2_binds @ [pair_bind];
    (pair_var, binds, t_gen);

  | ECons(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, vctx, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, vctx, t_gen);
    let cons: Anf.comp = {
      comp_kind: CCons(im1, im2),
      comp_ty: im2.imm_ty,
      comp_complete: default_completeness,
    };

    let (cons_var, cons_bind, t_gen) = mk_bind_var_tmp(cons, t_gen);
    let binds = im1_binds @ im2_binds @ [cons_bind];

    (cons_var, binds, t_gen);

  | EInj(other_ty, side, d) =>
    let (im, im_binds, t_gen) = linearize_exp(d, vctx, t_gen);
    let side = linearize_inj_side(side);

    let inj_ty: HTyp.t =
      switch (side) {
      | CInjL => Sum(im.imm_ty, other_ty)
      | CInjR => Sum(other_ty, im.imm_ty)
      };

    let inj: Anf.comp = {
      comp_kind: CInj(side, im),
      comp_ty: inj_ty,
      comp_complete: default_completeness,
    };

    let (inj_var, inj_bind, t_gen) = mk_bind_var_tmp(inj, t_gen);
    let binds = im_binds @ [inj_bind];
    (inj_var, binds, t_gen);

  | EConsistentCase(case) => linearize_case(case, vctx, t_gen)

  | EEmptyHole(u, i, sigma) =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, vctx, t_gen);
    let hole: Anf.comp = {
      comp_kind: CEmptyHole(u, i, sigma),
      comp_ty: Hole,
      comp_complete: default_completeness,
    };

    let (hole_var, hole_bind, t_gen) = mk_bind_var_tmp(hole, t_gen);
    let binds = sigma_binds @ [hole_bind];
    (hole_var, binds, t_gen);

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, vctx, t_gen);
    let (im, im_binds, t_gen) = linearize_exp(d', vctx, t_gen);
    let hole: Anf.comp = {
      comp_kind: CNonEmptyHole(reason, u, i, sigma, im),
      comp_ty: Hole,
      comp_complete: default_completeness,
    };

    let (hole_var, hole_bind, t_gen) = mk_bind_var_tmp(hole, t_gen);
    let binds = sigma_binds @ im_binds @ [hole_bind];
    (hole_var, binds, t_gen);

  | ECast(d', ty1, ty2) =>
    let (im, im_binds, t_gen) = linearize_exp(d', vctx, t_gen);
    let cast: Anf.comp = {
      comp_kind: CCast(im, ty1, ty2),
      comp_ty: ty2,
      comp_complete: default_completeness,
    };

    let (cast_var, cast_bind, t_gen) = mk_bind_var_tmp(cast, t_gen);
    let binds = im_binds @ [cast_bind];
    (cast_var, binds, t_gen);

  | _ => raise(NotImplemented)
  };
}

and linearize_sigma =
    (sigma: VarMap.t_(Hir.expr), vctx, t_gen)
    : (VarMap.t_(Anf.imm), list(bind), TmpVarGen.t) =>
  List.fold_left(
    ((sigma, sigma_binds, t_gen), (x, d)) => {
      let (im, im_binds, t_gen) = linearize_exp(d, vctx, t_gen);
      let sigma = VarMap.extend(sigma, (x, im));
      (sigma, sigma_binds @ im_binds, t_gen);
    },
    ([], [], t_gen),
    sigma,
  )

and linearize_bin_op =
    (op: Anf.bin_op, ty: HTyp.t, d1: Hir.expr, d2: Hir.expr, vctx, t_gen) => {
  let (im1, im1_binds, t_gen) = linearize_exp(d1, vctx, t_gen);
  let (im2, im2_binds, t_gen) = linearize_exp(d2, vctx, t_gen);
  let bin: Anf.comp = {
    comp_kind: CBinOp(op, im1, im2),
    comp_ty: ty,
    comp_complete: default_completeness,
  };

  let (bin_var, bin_bind, t_gen) = mk_bind_var_tmp(bin, t_gen);
  let binds = im1_binds @ im2_binds @ [bin_bind];
  (bin_var, binds, t_gen);
}

and linearize_case =
    (case: Hir.case, vctx, t_gen): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (case.case_kind) {
  | ECase(scrut, rules, _) =>
    let (scrut_imm, scrut_binds, t_gen) = linearize_exp(scrut, vctx, t_gen);
    let (rules, t_gen) = linearize_rules(rules, vctx, t_gen);

    let rules_ty = List.hd(rules).rule_branch.prog_ty;
    let case: Anf.comp = {
      comp_kind: CCase(scrut_imm, rules),
      comp_ty: rules_ty,
      comp_complete: default_completeness,
    };

    let (case_var, case_bind, t_gen) = mk_bind_var_tmp(case, t_gen);
    let binds = scrut_binds @ [case_bind];
    (case_var, binds, t_gen);
  };
}

and linearize_rules =
    (rules: list(Hir.rule), vctx, t_gen): (list(Anf.rule), TmpVarGen.t) => {
  let (rules_rev, t_gen) =
    rules
    |> List.fold_left(
         ((rules, t_gen), rule) => {
           let (rule, t_gen) = linearize_rule(rule, vctx, t_gen);
           ([rule, ...rules], t_gen);
         },
         ([], t_gen),
       );
  (List.rev(rules_rev), t_gen);
}

and linearize_rule = (rule: Hir.rule, vctx, t_gen): (Anf.rule, TmpVarGen.t) => {
  switch (rule.rule_kind) {
  | ERule(p, branch) =>
    let (p, vctx, t_gen) = linearize_pat(p, vctx, t_gen);
    let (branch, t_gen) = linearize_prog(branch, vctx, t_gen);

    (
      {rule_pat: p, rule_branch: branch, rule_complete: default_completeness},
      t_gen,
    );
  };
}

/* Transform Hir.pat into Anf.pat.
 *
 * Note that the pat_complete property of Anf.pat indicates the indet-ness of the
 * pattern alone, irrespective of the matchee.
 */
and linearize_pat =
    (p: Hir.pat, vctx, t_gen): (Anf.pat, var_remapping, TmpVarGen.t) => {
  linearize_pat_hole(p, vctx, t_gen);
}

and linearize_pat_hole =
    (p: Hir.pat, vctx, t_gen): (Anf.pat, var_remapping, TmpVarGen.t) => {
  switch (p.pat_kind) {
  | PPair(p1, p2) =>
    let (p1, vctx, t_gen) = linearize_pat_hole(p1, vctx, t_gen);
    let (p2, vctx, t_gen) = linearize_pat_hole(p2, vctx, t_gen);
    (
      {pat_kind: PPair(p1, p2), pat_complete: default_completeness},
      vctx,
      t_gen,
    );

  | PCons(p1, p2) =>
    let (p1, vctx, t_gen) = linearize_pat_hole(p1, vctx, t_gen);
    let (p2, vctx, t_gen) = linearize_pat_hole(p2, vctx, t_gen);
    (
      {pat_kind: PCons(p1, p2), pat_complete: default_completeness},
      vctx,
      t_gen,
    );

  | PInj(side, p') =>
    let side = linearize_inj_side(side);
    let (p', vctx, t_gen) = linearize_pat_hole(p', vctx, t_gen);
    (
      {pat_kind: PInj(side, p'), pat_complete: default_completeness},
      vctx,
      t_gen,
    );

  | PWild => (
      {pat_kind: PWild, pat_complete: default_completeness},
      vctx,
      t_gen,
    )

  | PVar(x) =>
    // Mark x as indet whenever the matchee is indet or the pattern is inside a
    // pattern hole.
    let (x', t_gen) = TmpVarGen.next_named(x, t_gen);
    let vctx = VarMap.extend(vctx, (x, x'));
    ({pat_kind: PVar(x'), pat_complete: default_completeness}, vctx, t_gen);

  | PIntLit(i) => (
      {pat_kind: PInt(i), pat_complete: default_completeness},
      vctx,
      t_gen,
    )

  | PFloatLit(f) => (
      {pat_kind: PFloat(f), pat_complete: default_completeness},
      vctx,
      t_gen,
    )

  | PBoolLit(b) => (
      {pat_kind: PBool(b), pat_complete: default_completeness},
      vctx,
      t_gen,
    )

  | PNil => (
      {pat_kind: PNil, pat_complete: default_completeness},
      vctx,
      t_gen,
    )

  | PTriv => (
      {pat_kind: PTriv, pat_complete: default_completeness},
      vctx,
      t_gen,
    )

  | PEmptyHole(_)
  | PNonEmptyHole(_)
  | PKeyword(_)
  | PInvalidText(_)
  | PAp(_) => raise(NotImplemented)
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
  let (prog, _) = linearize_prog(d, VarMap.empty, TmpVarGen.init);
  prog;
};
