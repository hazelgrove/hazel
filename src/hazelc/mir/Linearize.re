type error =
  | NotImplemented
  | WrongType
  | FreeBoundVar(Var.t);

exception Exception(error);

type bind =
  | BLet(Anf.pat, Anf.rec_flag, Anf.comp, Anf.has_indet);

let mk_bind = (p: Anf.pat, rec_flag: Anf.rec_flag, c: Anf.comp) =>
  BLet(p, rec_flag, c, p.pat_indet || c.comp_indet);

let mk_bind_var = (x: Var.t, rec_flag: Anf.rec_flag, c: Anf.comp) => (
  Anf.Imm.mk_var(x, c),
  mk_bind({pat_kind: PVar(x), pat_indet: false}, rec_flag, c),
);

let mk_bind_var_tmp =
    (rec_flag: Anf.rec_flag, c: Anf.comp, t_gen: TmpVarGen.t) => {
  let (x, t_gen) = TmpVarGen.next(t_gen);
  let (var, bind) = mk_bind_var(x, rec_flag, c);
  (var, bind, t_gen);
};

let convert_bind = (bn: bind): Anf.stmt => {
  switch (bn) {
  | BLet(p, rec_flag, c, indet) => {
      stmt_kind: SLet(p, rec_flag, c),
      stmt_indet: indet,
    }
  };
};

/* We maintain a mapping from variables to indet-ness during the linearization
 * process. */
type indet_context = VarMap.t_((Var.t, Anf.has_indet));

let rec linearize_prog = (d: Hir.expr, ictx, t_gen): (Anf.prog, TmpVarGen.t) => {
  let (im, im_binds, t_gen) = linearize_exp(d, ictx, t_gen);
  let body = im_binds |> List.map(convert_bind);

  let c = Anf.Comp.mk_imm(im);
  (Anf.Prog.mk(body, c), t_gen);
}

and linearize_exp =
    (d: Hir.expr, ictx, t_gen): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d.expr_kind) {
  | EBoundVar(ty, x) =>
    let (x', x_indet) =
      switch (VarMap.lookup(ictx, x)) {
      | Some((x', x_indet)) => (x', x_indet)
      | None => raise(Exception(FreeBoundVar(x)))
      };

    ({imm_kind: IVar(x'), imm_ty: ty, imm_indet: x_indet}, [], t_gen);

  | ELet(dp, d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, ictx, t_gen);
    let (p, ictx', t_gen) = linearize_pat(dp, im1.imm_indet, ictx, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, ictx', t_gen);

    let binds =
      im1_binds @ [mk_bind(p, NoRec, Anf.Comp.mk_imm(im1))] @ im2_binds;
    (im2, binds, t_gen);

  | ELam(dp, dp_ty, body) =>
    let (p, ictx, t_gen) = linearize_pat(dp, true, ictx, t_gen);
    let (body, t_gen) = linearize_prog(body, ictx, t_gen);

    let lam: Anf.comp = {
      comp_kind: CLam([p], body),
      comp_ty: Arrow(dp_ty, body.prog_ty),
      comp_indet: p.pat_indet || body.prog_indet,
    };

    let (lam_var, lam_bind, t_gen) = mk_bind_var_tmp(NoRec, lam, t_gen);
    (lam_var, [lam_bind], t_gen);

  | EAp(fn, arg) =>
    let (fn, fn_binds, t_gen) = linearize_exp(fn, ictx, t_gen);
    let (arg, arg_binds, t_gen) = linearize_exp(arg, ictx, t_gen);

    let ap_ty =
      switch (fn.imm_ty) {
      | Arrow(_, ty') => ty'
      | _ => raise(Exception(WrongType))
      };
    let ap: Anf.comp = {
      comp_kind: CAp(fn, [arg]),
      comp_ty: ap_ty,
      comp_indet: fn.imm_indet || arg.imm_indet,
    };

    let (ap_var, ap_bind, t_gen) = mk_bind_var_tmp(NoRec, ap, t_gen);
    let binds = fn_binds @ arg_binds @ [ap_bind];
    (ap_var, binds, t_gen);

  | EBoolLit(b) => (
      {imm_kind: IConst(ConstBool(b)), imm_ty: Bool, imm_indet: false},
      [],
      t_gen,
    )

  | EIntLit(i) => (
      {imm_kind: IConst(ConstInt(i)), imm_ty: Int, imm_indet: false},
      [],
      t_gen,
    )

  | EFloatLit(f) => (
      {imm_kind: IConst(ConstFloat(f)), imm_ty: Float, imm_indet: false},
      [],
      t_gen,
    )

  | ENil(ty) => (
      {imm_kind: IConst(ConstNil(ty)), imm_ty: List(ty), imm_indet: false},
      [],
      t_gen,
    )

  | ETriv => (
      {imm_kind: IConst(ConstTriv), imm_ty: Prod([]), imm_indet: false},
      [],
      t_gen,
    )

  | EBinBoolOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | OpAnd => OpAnd
      | OpOr => OpOr
      };
    linearize_bin_op(op, HTyp.Bool, d1, d2, ictx, t_gen);

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
    linearize_bin_op(op, HTyp.Int, d1, d2, ictx, t_gen);

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
    linearize_bin_op(op, HTyp.Float, d1, d2, ictx, t_gen);

  | EPair(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, ictx, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, ictx, t_gen);
    let pair: Anf.comp = {
      comp_kind: CPair(im1, im2),
      comp_ty: Prod([im1.imm_ty, im2.imm_ty]),
      comp_indet: im1.imm_indet || im2.imm_indet,
    };

    let (pair_var, pair_bind, t_gen) = mk_bind_var_tmp(NoRec, pair, t_gen);
    let binds = im1_binds @ im2_binds @ [pair_bind];
    (pair_var, binds, t_gen);

  | ECons(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, ictx, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, ictx, t_gen);
    let cons: Anf.comp = {
      comp_kind: CCons(im1, im2),
      comp_ty: im2.imm_ty,
      comp_indet: im1.imm_indet || im2.imm_indet,
    };

    let (cons_var, cons_bind, t_gen) = mk_bind_var_tmp(NoRec, cons, t_gen);
    let binds = im1_binds @ im2_binds @ [cons_bind];

    (cons_var, binds, t_gen);

  | EInj(other_ty, side, d) =>
    let (im, im_binds, t_gen) = linearize_exp(d, ictx, t_gen);
    let side = linearize_inj_side(side);

    let inj_ty: HTyp.t =
      switch (side) {
      | CInjL => Sum(im.imm_ty, other_ty)
      | CInjR => Sum(other_ty, im.imm_ty)
      };

    let inj: Anf.comp = {
      comp_kind: CInj(side, im),
      comp_ty: inj_ty,
      comp_indet: im.imm_indet,
    };

    let (inj_var, inj_bind, t_gen) = mk_bind_var_tmp(NoRec, inj, t_gen);
    let binds = im_binds @ [inj_bind];
    (inj_var, binds, t_gen);

  | EEmptyHole(u, i, sigma) =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, ictx, t_gen);
    let hole: Anf.comp = {
      comp_kind: CEmptyHole(u, i, sigma),
      comp_ty: Hole,
      comp_indet: true,
    };

    let (hole_var, hole_bind, t_gen) = mk_bind_var_tmp(NoRec, hole, t_gen);
    let binds = sigma_binds @ [hole_bind];
    (hole_var, binds, t_gen);

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, ictx, t_gen);
    let (im, im_binds, t_gen) = linearize_exp(d', ictx, t_gen);
    let hole: Anf.comp = {
      comp_kind: CNonEmptyHole(reason, u, i, sigma, im),
      comp_ty: Hole,
      comp_indet: true,
    };

    let (hole_var, hole_bind, t_gen) = mk_bind_var_tmp(NoRec, hole, t_gen);
    let binds = sigma_binds @ im_binds @ [hole_bind];
    (hole_var, binds, t_gen);

  | ECast(d', ty1, ty2) =>
    let (im, im_binds, t_gen) = linearize_exp(d', ictx, t_gen);
    let cast: Anf.comp = {
      comp_kind: CCast(im, ty1, ty2),
      comp_ty: ty2,
      comp_indet: true,
    };

    let (cast_var, cast_bind, t_gen) = mk_bind_var_tmp(NoRec, cast, t_gen);
    let binds = im_binds @ [cast_bind];
    (cast_var, binds, t_gen);

  | EKeyword(_, _, _, _)
  | EFreeVar(_, _, _, _)
  | EInvalidText(_, _, _, _)
  | EInvalidOperation(_, _)
  | EFailedCast(_, _, _)
  | EConsistentCase(_)
  | EInconsistentBranches(_, _, _, _)
  | ELetRec(_, _, _, _, _)
  | EApBuiltin(_, _) => raise(Exception(NotImplemented))
  };
}

and linearize_sigma =
    (sigma: VarMap.t_(Hir.expr), ictx, t_gen)
    : (VarMap.t_(Anf.comp), list(bind), TmpVarGen.t) =>
  List.fold_left(
    ((sigma, sigma_binds, t_gen), (x, d)) => {
      let (im, im_binds, t_gen) = linearize_exp(d, ictx, t_gen);
      let c = Anf.Comp.mk_imm(im);
      let sigma = VarMap.extend(sigma, (x, c));
      (sigma, sigma_binds @ im_binds, t_gen);
    },
    ([], [], t_gen),
    sigma,
  )

and linearize_bin_op =
    (op: Anf.bin_op, ty: HTyp.t, d1: Hir.expr, d2: Hir.expr, ictx, t_gen) => {
  let (im1, im1_binds, t_gen) = linearize_exp(d1, ictx, t_gen);
  let (im2, im2_binds, t_gen) = linearize_exp(d2, ictx, t_gen);
  let bin: Anf.comp = {
    comp_kind: CBinOp(op, im1, im2),
    comp_ty: ty,
    comp_indet: im1.imm_indet || im2.imm_indet,
  };

  let (bin_var, bin_bind, t_gen) = mk_bind_var_tmp(NoRec, bin, t_gen);
  let binds = im1_binds @ im2_binds @ [bin_bind];
  (bin_var, binds, t_gen);
}

/* Transform Hir.pat into Anf.pat.
 *
 * Note that the pat_indet property of Anf.pat indicates the indet-ness of the
 * pattern alone, irrespective of the matchee.
 */
and linearize_pat =
    (p: Hir.pat, d_indet: Anf.has_indet, ictx, t_gen)
    : (Anf.pat, indet_context, TmpVarGen.t) => {
  linearize_pat_hole(p, d_indet, false, ictx, t_gen);
}

and linearize_pat_hole =
    (p: Hir.pat, d_indet: Anf.has_indet, in_hole: bool, ictx, t_gen)
    : (Anf.pat, indet_context, TmpVarGen.t) => {
  switch (p.pat_kind) {
  | PPair(p1, p2) =>
    let (p1, ictx, t_gen) =
      linearize_pat_hole(p1, d_indet, in_hole, ictx, t_gen);
    let (p2, ictx, t_gen) =
      linearize_pat_hole(p2, d_indet, in_hole, ictx, t_gen);
    (
      {pat_kind: PPair(p1, p2), pat_indet: p1.pat_indet || p2.pat_indet},
      ictx,
      t_gen,
    );

  | PCons(p1, p2) =>
    let (p1, ictx, t_gen) =
      linearize_pat_hole(p1, d_indet, in_hole, ictx, t_gen);
    let (p2, ictx, t_gen) =
      linearize_pat_hole(p2, d_indet, in_hole, ictx, t_gen);
    (
      {pat_kind: PCons(p1, p2), pat_indet: p1.pat_indet || p2.pat_indet},
      ictx,
      t_gen,
    );

  | PInj(side, p') =>
    let side = linearize_inj_side(side);
    let (p', ictx, t_gen) =
      linearize_pat_hole(p', d_indet, in_hole, ictx, t_gen);
    ({pat_kind: PInj(side, p'), pat_indet: p'.pat_indet}, ictx, t_gen);

  | PWild => ({pat_kind: PWild, pat_indet: false}, ictx, t_gen)

  | PVar(x) =>
    // Mark x as indet whenever the matchee is indet or the pattern is inside a
    // pattern hole.
    let (x', t_gen) = TmpVarGen.next_named(x, t_gen);
    let ictx = VarMap.extend(ictx, (x, (x', d_indet || in_hole)));
    ({pat_kind: PVar(x'), pat_indet: false}, ictx, t_gen);

  | PIntLit(i) => ({pat_kind: PInt(i), pat_indet: false}, ictx, t_gen)

  | PFloatLit(f) => ({pat_kind: PFloat(f), pat_indet: false}, ictx, t_gen)

  | PBoolLit(b) => ({pat_kind: PBool(b), pat_indet: false}, ictx, t_gen)

  | PNil => ({pat_kind: PNil, pat_indet: false}, ictx, t_gen)

  | PTriv => ({pat_kind: PTriv, pat_indet: false}, ictx, t_gen)

  | PEmptyHole(_)
  | PNonEmptyHole(_)
  | PKeyword(_)
  | PInvalidText(_)
  | PAp(_) => raise(Exception(NotImplemented))
  };
}

and linearize_inj_side = (side: InjSide.t): Anf.inj_side => {
  switch (side) {
  | L => CInjL
  | R => CInjR
  };
};

let linearize = (d: Hir.expr): Anf.prog => {
  // TODO: Can't pass empty ictx once builtins are supported.
  let (prog, _) = linearize_prog(d, VarMap.empty, TmpVarGen.init);
  prog;
};
