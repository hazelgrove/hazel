exception NotImplemented;
exception WrongType;

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

let rec linearize_prog = (d: Hir.expr, t_gen): (Anf.prog, TmpVarGen.t) => {
  let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
  let body = im_binds |> List.map(convert_bind);

  let c = Anf.Comp.mk_imm(im);
  (Anf.Prog.mk(body, c), t_gen);
}

and linearize_exp = (d: Hir.expr, t_gen): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d.expr_kind) {
  | EBoundVar(ty, x) => (
      // FIXME: Need to determine if variable is indet or not.
      {imm_kind: IVar(x), imm_ty: ty, imm_indet: true},
      [],
      t_gen,
    )

  | ELet(dp, d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);

    let binds =
      im1_binds @ [mk_bind(p, NoRec, Anf.Comp.mk_imm(im1))] @ im2_binds;
    (im2, binds, t_gen);

  | ELam(dp, dp_ty, body) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (body, t_gen) = linearize_prog(body, t_gen);

    let lam: Anf.comp = {
      comp_kind: CLam([p], body),
      comp_ty: Arrow(dp_ty, body.prog_ty),
      comp_indet: p.pat_indet || body.prog_indet,
    };

    let (lam_var, lam_bind, t_gen) = mk_bind_var_tmp(NoRec, lam, t_gen);
    (lam_var, [lam_bind], t_gen);

  | EAp(fn, arg) =>
    let (fn, fn_binds, t_gen) = linearize_exp(fn, t_gen);
    let (arg, arg_binds, t_gen) = linearize_exp(arg, t_gen);

    let ap_ty =
      switch (fn.imm_ty) {
      | Arrow(_, ty') => ty'
      | _ => raise(WrongType)
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
    linearize_bin_op(op, HTyp.Bool, d1, d2, t_gen);

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
    linearize_bin_op(op, HTyp.Int, d1, d2, t_gen);

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
    linearize_bin_op(op, HTyp.Float, d1, d2, t_gen);

  | EPair(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);
    let pair: Anf.comp = {
      comp_kind: CPair(im1, im2),
      comp_ty: Prod([im1.imm_ty, im2.imm_ty]),
      comp_indet: im1.imm_indet || im2.imm_indet,
    };

    let (pair_var, pair_bind, t_gen) = mk_bind_var_tmp(NoRec, pair, t_gen);
    let binds = im1_binds @ im2_binds @ [pair_bind];
    (pair_var, binds, t_gen);

  | ECons(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);
    let cons: Anf.comp = {
      comp_kind: CCons(im1, im2),
      comp_ty: im2.imm_ty,
      comp_indet: im1.imm_indet || im2.imm_indet,
    };

    let (cons_var, cons_bind, t_gen) = mk_bind_var_tmp(NoRec, cons, t_gen);
    let binds = im1_binds @ im2_binds @ [cons_bind];

    (cons_var, binds, t_gen);

  | EInj(other_ty, side, d) =>
    let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
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
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);
    let hole: Anf.comp = {
      comp_kind: CEmptyHole(u, i, sigma),
      comp_ty: Hole,
      comp_indet: true,
    };

    let (hole_var, hole_bind, t_gen) = mk_bind_var_tmp(NoRec, hole, t_gen);
    let binds = sigma_binds @ [hole_bind];
    (hole_var, binds, t_gen);

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);
    let (im, im_binds, t_gen) = linearize_exp(d', t_gen);
    let hole: Anf.comp = {
      comp_kind: CNonEmptyHole(reason, u, i, sigma, im),
      comp_ty: Hole,
      comp_indet: true,
    };

    let (hole_var, hole_bind, t_gen) = mk_bind_var_tmp(NoRec, hole, t_gen);
    let binds = sigma_binds @ im_binds @ [hole_bind];
    (hole_var, binds, t_gen);

  | ECast(d', ty1, ty2) =>
    let (im, im_binds, t_gen) = linearize_exp(d', t_gen);
    let cast: Anf.comp = {
      comp_kind: CCast(im, ty1, ty2),
      comp_ty: ty2,
      comp_indet: true,
    };

    let (cast_var, cast_bind, t_gen) = mk_bind_var_tmp(NoRec, cast, t_gen);
    let binds = im_binds @ [cast_bind];
    (cast_var, binds, t_gen);

  | _ => raise(NotImplemented)
  };
}

and linearize_sigma =
    (sigma: VarMap.t_(Hir.expr), t_gen)
    : (VarMap.t_(Anf.comp), list(bind), TmpVarGen.t) =>
  List.fold_left(
    ((sigma, sigma_binds, t_gen), (x, d)) => {
      let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
      let c = Anf.Comp.mk_imm(im);
      let sigma = VarMap.extend(sigma, (x, c));
      (sigma, sigma_binds @ im_binds, t_gen);
    },
    ([], [], t_gen),
    sigma,
  )

and linearize_bin_op =
    (op: Anf.bin_op, ty: HTyp.t, d1: Hir.expr, d2: Hir.expr, t_gen) => {
  let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
  let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);
  let bin: Anf.comp = {
    comp_kind: CBinOp(op, im1, im2),
    comp_ty: ty,
    comp_indet: im1.imm_indet || im2.imm_indet,
  };

  let (bin_var, bin_bind, t_gen) = mk_bind_var_tmp(NoRec, bin, t_gen);
  let binds = im1_binds @ im2_binds @ [bin_bind];
  (bin_var, binds, t_gen);
}

and linearize_pat = (p: Hir.pat, t_gen): (Anf.pat, TmpVarGen.t) => {
  switch (p.pat_kind) {
  | PPair(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    (
      {pat_kind: PPair(p1, p2), pat_indet: p1.pat_indet || p2.pat_indet},
      t_gen,
    );

  | PCons(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    (
      {pat_kind: PCons(p1, p2), pat_indet: p1.pat_indet || p2.pat_indet},
      t_gen,
    );

  | PInj(side, p') =>
    let side = linearize_inj_side(side);
    let (p', t_gen) = linearize_pat(p', t_gen);
    ({pat_kind: PInj(side, p'), pat_indet: p'.pat_indet}, t_gen);

  | PWild => ({pat_kind: PWild, pat_indet: false}, t_gen)

  | PVar(x) => ({pat_kind: PVar(x), pat_indet: false}, t_gen)

  | PIntLit(i) => ({pat_kind: PInt(i), pat_indet: false}, t_gen)

  | PFloatLit(f) => ({pat_kind: PFloat(f), pat_indet: false}, t_gen)

  | PBoolLit(b) => ({pat_kind: PBool(b), pat_indet: false}, t_gen)

  | PNil => ({pat_kind: PNil, pat_indet: false}, t_gen)

  | PTriv => ({pat_kind: PTriv, pat_indet: false}, t_gen)

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
  let (prog, _) = linearize_prog(d, TmpVarGen.init);
  prog;
};
