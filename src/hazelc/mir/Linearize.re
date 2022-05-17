exception NotImplemented;

type bind =
  | BLet(Anf.pat, Anf.rec_flag, Anf.comp, Hir.has_indet);

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

  let c: Anf.comp = {comp_kind: CImm(im), comp_indet: d.expr_indet};
  ({prog_body: (body, c), prog_indet: d.expr_indet}, t_gen);
}

and linearize_exp = (d: Hir.expr, t_gen): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d.expr_kind) {
  | EBoundVar(x) => (
      {imm_kind: IVar(x), imm_indet: d.expr_indet},
      [],
      t_gen,
    )

  | ELet(dp, d1, d2) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);

    let {imm_kind: im2_kind, imm_indet: _}: Anf.imm = im2;

    let binds =
      im1_binds
      @ [
        BLet(
          p,
          NoRec,
          {comp_kind: CImm(im1), comp_indet: d.expr_indet},
          d.expr_indet,
        ),
      ]
      @ im2_binds;

    ({imm_kind: im2_kind, imm_indet: d.expr_indet}, binds, t_gen);

  | ELam(dp, _, body) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (body, t_gen) = linearize_prog(body, t_gen);

    let lam: Anf.comp = {
      comp_kind: CLam([p], body),
      comp_indet: d.expr_indet,
    };
    let (lam_tmp, t_gen) = TmpVarGen.next(t_gen);

    let binds = [
      BLet(
        {pat_kind: PVar(lam_tmp), pat_indet: false},
        NoRec,
        lam,
        d.expr_indet,
      ),
    ];
    ({imm_kind: IVar(lam_tmp), imm_indet: lam.comp_indet}, binds, t_gen);

  | EAp(fn, arg) =>
    let (fn, fn_binds, t_gen) = linearize_exp(fn, t_gen);
    let (arg, arg_binds, t_gen) = linearize_exp(arg, t_gen);
    let ap: Anf.comp = {
      comp_kind: CAp(fn, [arg]),
      comp_indet: d.expr_indet,
    };

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      fn_binds
      @ arg_binds
      @ [
        BLet(
          {pat_kind: PVar(ap_tmp), pat_indet: false},
          NoRec,
          ap,
          ap.comp_indet,
        ),
      ];

    ({imm_kind: IVar(ap_tmp), imm_indet: ap.comp_indet}, binds, t_gen);

  // TODO: Transform DHExp.ApBuiltin into IHExp.Ap at above level? Need to
  // ensure no name conflicts.
  | EApBuiltin(name, args) =>
    let (args, args_binds, t_gen) =
      List.fold_left(
        ((args, args_binds, t_gen), arg) => {
          let (arg, new_binds, t_gen) = linearize_exp(arg, t_gen);
          (args @ [arg], args_binds @ new_binds, t_gen);
        },
        ([], [], t_gen),
        args,
      );
    let ap: Anf.comp = {
      comp_kind: CAp({imm_kind: IVar(name), imm_indet: false}, args),
      comp_indet: d.expr_indet,
    };

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      args_binds
      @ [
        BLet(
          {pat_kind: PVar(ap_tmp), pat_indet: false},
          NoRec,
          ap,
          ap.comp_indet,
        ),
      ];
    ({imm_kind: IVar(ap_tmp), imm_indet: ap.comp_indet}, binds, t_gen);

  | EBoolLit(b) => (
      {imm_kind: IConst(ConstBool(b)), imm_indet: d.expr_indet},
      [],
      t_gen,
    )

  | EIntLit(i) => (
      {imm_kind: IConst(ConstInt(i)), imm_indet: d.expr_indet},
      [],
      t_gen,
    )

  | EFloatLit(f) => (
      {imm_kind: IConst(ConstFloat(f)), imm_indet: d.expr_indet},
      [],
      t_gen,
    )

  | EListNil(ty) => (
      {imm_kind: IConst(ConstNil(ty)), imm_indet: d.expr_indet},
      [],
      t_gen,
    )

  | ETriv => (
      {imm_kind: IConst(ConstTriv), imm_indet: d.expr_indet},
      [],
      t_gen,
    )

  | EBinBoolOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | OpAnd => OpAnd
      | OpOr => OpOr
      };
    linearize_bin_op(op, d, d1, d2, t_gen);

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
    linearize_bin_op(op, d, d1, d2, t_gen);

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
    linearize_bin_op(op, d, d1, d2, t_gen);

  | EPair(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);
    let pair: Anf.comp = {
      comp_kind: CPair(im1, im2),
      comp_indet: d.expr_indet,
    };

    let (pair_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im1_binds
      @ im2_binds
      @ [
        BLet(
          {pat_kind: PVar(pair_tmp), pat_indet: false},
          NoRec,
          pair,
          pair.comp_indet,
        ),
      ];

    ({imm_kind: IVar(pair_tmp), imm_indet: pair.comp_indet}, binds, t_gen);

  | ECons(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);
    let cons: Anf.comp = {
      comp_kind: CCons(im1, im2),
      comp_indet: d.expr_indet,
    };

    let (cons_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im1_binds
      @ im2_binds
      @ [
        BLet(
          {pat_kind: PVar(cons_tmp), pat_indet: false},
          NoRec,
          cons,
          cons.comp_indet,
        ),
      ];

    ({imm_kind: IVar(cons_tmp), imm_indet: cons.comp_indet}, binds, t_gen);

  | EInj(_, side, d) =>
    let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
    let side = linearize_inj_side(side);
    let inj: Anf.comp = {
      comp_kind: CInj(side, im),
      comp_indet: d.expr_indet,
    };

    let (inj_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im_binds
      @ [
        BLet(
          {pat_kind: PVar(inj_tmp), pat_indet: false},
          NoRec,
          inj,
          inj.comp_indet,
        ),
      ];

    ({imm_kind: IVar(inj_tmp), imm_indet: inj.comp_indet}, binds, t_gen);

  | EEmptyHole(u, i, sigma) =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);
    let hole: Anf.comp = {
      comp_kind: CEmptyHole(u, i, sigma),
      comp_indet: d.expr_indet,
    };

    let (hole_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      sigma_binds
      @ [
        BLet(
          {pat_kind: PVar(hole_tmp), pat_indet: false},
          NoRec,
          hole,
          hole.comp_indet,
        ),
      ];

    ({imm_kind: IVar(hole_tmp), imm_indet: hole.comp_indet}, binds, t_gen);

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);
    let (im, im_binds, t_gen) = linearize_exp(d', t_gen);
    let hole: Anf.comp = {
      comp_kind: CNonEmptyHole(reason, u, i, sigma, im),
      comp_indet: d.expr_indet,
    };

    let (hole_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      sigma_binds
      @ im_binds
      @ [
        BLet(
          {pat_kind: PVar(hole_tmp), pat_indet: false},
          NoRec,
          hole,
          hole.comp_indet,
        ),
      ];

    ({imm_kind: IVar(hole_tmp), imm_indet: hole.comp_indet}, binds, t_gen);

  | ECast(d', t1, t2) =>
    let (im, im_binds, t_gen) = linearize_exp(d', t_gen);
    let cast: Anf.comp = {
      comp_kind: CCast(im, t1, t2),
      comp_indet: d.expr_indet,
    };

    let (cast_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im_binds
      @ [
        BLet(
          {pat_kind: PVar(cast_tmp), pat_indet: false},
          NoRec,
          cast,
          cast.comp_indet,
        ),
      ];

    ({imm_kind: IVar(cast_tmp), imm_indet: cast.comp_indet}, binds, t_gen);

  | _ => raise(NotImplemented)
  };
}

and linearize_sigma =
    (sigma: VarMap.t_(Hir.expr), t_gen)
    : (VarMap.t_(Anf.comp), list(bind), TmpVarGen.t) =>
  List.fold_left(
    ((sigma, sigma_binds, t_gen), (x, d)) => {
      let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
      let c: Anf.comp = {comp_kind: CImm(im), comp_indet: d.expr_indet};
      let sigma = VarMap.extend(sigma, (x, c));
      (sigma, sigma_binds @ im_binds, t_gen);
    },
    ([], [], t_gen),
    sigma,
  )

and linearize_bin_op =
    (op: Anf.bin_op, d: Hir.expr, d1: Hir.expr, d2: Hir.expr, t_gen) => {
  let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
  let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);
  let bin: Anf.comp = {
    comp_kind: CBinOp(op, im1, im2),
    comp_indet: d.expr_indet,
  };

  let (bin_tmp, t_gen) = TmpVarGen.next(t_gen);
  let binds =
    im1_binds
    @ im2_binds
    @ [
      BLet(
        {pat_kind: PVar(bin_tmp), pat_indet: false},
        NoRec,
        bin,
        bin.comp_indet,
      ),
    ];

  ({imm_kind: IVar(bin_tmp), imm_indet: bin.comp_indet}, binds, t_gen);
}

and linearize_pat = (p: Hir.pat, t_gen): (Anf.pat, TmpVarGen.t) => {
  switch (p.pat_kind) {
  | PWild => ({pat_kind: PWild, pat_indet: p.pat_indet}, t_gen)

  | PVar(x) => ({pat_kind: PVar(x), pat_indet: p.pat_indet}, t_gen)

  | PIntLit(i) => ({pat_kind: PInt(i), pat_indet: p.pat_indet}, t_gen)

  | PFloatLit(f) => ({pat_kind: PFloat(f), pat_indet: p.pat_indet}, t_gen)

  | PBoolLit(b) => ({pat_kind: PBool(b), pat_indet: p.pat_indet}, t_gen)

  | PInj(side, p') =>
    let side = linearize_inj_side(side);
    let (p', t_gen) = linearize_pat(p', t_gen);
    ({pat_kind: PInj(side, p'), pat_indet: p.pat_indet}, t_gen);

  | PListNil => ({pat_kind: PNil, pat_indet: p.pat_indet}, t_gen)

  | PCons(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    ({pat_kind: PCons(p1, p2), pat_indet: p.pat_indet}, t_gen);

  | PPair(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    ({pat_kind: PPair(p1, p2), pat_indet: p.pat_indet}, t_gen);

  | PTriv => ({pat_kind: PTriv, pat_indet: p.pat_indet}, t_gen)

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
