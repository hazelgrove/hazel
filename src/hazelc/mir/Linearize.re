exception NotImplemented;

type bind =
  | BLet(Anf.pat, Anf.rec_flag, Anf.comp);

let convert_bind = (bn: bind): Anf.stmt => {
  switch (bn) {
  | BLet(p, rec_flag, c) => {stmt_kind: SLet(p, rec_flag, c)}
  };
};

let rec linearize_var = (x: Var.t): Anf.imm => {imm_kind: IVar(x)}

and linearize_prog = (d: Hir.expr, t_gen): (Anf.prog, TmpVarGen.t) => {
  let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
  let body = im_binds |> List.map(convert_bind);

  ({prog_body: (body, {comp_kind: CImm(im)})}, t_gen);
}

and linearize_exp = (d: Hir.expr, t_gen): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d.expr_kind) {
  | EBoundVar(x) => (linearize_var(x), [], t_gen)

  | ELet(dp, d1, d2) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);

    let binds =
      im1_binds @ [BLet(p, NoRec, {comp_kind: CImm(im1)})] @ im2_binds;
    (im2, binds, t_gen);

  | ELam(dp, _, body) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (body, t_gen) = linearize_prog(body, t_gen);

    let lam: Anf.comp = {comp_kind: CLam([p], body)};
    let (lam_tmp, t_gen) = TmpVarGen.next(t_gen);

    let binds = [BLet(PVar(lam_tmp), NoRec, lam)];
    ({imm_kind: IVar(lam_tmp)}, binds, t_gen);

  | EAp(fn, arg) =>
    let (fn, fn_binds, t_gen) = linearize_exp(fn, t_gen);
    let (arg, arg_binds, t_gen) = linearize_exp(arg, t_gen);

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      fn_binds
      @ arg_binds
      @ [BLet(PVar(ap_tmp), NoRec, {comp_kind: CAp(fn, [arg])})];

    ({imm_kind: IVar(ap_tmp)}, binds, t_gen);

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

    let name = linearize_var(name);

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      args_binds
      @ [BLet(PVar(ap_tmp), NoRec, {comp_kind: CAp(name, args)})];
    (name, binds, t_gen);

  | EBoolLit(b) => ({imm_kind: IConst(ConstBool(b))}, [], t_gen)

  | EIntLit(i) => ({imm_kind: IConst(ConstInt(i))}, [], t_gen)

  | EFloatLit(f) => ({imm_kind: IConst(ConstFloat(f))}, [], t_gen)

  | EListNil(ty) => ({imm_kind: IConst(ConstNil(ty))}, [], t_gen)

  | ETriv => ({imm_kind: IConst(ConstTriv)}, [], t_gen)

  | EBinBoolOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | OpAnd => OpAnd
      | OpOr => OpOr
      };
    linearize_bin_op(op, d1, d2, t_gen);

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
    linearize_bin_op(op, d1, d2, t_gen);

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
    linearize_bin_op(op, d1, d2, t_gen);

  | EPair(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);

    let (pair_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im1_binds
      @ im2_binds
      @ [BLet(PVar(pair_tmp), NoRec, {comp_kind: CPair(im1, im2)})];

    (linearize_var(pair_tmp), binds, t_gen);

  | ECons(d1, d2) =>
    let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);

    let (cons_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im1_binds
      @ im2_binds
      @ [BLet(PVar(cons_tmp), NoRec, {comp_kind: CCons(im1, im2)})];

    (linearize_var(cons_tmp), binds, t_gen);

  | EInj(_, side, d) =>
    let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
    let side = linearize_inj_side(side);

    let (inj_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im_binds @ [BLet(PVar(inj_tmp), NoRec, {comp_kind: CInj(side, im)})];

    (linearize_var(inj_tmp), binds, t_gen);

  | EEmptyHole(u, i, sigma) =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);

    let (hole_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      sigma_binds
      @ [
        BLet(PVar(hole_tmp), NoRec, {comp_kind: CEmptyHole(u, i, sigma)}),
      ];

    (linearize_var(hole_tmp), binds, t_gen);

  | ENonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);
    let (im, im_binds, t_gen) = linearize_exp(d', t_gen);

    let (hole_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      sigma_binds
      @ im_binds
      @ [
        BLet(
          PVar(hole_tmp),
          NoRec,
          {comp_kind: CNonEmptyHole(reason, u, i, sigma, im)},
        ),
      ];

    (linearize_var(hole_tmp), binds, t_gen);

  | ECast(d', t1, t2) =>
    let (im, im_binds, t_gen) = linearize_exp(d', t_gen);

    let (cast_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      im_binds
      @ [BLet(PVar(cast_tmp), NoRec, {comp_kind: CCast(im, t1, t2)})];

    (linearize_var(cast_tmp), binds, t_gen);

  | _ => raise(NotImplemented)
  };
}

and linearize_sigma =
    (sigma: VarMap.t_(Hir.expr), t_gen)
    : (VarMap.t_(Anf.comp), list(bind), TmpVarGen.t) =>
  List.fold_left(
    ((sigma, sigma_binds, t_gen), (x, d)) => {
      let (im, im_binds, t_gen) = linearize_exp(d, t_gen);
      let sigma =
        VarMap.extend(sigma, (x, {comp_kind: CImm(im)}: Anf.comp));
      (sigma, sigma_binds @ im_binds, t_gen);
    },
    ([], [], t_gen),
    sigma,
  )

and linearize_bin_op = (op: Anf.bin_op, d1: Hir.expr, d2: Hir.expr, t_gen) => {
  let (im1, im1_binds, t_gen) = linearize_exp(d1, t_gen);
  let (im2, im2_binds, t_gen) = linearize_exp(d2, t_gen);

  let (bin_tmp, t_gen) = TmpVarGen.next(t_gen);
  let binds =
    im1_binds
    @ im2_binds
    @ [BLet(PVar(bin_tmp), NoRec, {comp_kind: CBinOp(op, im1, im2)})];

  (linearize_var(bin_tmp), binds, t_gen);
}

and linearize_pat = (p: Hir.pat, t_gen): (Anf.pat, TmpVarGen.t) => {
  switch (p.pat_kind) {
  | PWild => (PWild, t_gen)
  | PVar(x) => (PVar(x), t_gen)
  | PIntLit(i) => (PInt(i), t_gen)
  | PFloatLit(f) => (PFloat(f), t_gen)
  | PBoolLit(b) => (PBool(b), t_gen)
  | PInj(side, p) =>
    let side = linearize_inj_side(side);
    let (p, t_gen) = linearize_pat(p, t_gen);
    (PInj(side, p), t_gen);
  | PListNil => (PNil, t_gen)
  | PCons(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    (PCons(p1, p2), t_gen);
  | PPair(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    (PPair(p1, p2), t_gen);
  | PTriv => (PTriv, t_gen)
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
