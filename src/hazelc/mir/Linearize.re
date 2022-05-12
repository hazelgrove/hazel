exception NotImplemented;

type bind =
  | BLet(Anf.pat, Anf.rec_flag, Anf.comp);

let convert_bind = (bn: bind): Anf.stmt => {
  switch (bn) {
  | BLet(p, rec_flag, c) => {stmt_kind: SLet(p, rec_flag, c)}
  };
};

let rec linearize_var = (var: Var.t): Anf.imm => {imm_kind: IVar(var)}

and linearize_prog =
    (d: IHExp.t, t_gen: TmpVarGen.t): (Anf.prog, TmpVarGen.t) => {
  let (i, binds, t_gen) = linearize_exp(d, t_gen);
  let stmts = binds |> List.map(convert_bind);

  ({prog_body: (stmts, {comp_kind: CImm(i)})}, t_gen);
}

and linearize_exp =
    (d: IHExp.t, t_gen: TmpVarGen.t): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d) {
  | BoundVar(var) => (linearize_var(var), [], t_gen)

  | Let(dp, d1, d2) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (i1, i1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (i2, i2_binds, t_gen) = linearize_exp(d2, t_gen);

    let binds =
      i1_binds @ [BLet(p, NoRec, {comp_kind: CImm(i1)})] @ i2_binds;
    (i2, binds, t_gen);

  | Lam(dp, _, body) =>
    let (p, t_gen) = linearize_pat(dp, t_gen);
    let (body, t_gen) = linearize_prog(body, t_gen);

    let lam: Anf.comp = {comp_kind: CLam([p], body)};
    let (lam_tmp, t_gen) = TmpVarGen.next(t_gen);

    let binds = [BLet(PVar(lam_tmp), NoRec, lam)];
    ({imm_kind: IVar(lam_tmp)}, binds, t_gen);

  | Ap(fn, arg) =>
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
  | ApBuiltin(name, args) =>
    let (args, binds, t_gen) =
      List.fold_left(
        ((args, binds, t_gen), arg) => {
          let (arg, new_binds, t_gen) = linearize_exp(arg, t_gen);
          (args @ [arg], binds @ new_binds, t_gen);
        },
        ([], [], t_gen),
        args,
      );

    let name = linearize_var(name);

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      binds @ [BLet(PVar(ap_tmp), NoRec, {comp_kind: CAp(name, args)})];
    (name, binds, t_gen);

  | BoolLit(b) => ({imm_kind: IConst(ConstBool(b))}, [], t_gen)

  | IntLit(i) => ({imm_kind: IConst(ConstInt(i))}, [], t_gen)

  | FloatLit(f) => ({imm_kind: IConst(ConstFloat(f))}, [], t_gen)

  | ListNil(_) => ({imm_kind: IConst(ConstNil)}, [], t_gen)

  | Triv => ({imm_kind: IConst(ConstTriv)}, [], t_gen)

  | BinBoolOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | And => OpAnd
      | Or => OpOr
      };
    linearize_bin_op(op, d1, d2, t_gen);

  | BinIntOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | Plus => OpPlus
      | Minus => OpMinus
      | Times => OpTimes
      | Divide => OpDivide
      | LessThan => OpLessThan
      | GreaterThan => OpGreaterThan
      | Equals => OpEquals
      };
    linearize_bin_op(op, d1, d2, t_gen);

  | BinFloatOp(op, d1, d2) =>
    let op: Anf.bin_op =
      switch (op) {
      | FPlus => OpFPlus
      | FMinus => OpFMinus
      | FTimes => OpFTimes
      | FDivide => OpFDivide
      | FLessThan => OpFLessThan
      | FGreaterThan => OpFGreaterThan
      | FEquals => OpFEquals
      };
    linearize_bin_op(op, d1, d2, t_gen);

  | Pair(d1, d2) =>
    let (i1, i1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (i2, i2_binds, t_gen) = linearize_exp(d2, t_gen);

    let (pair_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      i1_binds
      @ i2_binds
      @ [BLet(PVar(pair_tmp), NoRec, {comp_kind: CPair(i1, i2)})];

    (linearize_var(pair_tmp), binds, t_gen);

  | Cons(d1, d2) =>
    let (i1, i1_binds, t_gen) = linearize_exp(d1, t_gen);
    let (i2, i2_binds, t_gen) = linearize_exp(d2, t_gen);

    let (cons_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      i1_binds
      @ i2_binds
      @ [BLet(PVar(cons_tmp), NoRec, {comp_kind: CCons(i1, i2)})];

    (linearize_var(cons_tmp), binds, t_gen);

  | Inj(_, side, d) =>
    let (i, binds, t_gen) = linearize_exp(d, t_gen);
    let side = linearize_inj_side(side);

    let (inj_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      binds @ [BLet(PVar(inj_tmp), NoRec, {comp_kind: CInj(side, i)})];

    (linearize_var(inj_tmp), binds, t_gen);

  | EmptyHole(u, i, sigma) =>
    let (sigma, binds, t_gen) = linearize_sigma(sigma, t_gen);

    let (hole_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      binds
      @ [
        BLet(PVar(hole_tmp), NoRec, {comp_kind: CEmptyHole(u, i, sigma)}),
      ];

    (linearize_var(hole_tmp), binds, t_gen);

  | NonEmptyHole(reason, u, i, sigma, d') =>
    let (sigma, sigma_binds, t_gen) = linearize_sigma(sigma, t_gen);
    let (d', d'_binds, t_gen) = linearize_exp(d', t_gen);

    let (hole_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      sigma_binds
      @ d'_binds
      @ [
        BLet(
          PVar(hole_tmp),
          NoRec,
          {comp_kind: CNonEmptyHole(reason, u, i, sigma, d')},
        ),
      ];

    (linearize_var(hole_tmp), binds, t_gen);

  | _ => raise(NotImplemented)
  };
}

and linearize_sigma =
    (sigma: VarMap.t_(IHExp.t), t_gen: TmpVarGen.t)
    : (VarMap.t_(Anf.comp), list(bind), TmpVarGen.t) =>
  List.fold_left(
    ((sigma, binds, t_gen), (x, d)) => {
      let (d, new_binds, t_gen) = linearize_exp(d, t_gen);
      let sigma =
        VarMap.extend(sigma, (x, {comp_kind: CImm(d)}: Anf.comp));
      (sigma, binds @ new_binds, t_gen);
    },
    ([], [], t_gen),
    sigma,
  )

and linearize_bin_op =
    (op: Anf.bin_op, d1: IHExp.t, d2: IHExp.t, t_gen: TmpVarGen.t) => {
  let (i1, i1_binds, t_gen) = linearize_exp(d1, t_gen);
  let (i2, i2_binds, t_gen) = linearize_exp(d2, t_gen);

  let (tmp, t_gen) = TmpVarGen.next(t_gen);
  let binds =
    i1_binds
    @ i2_binds
    @ [BLet(PVar(tmp), NoRec, {comp_kind: CBinOp(op, i1, i2)})];

  (linearize_var(tmp), binds, t_gen);
}

and linearize_pat = (p: IHPat.t, t_gen: TmpVarGen.t): (Anf.pat, TmpVarGen.t) => {
  switch (p) {
  | Wild => (PWild, t_gen)
  | Var(x) => (PVar(x), t_gen)
  | IntLit(i) => (PInt(i), t_gen)
  | FloatLit(f) => (PFloat(f), t_gen)
  | BoolLit(b) => (PBool(b), t_gen)
  | Inj(side, p) =>
    let side = linearize_inj_side(side);
    let (p, t_gen) = linearize_pat(p, t_gen);
    (PInj(side, p), t_gen);
  | ListNil => (PNil, t_gen)
  | Cons(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    (PCons(p1, p2), t_gen);
  | Pair(p1, p2) =>
    let (p1, t_gen) = linearize_pat(p1, t_gen);
    let (p2, t_gen) = linearize_pat(p2, t_gen);
    (PPair(p1, p2), t_gen);
  | Triv => (PTriv, t_gen)
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_)
  | Ap(_) => raise(NotImplemented)
  };
}

and linearize_inj_side = (side: InjSide.t): Anf.inj_side => {
  switch (side) {
  | L => CInjL
  | R => CInjR
  };
};

let linearize = (d: IHExp.t): Anf.prog => {
  let (prog, _) = linearize_prog(d, TmpVarGen.init);
  prog;
};
