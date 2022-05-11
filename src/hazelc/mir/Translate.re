exception NotImplemented;

type bind =
  | BLet(Var.t, Anf.rec_flag, Anf.comp);

let rec translate_var = (var: Var.t): Anf.imm => {imm_kind: IVar(var)}

and translate_exp =
    (d: IHExp.t, t_gen: TmpVarGen.t): (Anf.imm, list(bind), TmpVarGen.t) => {
  switch (d) {
  | BoundVar(var) => (translate_var(var), [], t_gen)

  | Ap(fn, arg) =>
    let (fn, fn_binds, t_gen) = translate_exp(fn, t_gen);
    let (arg, arg_binds, t_gen) = translate_exp(arg, t_gen);

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      fn_binds
      @ arg_binds
      @ [BLet(ap_tmp, NoRec, {comp_kind: CAp(fn, [arg])})];

    ({imm_kind: IVar(ap_tmp)}, binds, t_gen);

  // TODO: Transform DHExp.ApBuiltin into IHExp.Ap at above level? Need to
  // ensure no name conflicts.
  | ApBuiltin(name, args) =>
    let (args, binds, t_gen) =
      List.fold_left(
        ((args, binds, t_gen), arg) => {
          let (arg, new_binds, t_gen) = translate_exp(arg, t_gen);
          (args @ [arg], binds @ new_binds, t_gen);
        },
        ([], [], t_gen),
        args,
      );

    let name = translate_var(name);

    let (ap_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      binds @ [BLet(ap_tmp, NoRec, {comp_kind: CAp(name, args)})];
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
    translate_bin_op(op, d1, d2, t_gen);

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
    translate_bin_op(op, d1, d2, t_gen);

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
    translate_bin_op(op, d1, d2, t_gen);

  | Pair(d1, d2) =>
    let (i1, i1_binds, t_gen) = translate_exp(d1, t_gen);
    let (i2, i2_binds, t_gen) = translate_exp(d2, t_gen);

    let (pair_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      i1_binds
      @ i2_binds
      @ [BLet(pair_tmp, NoRec, {comp_kind: CPair(i1, i2)})];

    (translate_var(pair_tmp), binds, t_gen);

  | Cons(d1, d2) =>
    let (i1, i1_binds, t_gen) = translate_exp(d1, t_gen);
    let (i2, i2_binds, t_gen) = translate_exp(d2, t_gen);

    let (cons_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds =
      i1_binds
      @ i2_binds
      @ [BLet(cons_tmp, NoRec, {comp_kind: CCons(i1, i2)})];

    (translate_var(cons_tmp), binds, t_gen);

  | Inj(_, side, d) =>
    let (i, binds, t_gen) = translate_exp(d, t_gen);
    let side: Anf.inj_side =
      switch (side) {
      | L => CInjL
      | R => CInjR
      };

    let (inj_tmp, t_gen) = TmpVarGen.next(t_gen);
    let binds = binds @ [BLet(inj_tmp, NoRec, {comp_kind: CInj(side, i)})];

    (translate_var(inj_tmp), binds, t_gen);

  | _ => raise(NotImplemented)
  };
}

and translate_bin_op =
    (op: Anf.bin_op, d1: IHExp.t, d2: IHExp.t, t_gen: TmpVarGen.t) => {
  let (i1, i1_binds, t_gen) = translate_exp(d1, t_gen);
  let (i2, i2_binds, t_gen) = translate_exp(d2, t_gen);

  let (tmp, t_gen) = TmpVarGen.next(t_gen);
  let binds =
    i1_binds
    @ i2_binds
    @ [BLet(tmp, NoRec, {comp_kind: CBinOp(op, i1, i2)})];

  (translate_var(tmp), binds, t_gen);
};

let translate_bind = (bn: bind): Anf.stmt => {
  switch (bn) {
  | BLet(var, rec_flag, c) => {stmt_kind: SLet(var, rec_flag, c)}
  };
};

let translate = (d: IHExp.t): Anf.prog => {
  let (i, binds, _) = translate_exp(d, TmpVarGen.init);
  let stmts = binds |> List.map(translate_bind);

  {prog_body: (stmts, {comp_kind: CImm(i)})};
};
