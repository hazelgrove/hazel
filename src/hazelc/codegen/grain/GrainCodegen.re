module HazelStd = GrainStd.Hazel;

let injl_ctor = "L";
let injr_ctor = "R";

let rec codegen_prog = ({prog_body: (stmts, c)}: Anf.prog): GrainIR.block => {
  let stmts = stmts |> List.map(codegen_stmt);
  let c = codegen_comp(c);
  stmts @ [SExpr(c)];
}

and codegen_stmt = (stmt: Anf.stmt): GrainIR.stmt => {
  switch (stmt.stmt_kind) {
  | SLet(p, NoRec, c) => SLet([codegen_pat(p)], codegen_comp(c))
  | SLet(p, Rec, c) => SLetRec([codegen_pat(p)], codegen_comp(c))
  };
}

and codegen_comp = (c: Anf.comp): GrainIR.expr => {
  switch (c.comp_kind) {
  | CImm(i) => codegen_imm(i)
  | CBinOp(op, i1, i2) =>
    EBinOp(codegen_op(op), codegen_imm(i1), codegen_imm(i2))
  | CAp(fn, args) =>
    let args = args |> List.map(codegen_imm);
    EAp(codegen_imm(fn), args);
  | CLam(params, body) =>
    let params = params |> List.map(codegen_pat);
    ELam(params, EBlock(codegen_prog(body)));
  | CCons(i1, i2) => ECons(codegen_imm(i1), codegen_imm(i2))
  | CPair(i1, i2) => ETuple([codegen_imm(i1), codegen_imm(i2)])
  | CInj(side, i) =>
    let ctor = codegen_inj_side(side);
    ECtor(ctor, [codegen_imm(i)]);
  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma)
  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im)
  };
}

and codegen_op = (op: Anf.bin_op): GrainIR.bin_op => {
  switch (op) {
  | OpAnd => OpAnd
  | OpOr => OpOr
  | OpPlus => OpPlus
  | OpMinus => OpMinus
  | OpTimes => OpTimes
  | OpDivide => OpDivide
  | OpLessThan => OpLessThan
  | OpGreaterThan => OpGreaterThan
  | OpEquals => OpEquals
  | OpFPlus => OpFPlus
  | OpFMinus => OpFMinus
  | OpFTimes => OpFTimes
  | OpFDivide => OpFDivide
  | OpFLessThan => OpFLessThan
  | OpFGreaterThan => OpFGreaterThan
  | OpFEquals => OpFEquals
  };
}

and codegen_imm = (i: Anf.imm): GrainIR.expr => {
  switch (i.imm_kind) {
  | IConst(const) => codegen_const(const)
  | IVar(x) => codegen_var(x)
  };
}

and codegen_var = (x: Var.t): GrainIR.expr => EVar(x)

and codegen_const = (const: Anf.constant): GrainIR.expr => {
  switch (const) {
  | ConstInt(n) => EIntLit(n)
  | ConstFloat(f) => EFloatLit(f)
  | ConstBool(b) => EBoolLit(b)
  | ConstNil => EList([])
  | ConstTriv => ETriv
  };
}

and codegen_pat = (p: Anf.pat): GrainIR.pat => {
  switch (p) {
  | PWild => PWild
  | PVar(var) => PVar(var)
  | PInt(i) => PInt(i)
  | PFloat(f) => PFloat(f)
  | PBool(b) => PBool(b)
  | PNil => PNil
  | PInj(side, p) => PCtor(codegen_inj_side(side), [codegen_pat(p)])
  | PCons(p1, p2) => PCons(codegen_pat(p1), codegen_pat(p2))
  | PPair(p1, p2) => PTuple([codegen_pat(p1), codegen_pat(p2)])
  | PTriv => PTriv
  };
}

and codegen_inj_side = (side: Anf.inj_side): Var.t => {
  switch (side) {
  | CInjL => injl_ctor
  | CInjR => injr_ctor
  };
}

and codegen_hole_reason = (reason: ErrStatus.HoleReason.t): GrainIR.expr =>
  HazelStd.Ast.(
    switch (reason) {
    | TypeInconsistent => EVar(HoleReason.type_inconsistent)
    | WrongLength => EVar(HoleReason.wrong_length)
    }
  )
and codegen_meta_var = (u: MetaVar.t): GrainIR.expr => EIntLit(u)
and codegen_meta_var_inst = (i: MetaVarInst.t): GrainIR.expr => EIntLit(i)
and codegen_sigma = (sigma: VarMap.t_(Anf.comp)): GrainIR.expr => {
  let sigma =
    sigma
    |> List.map(((x, c)) =>
         GrainIR.ETuple([GrainIR.EStringLit(x), codegen_comp(c)])
       );

  GrainIR.EList(sigma) |> GrainStd.Map.from_list;
}

and codegen_empty_hole = (u, i, sigma): GrainIR.expr => {
  let u = codegen_meta_var(u);
  let i = codegen_meta_var_inst(i);
  let sigma = codegen_sigma(sigma);

  HazelStd.Ast.empty_hole(u, i, sigma);
}

and codegen_non_empty_hole = (reason, u, i, sigma, im): GrainIR.expr => {
  let reason = codegen_hole_reason(reason);
  let u = codegen_meta_var(u);
  let i = codegen_meta_var_inst(i);
  let sigma = codegen_sigma(sigma);
  let e = codegen_imm(im);

  HazelStd.Ast.non_empty_hole(reason, u, i, sigma, e);
};

let codegen = (prog: Anf.prog): GrainIR.prog => {
  // TODO: Add necessary top-level statments.
  let tb = [];
  let b = codegen_prog(prog);
  (tb, b);
};
