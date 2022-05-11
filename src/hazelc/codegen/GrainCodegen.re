let injl_ctor = "L";
let injr_ctor = "R";

let rec codegen_prog = ({prog_body: (stmts, c)}: Anf.prog): GrainIR.block => {
  let stmts = stmts |> List.map(codegen_stmt);
  let c = codegen_comp(c);
  stmts @ [Expr(c)];
}

and codegen_stmt = (stmt: Anf.stmt): GrainIR.statement => {
  switch (stmt.stmt_kind) {
  | SLet(x, NoRec, c) => Let([x], codegen_comp(c))
  | SLet(x, Rec, c) => LetRec([x], codegen_comp(c))
  };
}

and codegen_comp = (c: Anf.comp): GrainIR.expr => {
  switch (c.comp_kind) {
  | CImm(i) => codegen_imm(i)
  | CBinOp(op, i1, i2) =>
    BinOp(codegen_op(op), codegen_imm(i1), codegen_imm(i2))
  | CAp(fn, args) =>
    let args = args |> List.map(codegen_imm);
    Ap(codegen_imm(fn), args);
  | CLam(params, body) => Lam(params, Block(codegen_prog(body)))
  | CCons(i1, i2) => Cons(codegen_imm(i1), codegen_imm(i2))
  | CPair(i1, i2) => Tuple([codegen_imm(i1), codegen_imm(i2)])
  | CInj(side, i) =>
    let ctor =
      switch (side) {
      | CInjL => injl_ctor
      | CInjR => injr_ctor
      };
    Ctor(ctor, [codegen_imm(i)]);
  };
}

and codegen_op = (op: Anf.bin_op): GrainIR.bin_op => {
  switch (op) {
  | OpAnd => And
  | OpOr => Or
  | OpPlus => Plus
  | OpMinus => Minus
  | OpTimes => Times
  | OpDivide => Divide
  | OpLessThan => LessThan
  | OpGreaterThan => GreaterThan
  | OpEquals => Equals
  | OpFPlus => FPlus
  | OpFMinus => FMinus
  | OpFTimes => FTimes
  | OpFDivide => FDivide
  | OpFLessThan => FLessThan
  | OpFGreaterThan => FGreaterThan
  | OpFEquals => FEquals
  };
}

and codegen_imm = (i: Anf.imm): GrainIR.expr => {
  switch (i.imm_kind) {
  | IConst(const) => codegen_const(const)
  | IVar(var) => Var(var)
  };
}

and codegen_const = (const: Anf.constant): GrainIR.expr => {
  switch (const) {
  | ConstInt(n) => IntLit(n)
  | ConstFloat(f) => FloatLit(f)
  | ConstBool(b) => BoolLit(b)
  | ConstNil => List([])
  | ConstTriv => Triv
  };
};

let codegen = (prog: Anf.prog): GrainIR.prog => {
  // TODO: Add necessary top-level statments.
  let tb = [];
  let b = codegen_prog(prog);
  (tb, b);
};
