module Import = {
  type t = (Var.t, GrainIR.import_path);

  let compare = ((x, path), (x', path')) =>
    switch (String.compare(x, x')) {
    | 0 =>
      GrainIR.(
        switch (path, path') {
        | (ImportStd(path), ImportStd(path')) =>
          String.compare(path, path')
        | (ImportRel(path), ImportRel(path')) =>
          String.compare(path, path')
        | (ImportStd(_), ImportRel(_)) => (-1)
        | (ImportRel(_), ImportStd(_)) => 1
        }
      )
    | c => c
    };
};

module Imports = Set.Make(Import);

let codegen_fold = (codegen_f, xs, imports: Imports.t) => {
  List.fold_left(
    ((xs, imports), x) => {
      let (x, imports) = codegen_f(x, imports);
      (xs @ [x], imports);
    },
    ([], imports),
    xs,
  );
};

let rec codegen_prog =
        ({prog_body: (stmts, c)}: Anf.prog, imports: Imports.t)
        : (GrainIR.block, Imports.t) => {
  let (stmts, imports) = codegen_fold(codegen_stmt, stmts, imports);
  let (c, imports) = codegen_comp(c, imports);
  (stmts @ [SExpr(c)], imports);
}

and codegen_stmt =
    (stmt: Anf.stmt, imports: Imports.t): (GrainIR.stmt, Imports.t) => {
  switch (stmt.stmt_kind) {
  | SLet(p, NoRec, c) =>
    let (p, imports) = codegen_pat(p, imports);
    let (c, imports) = codegen_comp(c, imports);
    (SLet([p], c), imports);

  | SLet(p, Rec, c) =>
    let (p, imports) = codegen_pat(p, imports);
    let (c, imports) = codegen_comp(c, imports);
    (SLetRec([p], c), imports);
  };
}

and codegen_comp =
    (c: Anf.comp, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  switch (c.comp_kind) {
  | CImm(i) => codegen_imm(i, imports)

  | CBinOp(op, i1, i2) =>
    let (op, imports) = codegen_op(op, imports);
    let (i1, imports) = codegen_imm(i1, imports);
    let (i2, imports) = codegen_imm(i2, imports);
    (EBinOp(op, i1, i2), imports);

  | CAp(fn, args) =>
    let (fn, imports) = codegen_imm(fn, imports);
    let (args, imports) = codegen_fold(codegen_imm, args, imports);
    (EAp(fn, args), imports);

  | CLam(params, body) =>
    let (params, imports) = codegen_fold(codegen_pat, params, imports);
    let (body, imports) = codegen_prog(body, imports);
    (ELam(params, EBlock(body)), imports);

  | CCons(i1, i2) =>
    let (i1, imports) = codegen_imm(i1, imports);
    let (i2, imports) = codegen_imm(i2, imports);
    (ECons(i1, i2), imports);

  | CPair(i1, i2) =>
    let (i1, imports) = codegen_imm(i1, imports);
    let (i2, imports) = codegen_imm(i2, imports);
    (ETuple([i1, i2]), imports);

  | CInj(side, i) =>
    let (ctor, imports) = codegen_inj_side(side, imports);
    let (i, imports) = codegen_imm(i, imports);
    (ctor(i), imports);

  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma, imports)

  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im, imports)
  };
}

and codegen_op =
    (op: Anf.bin_op, imports: Imports.t): (GrainIR.bin_op, Imports.t) => {
  let op: GrainIR.bin_op =
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
  (op, imports);
}

and codegen_imm = (i: Anf.imm, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  switch (i.imm_kind) {
  | IConst(const) => codegen_const(const, imports)

  | IVar(x) => codegen_var(x, imports)
  };
}

and codegen_var = (x: Var.t, imports: Imports.t): (GrainIR.expr, Imports.t) => (
  EVar(x),
  imports,
)

and codegen_const =
    (const: Anf.constant, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  let const: GrainIR.expr =
    switch (const) {
    | ConstInt(n) => EIntLit(n)
    | ConstFloat(f) => EFloatLit(f)
    | ConstBool(b) => EBoolLit(b)
    | ConstNil => EList([])
    | ConstTriv => ETriv
    };
  (const, imports);
}

and codegen_pat = (p: Anf.pat, imports: Imports.t): (GrainIR.pat, Imports.t) => {
  switch (p) {
  | PWild => (PWild, imports)
  | PVar(var) => (PVar(var), imports)
  | PInt(i) => (PInt(i), imports)
  | PFloat(f) => (PFloat(f), imports)
  | PBool(b) => (PBool(b), imports)
  | PNil => (PNil, imports)

  | PInj(side, p) =>
    let (ctor, imports) = codegen_inj_side_pat(side, imports);
    let (p, imports) = codegen_pat(p, imports);
    (ctor(p), imports);

  | PCons(p1, p2) =>
    let (p1, imports) = codegen_pat(p1, imports);
    let (p2, imports) = codegen_pat(p2, imports);
    (PCons(p1, p2), imports);

  | PPair(p1, p2) =>
    let (p1, imports) = codegen_pat(p1, imports);
    let (p2, imports) = codegen_pat(p2, imports);
    (PTuple([p1, p2]), imports);

  | PTriv => (PTriv, imports)
  };
}

and codegen_inj_side =
    (side: Anf.inj_side, imports: Imports.t)
    : (GrainIR.expr => GrainIR.expr, Imports.t) => {
  let side =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l
    | CInjR => HazelStd.Rt.Sum.inj_r
    };

  (side, Imports.add(HazelStd.Rt.Sum.import, imports));
}

and codegen_inj_side_pat =
    (side: Anf.inj_side, imports: Imports.t)
    : (GrainIR.pat => GrainIR.pat, Imports.t) => {
  let side =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l_pat
    | CInjR => HazelStd.Rt.Sum.inj_r_pat
    };

  (side, Imports.add(HazelStd.Rt.Ast.import, imports));
}

and codegen_hole_reason =
    (reason: ErrStatus.HoleReason.t, imports: Imports.t)
    : (GrainIR.expr, Imports.t) => {
  let reason =
    HazelStd.Rt.Ast.(
      switch (reason) {
      | TypeInconsistent => HoleReason.type_inconsistent
      | WrongLength => HoleReason.wrong_length
      }
    );
  (EVar(reason), imports);
}
and codegen_meta_var =
    (u: MetaVar.t, imports: Imports.t): (GrainIR.expr, Imports.t) => (
  EIntLit(u),
  imports,
)
and codegen_meta_var_inst =
    (i: MetaVarInst.t, imports: Imports.t): (GrainIR.expr, Imports.t) => (
  EIntLit(i),
  imports,
)
and codegen_sigma =
    (sigma: VarMap.t_(Anf.comp), imports: Imports.t)
    : (GrainIR.expr, Imports.t) => {
  let (sigma, imports) =
    codegen_fold(
      ((x, c), imports) => {
        let (c, imports) = codegen_comp(c, imports);
        (GrainIR.ETuple([GrainIR.EStringLit(x), c]), imports);
      },
      sigma,
      imports,
    );

  (GrainIR.EList(sigma) |> GrainStd.Map.from_list, imports);
}

and codegen_empty_hole = (u, i, sigma, imports): (GrainIR.expr, Imports.t) => {
  let (u, imports) = codegen_meta_var(u, imports);
  let (i, imports) = codegen_meta_var_inst(i, imports);
  let (sigma, imports) = codegen_sigma(sigma, imports);

  HazelStd.Rt.(
    Ast.empty_hole(u, i, sigma),
    Imports.union(Imports.of_list([Ast.import, AstSexp.import]), imports),
  );
}

and codegen_non_empty_hole =
    (reason, u, i, sigma, im, imports): (GrainIR.expr, Imports.t) => {
  let (reason, imports) = codegen_hole_reason(reason, imports);
  let (u, imports) = codegen_meta_var(u, imports);
  let (i, imports) = codegen_meta_var_inst(i, imports);
  let (sigma, imports) = codegen_sigma(sigma, imports);
  let (e, imports) = codegen_imm(im, imports);

  HazelStd.Rt.(
    Ast.non_empty_hole(reason, u, i, sigma, e),
    Imports.union(Imports.of_list([Ast.import, AstSexp.import]), imports),
  );
};

let codegen_imports = (imports: Imports.t): GrainIR.top_block => {
  imports
  |> Imports.elements
  |> List.map(((x, path)) => GrainIR.TSImport(x, path));
};

let codegen = (prog: Anf.prog): GrainIR.prog => {
  // TODO: Generate code to print final result
  let (b, imports) = codegen_prog(prog, Imports.empty);
  let tb = codegen_imports(imports);

  (tb, b);
};
