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

module Imports = {
  include Set.Make(Import);

  let add_hole_imports = (imports: t): t =>
    HazelStd.Rt.(union(of_list([Ast.import, AstSexp.import]), imports));
};

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
        ({prog_body: (body, c)}: Anf.prog, imports: Imports.t)
        : (GrainIR.block, Imports.t) => {
  let (stmts, imports) = codegen_fold(codegen_stmt, body, imports);
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
  | CImm(im) => codegen_imm(im, imports)

  | CBinOp(op, im1, im2) =>
    let (op, imports) = codegen_op(op, imports);
    let (im1, imports) = codegen_imm(im1, imports);
    let (im2, imports) = codegen_imm(im2, imports);
    (EBinOp(op, im1, im2), imports);

  | CAp(fn, args) =>
    let (fn, imports) = codegen_imm(fn, imports);
    let (args, imports) = codegen_fold(codegen_imm, args, imports);
    (EAp(fn, args), imports);

  | CLam(params, body) =>
    let (params, imports) = codegen_fold(codegen_pat, params, imports);
    let (body, imports) = codegen_prog(body, imports);
    (ELam(params, EBlock(body)), imports);

  | CCons(im1, im2) =>
    let (im1, imports) = codegen_imm(im1, imports);
    let (im2, imports) = codegen_imm(im2, imports);
    (ECons(im1, im2), imports);

  | CPair(im1, im2) =>
    let (im1, imports) = codegen_imm(im1, imports);
    let (im2, imports) = codegen_imm(im2, imports);
    (ETuple([im1, im2]), imports);

  | CInj(side, im) =>
    let (ctor, imports) = codegen_inj_side(side, imports);
    let (im, imports) = codegen_imm(im, imports);
    (ctor(im), imports);

  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma, imports)

  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im, imports)

  | CCast(im, t1, t2) => codegen_cast(im, t1, t2, imports)
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

and codegen_imm =
    (im: Anf.imm, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  switch (im.imm_kind) {
  | IConst(const) => codegen_const(const, imports)

  | IVar(x) => codegen_var(x, imports)
  };
}

and codegen_var = (x: Var.t, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  (EVar(x), imports);
}

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
  | PVar(x) => (PVar(x), imports)
  | PInt(n) => (PInt(n), imports)
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
    (u: MetaVar.t, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  (EIntLit(u), imports);
}
and codegen_meta_var_inst =
    (i: MetaVarInst.t, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  (EIntLit(i), imports);
}
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
    Imports.add_hole_imports(imports),
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
    Imports.add_hole_imports(imports),
  );
}

and codegen_cast =
    (im: Anf.imm, t1: HTyp.t, t2: HTyp.t, imports: Imports.t)
    : (GrainIR.expr, Imports.t) => {
  let (im, imports) = codegen_imm(im, imports);
  let (t1, imports) = codegen_htyp(t1, imports);
  let (t2, imports) = codegen_htyp(t2, imports);

  HazelStd.Rt.(Ast.cast(im, t1, t2), Imports.add_hole_imports(imports));
}

and codegen_htyp = (t: HTyp.t, imports: Imports.t): (GrainIR.expr, Imports.t) => {
  HazelStd.Rt.(
    switch (t) {
    | Hole => (Ast.HTyp.hole, imports)
    | Int => (Ast.HTyp.int, imports)
    | Float => (Ast.HTyp.float, imports)
    | Bool => (Ast.HTyp.bool, imports)

    | Arrow(t1, t2) =>
      let (t1, imports) = codegen_htyp(t1, imports);
      let (t2, imports) = codegen_htyp(t2, imports);
      (Ast.HTyp.arrow(t1, t2), imports);

    | Sum(t1, t2) =>
      let (t1, imports) = codegen_htyp(t1, imports);
      let (t2, imports) = codegen_htyp(t2, imports);
      (Ast.HTyp.sum(t1, t2), imports);
    | Prod(ts) =>
      let (ts, imports) = codegen_fold(codegen_htyp, ts, imports);
      (Ast.HTyp.prod(ts), imports);

    | List(t) =>
      let (t, imports) = codegen_htyp(t, imports);
      (Ast.HTyp.list(t), imports);
    }
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
