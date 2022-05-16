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

  let add_hole_imports = imps =>
    HazelStd.Rt.(union(of_list([Ast.import, AstSexp.import]), imps));

  let add_sum_imports = imps => HazelStd.Rt.(add(Sum.import, imps));
};

let codegen_fold = (codegen_f, xs, imps) => {
  List.fold_left(
    ((xs, imps), x) => {
      let (x, imps) = codegen_f(x, imps);
      (xs @ [x], imps);
    },
    ([], imps),
    xs,
  );
};

let rec codegen_prog =
        ({prog_body: (body, c)}: Anf.prog, imps)
        : (GrainIR.block, GrainIR.expr, Imports.t) => {
  let (stmts, imps) = codegen_fold(codegen_stmt, body, imps);
  let (c, imps) = codegen_comp(c, imps);
  (stmts, c, imps);
}

and codegen_stmt = (stmt: Anf.stmt, imps): (GrainIR.stmt, Imports.t) => {
  switch (stmt.stmt_kind) {
  | SLet(p, NoRec, c) =>
    let (p, imps) = codegen_pat(p, imps);
    let (c, imps) = codegen_comp(c, imps);
    (SLet([p], c), imps);

  | SLet(p, Rec, c) =>
    let (p, imps) = codegen_pat(p, imps);
    let (c, imps) = codegen_comp(c, imps);
    (SLetRec([p], c), imps);
  };
}

and codegen_comp = (c: Anf.comp, imps): (GrainIR.expr, Imports.t) => {
  switch (c.comp_kind) {
  | CImm(im) => codegen_imm(im, imps)

  | CBinOp(op, im1, im2) =>
    let (op, imps) = codegen_op(op, imps);
    let (im1, imps) = codegen_imm(im1, imps);
    let (im2, imps) = codegen_imm(im2, imps);
    (EBinOp(op, im1, im2), imps);

  | CAp(fn, args) =>
    let (fn, imps) = codegen_imm(fn, imps);
    let (args, imps) = codegen_fold(codegen_imm, args, imps);
    (EAp(fn, args), imps);

  | CLam(params, body) =>
    let (params, imps) = codegen_fold(codegen_pat, params, imps);
    let (body, c, imps) = codegen_prog(body, imps);
    let body = body @ [SExpr(c)];
    (ELam(params, EBlock(body)), imps);

  | CCons(im1, im2) =>
    let (im1, imps) = codegen_imm(im1, imps);
    let (im2, imps) = codegen_imm(im2, imps);
    (ECons(im1, im2), imps);

  | CPair(im1, im2) =>
    let (im1, imps) = codegen_imm(im1, imps);
    let (im2, imps) = codegen_imm(im2, imps);
    (ETuple([im1, im2]), imps);

  | CInj(side, im) =>
    let (ctor, imps) = codegen_inj_side(side, imps);
    let (im, imps) = codegen_imm(im, imps);
    (ctor(im), imps);

  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma, imps)

  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im, imps)

  | CCast(im, t1, t2) => codegen_cast(im, t1, t2, imps)
  };
}

and codegen_op = (op: Anf.bin_op, imps): (GrainIR.bin_op, Imports.t) => {
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
  (op, imps);
}

and codegen_imm = (im: Anf.imm, imps): (GrainIR.expr, Imports.t) => {
  switch (im.imm_kind) {
  | IConst(const) => codegen_const(const, imps)

  | IVar(x) => codegen_var(x, imps)
  };
}

and codegen_var = (x: Var.t, imps): (GrainIR.expr, Imports.t) => {
  (EVar(x), imps);
}

and codegen_const = (const: Anf.constant, imps): (GrainIR.expr, Imports.t) => {
  let const: GrainIR.expr =
    switch (const) {
    | ConstInt(n) => EIntLit(n)
    | ConstFloat(f) => EFloatLit(f)
    | ConstBool(b) => EBoolLit(b)
    | ConstNil => EList([])
    | ConstTriv => ETriv
    };
  (const, imps);
}

and codegen_pat = (p: Anf.pat, imps): (GrainIR.pat, Imports.t) => {
  switch (p) {
  | PWild => (PWild, imps)
  | PVar(x) => (PVar(x), imps)
  | PInt(n) => (PInt(n), imps)
  | PFloat(f) => (PFloat(f), imps)
  | PBool(b) => (PBool(b), imps)
  | PNil => (PNil, imps)

  | PInj(side, p) =>
    let (ctor, imps) = codegen_inj_side_pat(side, imps);
    let (p, imps) = codegen_pat(p, imps);
    (ctor(p), imps);

  | PCons(p1, p2) =>
    let (p1, imps) = codegen_pat(p1, imps);
    let (p2, imps) = codegen_pat(p2, imps);
    (PCons(p1, p2), imps);

  | PPair(p1, p2) =>
    let (p1, imps) = codegen_pat(p1, imps);
    let (p2, imps) = codegen_pat(p2, imps);
    (PTuple([p1, p2]), imps);

  | PTriv => (PTriv, imps)
  };
}

and codegen_inj_side =
    (side: Anf.inj_side, imps): (GrainIR.expr => GrainIR.expr, Imports.t) => {
  let side =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l
    | CInjR => HazelStd.Rt.Sum.inj_r
    };

  (side, Imports.add_sum_imports(imps));
}

and codegen_inj_side_pat =
    (side: Anf.inj_side, imps): (GrainIR.pat => GrainIR.pat, Imports.t) => {
  let side =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l_pat
    | CInjR => HazelStd.Rt.Sum.inj_r_pat
    };

  (side, Imports.add_sum_imports(imps));
}

and codegen_hole_reason =
    (reason: ErrStatus.HoleReason.t, imps): (GrainIR.expr, Imports.t) => {
  let reason =
    HazelStd.Rt.Ast.(
      switch (reason) {
      | TypeInconsistent => HoleReason.type_inconsistent
      | WrongLength => HoleReason.wrong_length
      }
    );
  (EVar(reason), imps);
}
and codegen_meta_var = (u: MetaVar.t, imps): (GrainIR.expr, Imports.t) => {
  (EIntLit(u), imps);
}
and codegen_meta_var_inst =
    (i: MetaVarInst.t, imps): (GrainIR.expr, Imports.t) => {
  (EIntLit(i), imps);
}
and codegen_sigma =
    (sigma: VarMap.t_(Anf.comp), imps): (GrainIR.expr, Imports.t) => {
  let (sigma, imps) =
    codegen_fold(
      ((x, c), imps) => {
        let (c, imps) = codegen_comp(c, imps);
        (GrainIR.ETuple([GrainIR.EStringLit(x), c]), imps);
      },
      sigma,
      imps,
    );

  (GrainIR.EList(sigma) |> GrainStd.Map.from_list, imps);
}

and codegen_empty_hole = (u, i, sigma, imps): (GrainIR.expr, Imports.t) => {
  let (u, imps) = codegen_meta_var(u, imps);
  let (i, imps) = codegen_meta_var_inst(i, imps);
  let (sigma, imps) = codegen_sigma(sigma, imps);

  HazelStd.Rt.(Ast.empty_hole(u, i, sigma), Imports.add_hole_imports(imps));
}

and codegen_non_empty_hole =
    (reason, u, i, sigma, im, imps): (GrainIR.expr, Imports.t) => {
  let (reason, imps) = codegen_hole_reason(reason, imps);
  let (u, imps) = codegen_meta_var(u, imps);
  let (i, imps) = codegen_meta_var_inst(i, imps);
  let (sigma, imps) = codegen_sigma(sigma, imps);
  let (e, imps) = codegen_imm(im, imps);

  HazelStd.Rt.(
    Ast.non_empty_hole(reason, u, i, sigma, e),
    Imports.add_hole_imports(imps),
  );
}

and codegen_cast =
    (im: Anf.imm, t1: HTyp.t, t2: HTyp.t, imps): (GrainIR.expr, Imports.t) => {
  let (im, imps) = codegen_imm(im, imps);
  let (t1, imps) = codegen_htyp(t1, imps);
  let (t2, imps) = codegen_htyp(t2, imps);

  HazelStd.Rt.(Ast.cast(im, t1, t2), Imports.add_hole_imports(imps));
}

and codegen_htyp = (t: HTyp.t, imps): (GrainIR.expr, Imports.t) => {
  HazelStd.Rt.(
    switch (t) {
    | Hole => (Ast.HTyp.hole, imps)
    | Int => (Ast.HTyp.int, imps)
    | Float => (Ast.HTyp.float, imps)
    | Bool => (Ast.HTyp.bool, imps)

    | Arrow(t1, t2) =>
      let (t1, imps) = codegen_htyp(t1, imps);
      let (t2, imps) = codegen_htyp(t2, imps);
      (Ast.HTyp.arrow(t1, t2), imps);

    | Sum(t1, t2) =>
      let (t1, imps) = codegen_htyp(t1, imps);
      let (t2, imps) = codegen_htyp(t2, imps);
      (Ast.HTyp.sum(t1, t2), imps);
    | Prod(ts) =>
      let (ts, imps) = codegen_fold(codegen_htyp, ts, imps);
      (Ast.HTyp.prod(ts), imps);

    | List(t) =>
      let (t, imps) = codegen_htyp(t, imps);
      (Ast.HTyp.list(t), imps);
    }
  );
};

let codegen_imps = (imps): GrainIR.top_block => {
  imps
  |> Imports.elements
  |> List.map(((x, path)) => GrainIR.TSImport(x, path));
};

let codegen = (prog: Anf.prog): GrainIR.prog => {
  let (body, c, imps) = codegen_prog(prog, Imports.empty);
  let tb = codegen_imps(imps);

  // TODO: This is a stopgap solution
  let b = body @ [SExpr(EAp(EVar("print"), [c]))];

  (tb, b);
};
