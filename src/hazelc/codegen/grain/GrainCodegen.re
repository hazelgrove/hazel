open Sexplib.Std;

[@deriving sexp]
type opts = {print_final_expr: bool};

module Imports = {
  type t = {
    indet: bool,
    sum: bool,
    int32: bool,
    int64: bool,
    float32: bool,
    float64: bool,
  };

  let default = {
    indet: false,
    sum: false,
    int32: false,
    int64: false,
    float32: false,
    float64: false,
  };

  let with_complete = imps => {...imps, indet: true};
  let with_sum = imps => {...imps, sum: true};
  /* let with_int32 = imps => {...imps, int32: true}; */
  let with_int32 = imps => {...imps, int32: true};
  /* let with_float32 = imps => {...imps, float32: true}; */
  let with_float32 = imps => {...imps, float32: true};

  /*
     Generate GrainIR from the imports specification.
   */
  let codegen = imps => {
    open HazelStd.Rt;

    let add = (flag, imps', imps) =>
      if (flag) {
        imps' @ imps;
      } else {
        imps;
      };
    let {indet, sum, int32, int64, float32, float64}: t = imps;

    let imps =
      []
      |> add(
           indet,
           [
             Ast.import,
             AstMk.import,
             AstOps.import,
             AstPrint.import,
             AstSexp.import,
             GrainStd.Map.import,
           ],
         )
      |> add(sum, [Sum.import])
      |> add(int32, [GrainStd.Int32.import])
      |> add(int64, [GrainStd.Int32.import])
      |> add(float32, [GrainStd.Float32.import])
      |> add(float64, [GrainStd.Float32.import]);

    imps |> List.map(((x, path)) => GrainIR.TSImport(x, path));
  };
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
        (
          {prog_body: (body, c), prog_ty: _, prog_complete: _}: Anf.prog,
          imps,
        )
        : (GrainIR.block, GrainIR.expr, Imports.t) => {
  let (stmts, imps) = codegen_fold(codegen_stmt, body, imps);
  let (c, imps) = codegen_comp(c, imps);
  (stmts, c, imps);
}

and codegen_stmt = (stmt: Anf.stmt, imps): (GrainIR.stmt, Imports.t) => {
  switch (stmt.stmt_kind) {
  | SLet(p, c) =>
    let (p', imps) = codegen_pat(p, imps);
    let (c', imps) = codegen_comp(c, imps);
    (SLet([p'], c'), imps);

  | SLetRec(x, c) =>
    let (p', imps) =
      codegen_pat(
        {pat_kind: PVar(x), pat_complete: NecessarilyComplete},
        imps,
      );
    let (c', imps) = codegen_comp(c, imps);
    (SLetRec([p'], c'), imps);
  };
}

and codegen_comp = (c: Anf.comp, imps): (GrainIR.expr, Imports.t) => {
  switch (c.comp_kind) {
  | CImm(im) => codegen_imm(im, imps)

  | CBinOp(op, im1, im2) =>
    codegen_bin_op(op, im1, im2, c.comp_complete, imps)

  | CAp(fn, args) =>
    let (fn', imps) = codegen_imm(fn, imps);
    let (args', imps) = codegen_fold(codegen_imm, args, imps);
    (EAp(fn', args'), imps);

  | CLam(param, body) =>
    let (param', imps) = codegen_pat(param, imps);
    let (body', c, imps) = codegen_prog(body, imps);
    let body' = body' @ [SExpr(c)];
    (ELam([param'], EBlock(body')), imps);

  | CCons(im1, im2) =>
    let (e1, imps) = codegen_imm(im1, imps);
    let (e2, imps) = codegen_imm(im2, imps);
    (ECons(e1, e2), imps);

  | CPair(im1, im2) =>
    let (e1, imps) = codegen_imm(im1, imps);
    let (e2, imps) = codegen_imm(im2, imps);
    (ETuple([e1, e2]), imps);

  | CInj(side, im) =>
    let (ctor, imps) = codegen_inj_side(side, imps);
    let (e, imps) = codegen_imm(im, imps);
    (ctor(e), imps);

  | CCase(scrut, rules) =>
    let (scrut, imps) = codegen_imm(scrut, imps);
    let (rules, imps) = codegen_rules(rules, imps);
    (EMatch(scrut, rules), imps);

  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma, imps)

  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im, imps)

  | CCast(im, t1, t2) => codegen_cast(im, t1, t2, imps)
  };
}

and codegen_bin_op_complete = (op: Anf.bin_op, imps) => {
  let (op, with_int32, with_float32) =
    GrainStd.(
      switch (op) {
      | OpAnd => (((e1, e2) => GrainIR.EBinOp(OpAnd, e1, e2)), false, false)
      | OpOr => (((e1, e2) => GrainIR.EBinOp(OpOr, e1, e2)), false, false)
      | OpPlus => (Int32.add, true, false)
      | OpMinus => (Int32.sub, true, false)
      | OpTimes => (Int32.mul, true, false)
      | OpDivide => (Int32.div, true, false)
      | OpLessThan => (Int32.lt, true, false)
      | OpGreaterThan => (Int32.gt, true, false)
      | OpEquals => (Int32.eq, true, false)
      | OpFPlus => (Float32.add, false, true)
      | OpFMinus => (Float32.sub, false, true)
      | OpFTimes => (Float32.mul, false, true)
      | OpFDivide => (Float32.div, false, true)
      | OpFLessThan => (Float32.lt, false, true)
      | OpFGreaterThan => (Float32.gt, false, true)
      | OpFEquals => (Float32.eq, false, true)
      }
    );

  let imps =
    if (with_int32) {
      Imports.with_int32(imps);
    } else {
      imps;
    };
  let imps =
    if (with_float32) {
      Imports.with_float32(imps);
    } else {
      imps;
    };
  (op, imps);
}

and codegen_bin_op_incomplete = (op: Anf.bin_op, imps) => {
  let op =
    HazelStd.Rt.(
      switch (op) {
      | OpAnd => AstOps.indet_and
      | OpOr => AstOps.indet_or
      | OpPlus => AstOps.indet_plus
      | OpMinus => AstOps.indet_minus
      | OpTimes => AstOps.indet_times
      | OpDivide => AstOps.indet_divide
      | OpLessThan => AstOps.indet_less_than
      | OpGreaterThan => AstOps.indet_greater_than
      | OpEquals => AstOps.indet_equals
      | OpFPlus => AstOps.indet_fplus
      | OpFMinus => AstOps.indet_fminus
      | OpFTimes => AstOps.indet_ftimes
      | OpFDivide => AstOps.indet_fdivide
      | OpFLessThan => AstOps.indet_fless_than
      | OpFGreaterThan => AstOps.indet_fgreater_than
      | OpFEquals => AstOps.indet_fequals
      }
    );
  (op, Imports.with_complete(imps));
}

and codegen_bin_op =
    (
      op: Anf.bin_op,
      im1: Anf.imm,
      im2: Anf.imm,
      indet: Anf.completeness,
      imps,
    )
    : (GrainIR.expr, Imports.t) => {
  let (e1, imps) = codegen_imm(im1, imps);
  let (e2, imps) = codegen_imm(im2, imps);
  let (op, imps) =
    switch (indet) {
    // TODO: Separate cases
    | NecessarilyComplete => codegen_bin_op_complete(op, imps)
    | NecessarilyIncomplete
    | IndeterminatelyIncomplete => codegen_bin_op_incomplete(op, imps)
    };
  (op(e1, e2), imps);
}

and codegen_rules =
    (rules: list(Anf.rule), imps): (list(GrainIR.rule), Imports.t) => {
  codegen_fold(codegen_rule, rules, imps);
}

and codegen_rule = (rule: Anf.rule, imps): (GrainIR.rule, Imports.t) => {
  let (pat, imps) = codegen_pat(rule.rule_pat, imps);
  let (branch_stmts, branch_expr, imps) =
    codegen_prog(rule.rule_branch, imps);
  let branch = branch_stmts @ [SExpr(branch_expr)];
  (RRule(pat, EBlock(branch)), imps);
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
  switch (const) {
  | ConstInt(n) => (EInt32Lit(n), imps)
  | ConstFloat(f) => (EFloat32Lit(f), imps)
  | ConstBool(b) => (EBoolLit(b), imps)
  | ConstNil(_) => (EList([]), imps)
  | ConstTriv => (ETriv, imps)
  };
}

and codegen_pat = (p: Anf.pat, imps): (GrainIR.pat, Imports.t) => {
  switch (p.pat_kind) {
  | PWild => (PWild, imps)
  | PVar(x) => (PVar(x), imps)
  | PInt(n) => (PInt(n), imps)
  | PFloat(f) => (PFloat(f), imps)
  | PBool(b) => (PBool(b), imps)
  | PNil => (PNil, imps)

  | PInj(side, p) =>
    let (ctor, imps) = codegen_inj_side_pat(side, imps);
    let (p', imps) = codegen_pat(p, imps);
    (ctor(p'), imps);

  | PCons(p1, p2) =>
    let (p1', imps) = codegen_pat(p1, imps);
    let (p2', imps) = codegen_pat(p2, imps);
    (PCons(p1', p2'), imps);

  | PPair(p1, p2) =>
    let (p1', imps) = codegen_pat(p1, imps);
    let (p2', imps) = codegen_pat(p2, imps);
    (PTuple([p1', p2']), imps);

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

  (side, Imports.with_sum(imps));
}

and codegen_inj_side_pat =
    (side: Anf.inj_side, imps): (GrainIR.pat => GrainIR.pat, Imports.t) => {
  let side' =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l_pat
    | CInjR => HazelStd.Rt.Sum.inj_r_pat
    };

  (side', Imports.with_sum(imps));
}

and codegen_hole_reason =
    (reason: ErrStatus.HoleReason.t, imps): (GrainIR.expr, Imports.t) => {
  let reason' =
    HazelStd.Rt.Ast.(
      switch (reason) {
      | TypeInconsistent => HoleReason.type_inconsistent
      | WrongLength => HoleReason.wrong_length
      }
    );
  (EVar(reason'), imps);
}
and codegen_meta_var = (u: MetaVar.t, imps): (GrainIR.expr, Imports.t) => {
  (EInt32Lit(u), imps);
}
and codegen_meta_var_inst =
    (i: MetaVarInst.t, imps): (GrainIR.expr, Imports.t) => {
  (EInt32Lit(i), imps);
}
and codegen_sigma =
    (sigma: VarMap.t_(Anf.comp), imps): (GrainIR.expr, Imports.t) => {
  let (sigma', imps) =
    codegen_fold(
      ((x, c), imps) => {
        let (c', imps) = codegen_comp(c, imps);
        (GrainIR.ETuple([GrainIR.EStringLit(x), c']), imps);
      },
      sigma,
      imps,
    );

  (GrainIR.EList(sigma') |> GrainStd.Map.from_list, imps);
}

and codegen_empty_hole = (u, i, sigma, imps): (GrainIR.expr, Imports.t) => {
  let (u', imps) = codegen_meta_var(u, imps);
  let (i', imps) = codegen_meta_var_inst(i, imps);
  let (sigma', imps) = codegen_sigma(sigma, imps);

  HazelStd.Rt.(Ast.empty_hole(u', i', sigma'), Imports.with_complete(imps));
}

and codegen_non_empty_hole =
    (reason, u, i, sigma, im, imps): (GrainIR.expr, Imports.t) => {
  let (reason', imps) = codegen_hole_reason(reason, imps);
  let (u', imps) = codegen_meta_var(u, imps);
  let (i', imps) = codegen_meta_var_inst(i, imps);
  let (sigma', imps) = codegen_sigma(sigma, imps);
  let (e', imps) = codegen_imm(im, imps);

  HazelStd.Rt.(
    Ast.non_empty_hole(reason', u', i', sigma', e'),
    Imports.with_complete(imps),
  );
}

and codegen_cast =
    (im: Anf.imm, ty1: HTyp.t, ty2: HTyp.t, imps): (GrainIR.expr, Imports.t) => {
  let rec codegen_htyp = (t: HTyp.t): GrainIR.expr => {
    HazelStd.Rt.(
      switch (t) {
      | Hole => Ast.HTyp.hole
      | Int => Ast.HTyp.int
      | Float => Ast.HTyp.float
      | Bool => Ast.HTyp.bool

      | Arrow(ty1, ty2) =>
        let ty1' = codegen_htyp(ty1);
        let ty2' = codegen_htyp(ty2);
        Ast.HTyp.arrow(ty1', ty2');

      | Sum(ty1, ty2) =>
        let ty1' = codegen_htyp(ty1);
        let ty2' = codegen_htyp(ty2);
        Ast.HTyp.sum(ty1', ty2');
      | Prod(tys) =>
        let tys' = tys |> List.map(codegen_htyp);
        Ast.HTyp.prod(tys');

      | List(ty) =>
        let ty' = codegen_htyp(ty);
        Ast.HTyp.list(ty');
      }
    );
  };

  let (e, imps) = codegen_imm(im, imps);
  let ty1' = codegen_htyp(ty1);
  let ty2' = codegen_htyp(ty2);

  HazelStd.Rt.(Ast.cast(e, ty1', ty2'), Imports.with_complete(imps));
};

let codegen = (~opts, prog: Anf.prog): GrainIR.prog => {
  let (body, c, imps) = codegen_prog(prog, Imports.default);
  let tb = Imports.codegen(imps);

  let b =
    if (opts.print_final_expr) {
      /* TODO: Clean this up. */
      /* FIXME: Probably doesn't work all the time. */
      let print_ap: GrainIR.expr =
        switch (prog.prog_complete) {
        | NecessarilyComplete => EAp(EVar("print"), [c])
        | NecessarilyIncomplete
        | IndeterminatelyIncomplete => HazelStd.Rt.AstPrint.print(c)
        };
      body @ [SExpr(print_ap)];
    } else {
      body @ [SExpr(c)];
    };

  (tb, b);
};
