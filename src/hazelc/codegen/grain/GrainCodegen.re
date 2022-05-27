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

  let with_indet = imps => {...imps, indet: true};
  let with_sum = imps => {...imps, sum: true};
  /* let with_int32 = imps => {...imps, int32: true}; */
  let with_int64 = imps => {...imps, int64: true};
  /* let with_float32 = imps => {...imps, float32: true}; */
  let with_float64 = imps => {...imps, float64: true};

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
      |> add(int64, [GrainStd.Int64.import])
      |> add(float32, [GrainStd.Float32.import])
      |> add(float64, [GrainStd.Float64.import]);

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
        ({prog_body: (body, c), prog_ty: _, prog_indet: _}: Anf.prog, imps)
        : (GrainIR.block, GrainIR.expr, Imports.t) => {
  let (stmts, imps) = codegen_fold(codegen_stmt, body, imps);
  let (c, imps) = codegen_comp(c, imps);
  (stmts, c, imps);
}

and codegen_stmt = (stmt: Anf.stmt, imps): (GrainIR.stmt, Imports.t) => {
  switch (stmt.stmt_kind) {
  | SLet(p, NoRec, c) =>
    let (p', imps) = codegen_pat(p, imps);
    let (c', imps) = codegen_comp(c, imps);
    (SLet([p'], c'), imps);

  | SLet(p, Rec, c) =>
    let (p', imps) = codegen_pat(p, imps);
    let (c', imps) = codegen_comp(c, imps);
    (SLetRec([p'], c'), imps);
  };
}

and codegen_comp = (c: Anf.comp, imps): (GrainIR.expr, Imports.t) => {
  switch (c.comp_kind) {
  | CImm(im) => codegen_imm(im, imps)

  | CBinOp(op, im1, im2) => codegen_bin_op(op, im1, im2, c.comp_indet, imps)

  | CAp(fn, args) =>
    let (fn', imps) = codegen_imm(fn, imps);
    let (args', imps) = codegen_fold(codegen_imm, args, imps);
    (EAp(fn', args'), imps);

  | CLam(params, body) =>
    let (params', imps) = codegen_fold(codegen_pat, params, imps);
    let (body', c, imps) = codegen_prog(body, imps);
    let body' = body' @ [SExpr(c)];
    (ELam(params', EBlock(body')), imps);

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

  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma, imps)

  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im, imps)

  | CCast(im, t1, t2) => codegen_cast(im, t1, t2, imps)
  };
}

and codegen_bin_op_non_indet = (op: Anf.bin_op, imps) => {
  let (op, with_int64, with_float64) =
    GrainStd.(
      switch (op) {
      | OpAnd => (((e1, e2) => GrainIR.EBinOp(OpAnd, e1, e2)), false, false)
      | OpOr => (((e1, e2) => GrainIR.EBinOp(OpOr, e1, e2)), false, false)
      | OpPlus => (Int64.add, true, false)
      | OpMinus => (Int64.sub, true, false)
      | OpTimes => (Int64.mul, true, false)
      | OpDivide => (Int64.div, true, false)
      | OpLessThan => (Int64.lt, true, false)
      | OpGreaterThan => (Int64.gt, true, false)
      | OpEquals => (Int64.eq, true, false)
      | OpFPlus => (Float64.add, false, true)
      | OpFMinus => (Float64.sub, false, true)
      | OpFTimes => (Float64.mul, false, true)
      | OpFDivide => (Float64.div, false, true)
      | OpFLessThan => (Float64.lt, false, true)
      | OpFGreaterThan => (Float64.gt, false, true)
      | OpFEquals => (Float64.eq, false, true)
      }
    );

  let imps =
    if (with_int64) {
      Imports.with_int64(imps);
    } else {
      imps;
    };
  let imps =
    if (with_float64) {
      Imports.with_float64(imps);
    } else {
      imps;
    };
  (op, imps);
}

and codegen_bin_op_indet = (op: Anf.bin_op, imps) => {
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
  (op, Imports.with_indet(imps));
}

and codegen_bin_op =
    (op: Anf.bin_op, im1: Anf.imm, im2: Anf.imm, indet: Hir.has_indet, imps)
    : (GrainIR.expr, Imports.t) => {
  let (e1, imps) = codegen_imm(im1, imps);
  let (e2, imps) = codegen_imm(im2, imps);
  let (op, imps) =
    if (!indet) {
      codegen_bin_op_non_indet(op, imps);
    } else {
      codegen_bin_op_indet(op, imps);
    };
  (op(e1, e2), imps);
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
  | ConstInt(n) => (EInt64Lit(n), imps)
  | ConstFloat(f) => (EFloat64Lit(f), imps)
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
  (EInt64Lit(u), imps);
}
and codegen_meta_var_inst =
    (i: MetaVarInst.t, imps): (GrainIR.expr, Imports.t) => {
  (EInt64Lit(i), imps);
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

  HazelStd.Rt.(Ast.empty_hole(u', i', sigma'), Imports.with_indet(imps));
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
    Imports.with_indet(imps),
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

  HazelStd.Rt.(Ast.cast(e, ty1', ty2'), Imports.with_indet(imps));
};

let codegen = (~opts, prog: Anf.prog): GrainIR.prog => {
  let (body, c, imps) = codegen_prog(prog, Imports.default);
  let tb = Imports.codegen(imps);

  let b =
    if (opts.print_final_expr) {
      /* TODO: Clean this up. */
      /* FIXME: Probably doesn't work all the time. */
      let print_ap =
        if (prog.prog_indet) {
          HazelStd.Rt.AstPrint.print(c);
        } else {
          EAp(EVar("print"), [c]);
        };
      body @ [SExpr(print_ap)];
    } else {
      body @ [SExpr(c)];
    };

  (tb, b);
};
