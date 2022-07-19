open Sexplib.Std;

open Hazelc_mir;
open Grainlib;

[@deriving sexp]
type opts = {print_final_expr: bool};

module Imports = {
  [@deriving sexp]
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

  let with_incomplete = imps => {...imps, indet: true};
  let with_sum = imps => {...imps, sum: true};
  /* let with_int64 = imps => {...imps, int64: true}; */
  let with_int32 = imps => {...imps, int32: true};
  /* let with_float64 = imps => {...imps, float64: true}; */
  let with_float32 = imps => {...imps, float32: true};

  /*
     Generate Grainlib from the imports specification.
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

    imps |> List.map(((x, path)) => TImport(x, path));
  };
};

module State = {
  [@deriving sexp]
  type t = {imps: Imports.t};

  let init = {imps: Imports.default};

  let with_incomplete_import = ({imps}) => {
    imps: Imports.with_incomplete(imps),
  };
  let with_sum_import = ({imps}) => {imps: Imports.with_sum(imps)};
  let with_int32_import = ({imps}) => {imps: Imports.with_int32(imps)};
  let with_float32_import = ({imps}) => {imps: Imports.with_float32(imps)};
};

module Monad = {
  include StateMonad.Make(State);

  let with_incomplete_import = update(State.with_incomplete_import);
  let with_sum_import = update(State.with_sum_import);
  let with_int32_import = update(State.with_int32_import);
  let with_float32_import = update(State.with_float32_import);
};

open Monad;
open Monad.Syntax;

let dummy_label = Label.init;

let codegen_fold = (codegen_f, xs) =>
  List.map(codegen_f, xs) |> Monad.sequence;

let rec codegen_prog =
        (
          {prog_body: (body, im), prog_ty: _, prog_complete: _, prog_label: _}: Anf.prog,
        )
        : t((Grainlib.block, Grainlib.expr)) => {
  let* stmts = codegen_fold(codegen_stmt, body);
  let* im = codegen_imm(im);
  (stmts, im) |> return;
}

and codegen_stmt = (stmt: Anf.stmt): t(Grainlib.stmt) => {
  switch (stmt.stmt_kind) {
  | SLet(p, c) =>
    let* p' = codegen_pat(p);
    let* c' = codegen_comp(c);
    SLet([p'], c') |> return;

  | SLetRec(x, c) =>
    let* p' =
      codegen_pat({
        pat_kind: PVar(x),
        pat_complete: NecessarilyComplete,
        pat_label: dummy_label,
      });
    let* c' = codegen_comp(c);
    SLetRec([p'], c') |> return;
  };
}

and codegen_comp = (c: Anf.comp): t(Grainlib.expr) => {
  switch (c.comp_kind) {
  | CImm(im) => codegen_imm(im)

  | CBinOp(op, im1, im2) => codegen_bin_op(op, im1, im2, c.comp_complete)

  | CAp(fn, arg) =>
    let* fn' = codegen_imm(fn);
    let* arg' = codegen_imm(arg);
    EAp(fn', [arg']) |> return;

  | CFun(param, body) =>
    let* param' = codegen_pat(param);
    let* (body', c) = codegen_prog(body);
    let body' = body' @ [SExpr(c)];
    ELam([param'], EBlock(body')) |> return;

  | CCons(im1, im2) =>
    let* e1 = codegen_imm(im1);
    let* e2 = codegen_imm(im2);
    ECons(e1, e2) |> return;

  | CPair(im1, im2) =>
    let* e1 = codegen_imm(im1);
    let* e2 = codegen_imm(im2);
    ETuple([e1, e2]) |> return;

  | CInj(side, im) =>
    let* ctor = codegen_inj_side(side);
    let* e = codegen_imm(im);
    ctor(e) |> return;

  | CCase(scrut, rules) =>
    let* scrut = codegen_imm(scrut);
    let* rules = codegen_rules(rules);
    EMatch(scrut, rules) |> return;

  | CEmptyHole(u, i, sigma) => codegen_empty_hole(u, i, sigma)

  | CNonEmptyHole(reason, u, i, sigma, im) =>
    codegen_non_empty_hole(reason, u, i, sigma, im)

  | CCast(im, t1, t2) => codegen_cast(im, t1, t2)
  };
}

and codegen_bin_op_complete = (op: Anf.bin_op) => {
  let (op, with_int32, with_float32) =
    GrainStd.(
      switch (op) {
      | OpAnd => (
          ((e1, e2) => Grainlib.EBinOp(OpAnd, e1, e2)),
          false,
          false,
        )
      | OpOr => (((e1, e2) => Grainlib.EBinOp(OpOr, e1, e2)), false, false)
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

  let* () =
    if (with_int32) {
      with_int32_import;
    } else {
      () |> return;
    };
  let* () =
    if (with_float32) {
      with_float32_import;
    } else {
      () |> return;
    };
  op |> return;
}

and codegen_bin_op_incomplete = (op: Anf.bin_op) => {
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

  let* () = with_incomplete_import;
  op |> return;
}

and codegen_bin_op =
    (op: Anf.bin_op, im1: Anf.imm, im2: Anf.imm, indet: Anf.completeness)
    : t(Grainlib.expr) => {
  let* e1 = codegen_imm(im1);
  let* e2 = codegen_imm(im2);
  let* op =
    switch (indet) {
    // TODO: Separate cases
    | NecessarilyComplete => codegen_bin_op_complete(op)
    | NecessarilyIncomplete
    | IndeterminatelyIncomplete => codegen_bin_op_incomplete(op)
    };
  op(e1, e2) |> return;
}

and codegen_rules = (rules: list(Anf.rule)): t(list(Grainlib.rule)) => {
  codegen_fold(codegen_rule, rules);
}

and codegen_rule = (rule: Anf.rule): t(Grainlib.rule) => {
  let* pat = codegen_pat(rule.rule_pat);
  let* (branch_stmts, branch_expr) = codegen_prog(rule.rule_branch);
  let branch = branch_stmts @ [SExpr(branch_expr)];
  RRule(pat, EBlock(branch)) |> return;
}

and codegen_imm = (im: Anf.imm): t(Grainlib.expr) => {
  switch (im.imm_kind) {
  | IConst(const) => codegen_const(const)

  | IVar(x) => codegen_var(x)
  };
}

and codegen_var = (x: Var.t): t(Grainlib.expr) => EVar(x) |> return

and codegen_const = (const: Anf.constant): t(Grainlib.expr) => {
  (
    switch (const) {
    | ConstInt(n) => EInt32Lit(n)
    | ConstFloat(f) => EFloat32Lit(f)
    | ConstBool(b) => EBoolLit(b)
    | ConstNil(_) => EList([])
    | ConstTriv => ETriv
    }
  )
  |> return;
}

and codegen_pat = (p: Anf.pat): t(Grainlib.pat) => {
  switch (p.pat_kind) {
  | PWild => PWild |> return
  | PVar(x) => PVar(x) |> return
  | PInt(n) => PInt(n) |> return
  | PFloat(f) => PFloat(f) |> return
  | PBool(b) => PBool(b) |> return
  | PNil => PNil |> return

  | PInj(side, p) =>
    let* ctor = codegen_inj_side_pat(side);
    let* p' = codegen_pat(p);
    ctor(p') |> return;

  | PCons(p1, p2) =>
    let* p1' = codegen_pat(p1);
    let* p2' = codegen_pat(p2);
    PCons(p1', p2') |> return;

  | PPair(p1, p2) =>
    let* p1' = codegen_pat(p1);
    let* p2' = codegen_pat(p2);
    PTuple([p1', p2']) |> return;

  | PTriv => PTriv |> return
  };
}

and codegen_inj_side =
    (side: Anf.inj_side): t(Grainlib.expr => Grainlib.expr) => {
  let side' =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l
    | CInjR => HazelStd.Rt.Sum.inj_r
    };

  let* () = with_sum_import;
  side' |> return;
}

and codegen_inj_side_pat =
    (side: Anf.inj_side): t(Grainlib.pat => Grainlib.pat) => {
  let side' =
    switch (side) {
    | CInjL => HazelStd.Rt.Sum.inj_l_pat
    | CInjR => HazelStd.Rt.Sum.inj_r_pat
    };

  let* () = with_sum_import;
  side' |> return;
}

and codegen_hole_reason = (reason: ErrStatus.HoleReason.t): t(Grainlib.expr) => {
  let reason' =
    HazelStd.Rt.Ast.(
      switch (reason) {
      | TypeInconsistent => HoleReason.type_inconsistent
      | WrongLength => HoleReason.wrong_length
      }
    );

  let* () = with_incomplete_import;
  EVar(reason') |> return;
}
and codegen_meta_var = (u: MetaVar.t): t(Grainlib.expr) =>
  EInt32Lit(u) |> return

and codegen_meta_var_inst = (i: MetaVarInst.t): t(Grainlib.expr) =>
  EInt32Lit(i) |> return

and codegen_sigma = (sigma: VarMap.t_(Anf.imm)): t(Grainlib.expr) => {
  let* sigma' =
    sigma
    |> codegen_fold(((x, im)) => {
         let* im' = codegen_imm(im);
         Grainlib.ETuple([Grainlib.EStringLit(x), im']) |> return;
       });

  Grainlib.EList(sigma') |> GrainStd.Map.from_list |> return;
}

and codegen_empty_hole = (u, i, sigma): t(Grainlib.expr) => {
  let* u' = codegen_meta_var(u);
  let* i' = codegen_meta_var_inst(i);
  let* sigma' = codegen_sigma(sigma);

  let* () = with_incomplete_import;
  HazelStd.Rt.Ast.empty_hole(u', i', sigma') |> return;
}

and codegen_non_empty_hole = (reason, u, i, sigma, im): t(Grainlib.expr) => {
  let* reason' = codegen_hole_reason(reason);
  let* u' = codegen_meta_var(u);
  let* i' = codegen_meta_var_inst(i);
  let* sigma' = codegen_sigma(sigma);
  let* e' = codegen_imm(im);

  let* () = with_incomplete_import;
  HazelStd.Rt.Ast.non_empty_hole(reason', u', i', sigma', e') |> return;
}

and codegen_cast = (im: Anf.imm, ty1: HTyp.t, ty2: HTyp.t): t(Grainlib.expr) => {
  let rec codegen_htyp = (t: HTyp.t): t(Grainlib.expr) => {
    HazelStd.Rt.(
      switch (t) {
      | Hole => Ast.HTyp.hole |> return
      | Int => Ast.HTyp.int |> return
      | Float => Ast.HTyp.float |> return
      | Bool => Ast.HTyp.bool |> return

      | Arrow(ty1, ty2) =>
        let* ty1' = codegen_htyp(ty1);
        let* ty2' = codegen_htyp(ty2);
        Ast.HTyp.arrow(ty1', ty2') |> return;

      | Sum(ty1, ty2) =>
        let* ty1' = codegen_htyp(ty1);
        let* ty2' = codegen_htyp(ty2);
        Ast.HTyp.sum(ty1', ty2') |> return;
      | Prod(tys) =>
        let* tys' = tys |> codegen_fold(codegen_htyp);
        Ast.HTyp.prod(tys') |> return;

      | List(ty) =>
        let* ty' = codegen_htyp(ty);
        Ast.HTyp.list(ty') |> return;
      }
    );
  };

  let* () = with_incomplete_import;

  let* e = codegen_imm(im);
  let* ty1' = codegen_htyp(ty1);
  let* ty2' = codegen_htyp(ty2);

  HazelStd.Rt.Ast.cast(e, ty1', ty2') |> return;
};

let codegen = (~opts, prog: Anf.prog): Grainlib.prog => {
  let (State.{imps}, (body, c)) = codegen_prog(prog, State.init);
  let tb = Imports.codegen(imps);

  let b =
    if (opts.print_final_expr) {
      /* TODO: Clean this up. */
      /* FIXME: Probably doesn't work all the time. */
      let print_ap: Grainlib.expr =
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
