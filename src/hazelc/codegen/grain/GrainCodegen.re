open Sexplib.Std;

open Grain;
open Grain.Ident;
open Grain.Expr;
open Grain.Pat;

open GrainRt;

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

  /**
     Generate Grain from the imports specification.
   */
  let codegen = imps => {
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
             Ast.imp,
             AstMk.imp,
             AstPrint.imp,
             AstSexp.imp,
             Ops.imp,
             Std.Map.imp,
           ],
         )
      |> add(sum, [Sum.imp])
      |> add(int32, [Std.Int32.imp])
      |> add(int64, [Std.Int32.imp])
      |> add(float32, [Std.Float32.imp])
      |> add(float64, [Std.Float32.imp]);

    imps |> List.map(imp => Decl.DImport(imp));
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

let dummy_label = Mir.Label.init;

let codegen_fold = (codegen_f, xs) =>
  List.map(codegen_f, xs) |> Monad.sequence;

let rec codegen_prog =
        (
          {prog_body: (body, im), prog_ty: _, prog_complete: _, prog_label: _}: Mir.prog,
        )
        : t((block, expr)) => {
  let* stmts = codegen_fold(codegen_stmt, body);
  let* im = codegen_imm(im);
  (stmts, im) |> return;
}

and codegen_stmt = (stmt: Mir.stmt): t(stmt) => {
  switch (stmt.stmt_kind) {
  | SLet(p, c) =>
    let* p' = codegen_pat(p);
    let* c' = codegen_comp(c);
    SLet(p', c') |> return;

  | SLetRec(x, c) =>
    let* p' =
      codegen_pat({
        pat_kind: PVar(x),
        pat_complete: NecessarilyComplete,
        pat_label: dummy_label,
      });
    let* c' = codegen_comp(c);
    SLetRec(p', c') |> return;
  };
}

and codegen_comp = (c: Mir.comp): t(expr) => {
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

and codegen_bin_op_complete = (op: Mir.bin_op) => {
  let (op, with_int32, with_float32) =
    Std.(
      switch (op) {
      | OpAnd => (((e1, e2) => EBinOp(OpAnd, e1, e2)), false, false)
      | OpOr => (((e1, e2) => EBinOp(OpOr, e1, e2)), false, false)
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

and codegen_bin_op_incomplete = (op: Mir.bin_op) => {
  /* FIXME: Use for fine-grained indet data. */
  let op =
    switch (op) {
    | OpAnd => Ops.bin_bool_op_and_mm
    | OpOr => Ops.bin_bool_op_or_mm
    | OpPlus => Ops.bin_int_op_plus_mm
    | OpMinus => Ops.bin_int_op_minus_mm
    | OpTimes => Ops.bin_int_op_times_mm
    | OpDivide => Ops.bin_int_op_divide_mm
    | OpLessThan => Ops.bin_int_op_less_than_mm
    | OpGreaterThan => Ops.bin_int_op_greater_than_mm
    | OpEquals => Ops.bin_int_op_equals_mm
    | OpFPlus => Ops.bin_float_op_plus_mm
    | OpFMinus => Ops.bin_float_op_minus_mm
    | OpFTimes => Ops.bin_float_op_times_mm
    | OpFDivide => Ops.bin_float_op_divide_mm
    | OpFLessThan => Ops.bin_float_op_less_than_mm
    | OpFGreaterThan => Ops.bin_float_op_greater_than_mm
    | OpFEquals => Ops.bin_float_op_equals_mm
    };

  let* () = with_incomplete_import;
  op |> return;
}

and codegen_bin_op =
    (op: Mir.bin_op, im1: Mir.imm, im2: Mir.imm, indet: Mir.completeness)
    : t(expr) => {
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

and codegen_rules = (rules: list(Mir.rule)): t(list(rule)) => {
  codegen_fold(codegen_rule, rules);
}

and codegen_rule = (rule: Mir.rule): t(rule) => {
  let* pat = codegen_pat(rule.rule_pat);
  let* (branch_stmts, branch_expr) = codegen_prog(rule.rule_branch);
  let branch = branch_stmts @ [SExpr(branch_expr)];
  RRule(pat, EBlock(branch)) |> return;
}

and codegen_imm = (im: Mir.imm): t(expr) => {
  switch (im.imm_kind) {
  | IConst(const) => codegen_const(const)

  | IVar(x) => codegen_var(x)
  };
}

and codegen_var = (x: Var.t): t(expr) => EVar(v(x)) |> return

and codegen_const = (const: Mir.constant): t(expr) => {
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

and codegen_pat = (p: Mir.pat): t(pat) => {
  switch (p.pat_kind) {
  | PWild => PWild |> return
  | PVar(x) => PVar(v(x)) |> return
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

and codegen_inj_side = (side: Mir.inj_side): t(expr => expr) => {
  let side' =
    switch (side) {
    | CInjL => Sum.inj_l
    | CInjR => Sum.inj_r
    };

  let* () = with_sum_import;
  side' |> return;
}

and codegen_inj_side_pat = (side: Mir.inj_side): t(Grain.pat => Grain.pat) => {
  let side' =
    switch (side) {
    | CInjL => Sum.inj_l_pat
    | CInjR => Sum.inj_r_pat
    };

  let* () = with_sum_import;
  side' |> return;
}

and codegen_hole_reason = (reason: ErrStatus.HoleReason.t): t(expr) => {
  let reason' =
    Ast.(
      switch (reason) {
      | TypeInconsistent => HoleReason.type_inconsistent
      | WrongLength => HoleReason.wrong_length
      }
    );

  let* () = with_incomplete_import;
  reason' |> return;
}
and codegen_meta_var = (u: MetaVar.t): t(expr) => EInt32Lit(u) |> return

and codegen_meta_var_inst = (i: MetaVarInst.t): t(expr) =>
  EInt32Lit(i) |> return

and codegen_sigma = (sigma: VarMap.t_(Mir.imm)): t(expr) => {
  let* sigma' =
    sigma
    |> codegen_fold(((x, im)) => {
         let* im' = codegen_imm(im);
         ETuple([EStringLit(x), im']) |> return;
       });

  EList(sigma') |> Std.Map.from_list |> return;
}

and codegen_empty_hole = (u, i, sigma): t(expr) => {
  let* u' = codegen_meta_var(u);
  let* i' = codegen_meta_var_inst(i);
  let* sigma' = codegen_sigma(sigma);

  let* () = with_incomplete_import;
  Ast.Ast.empty_hole(u', i', sigma') |> return;
}

and codegen_non_empty_hole = (reason, u, i, sigma, im): t(expr) => {
  let* reason' = codegen_hole_reason(reason);
  let* u' = codegen_meta_var(u);
  let* i' = codegen_meta_var_inst(i);
  let* sigma' = codegen_sigma(sigma);
  let* e' = codegen_imm(im);

  let* () = with_incomplete_import;
  Ast.Ast.non_empty_hole(reason', u', i', sigma', e') |> return;
}

and codegen_cast = (im: Mir.imm, ty1: HTyp.t, ty2: HTyp.t): t(expr) => {
  let rec codegen_htyp = (t: HTyp.t): t(expr) => {
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
    };
  };

  let* () = with_incomplete_import;

  let* e = codegen_imm(im);
  let* ty1' = codegen_htyp(ty1);
  let* ty2' = codegen_htyp(ty2);

  AstMk.cast(e, ty1', ty2') |> return;
};

let codegen = (~opts, prog: Mir.prog): modl => {
  let (State.{imps}, (body, c)) = codegen_prog(prog, State.init);
  let tb = Imports.codegen(imps);

  let b =
    if (opts.print_final_expr) {
      /* TODO: Clean this up. */
      /* FIXME: Probably doesn't work all the time. */
      let print_ap: expr =
        switch (prog.prog_complete) {
        | NecessarilyComplete => Std.print(c)
        | NecessarilyIncomplete
        | IndeterminatelyIncomplete => AstPrint.print(c)
        };
      body @ [SExpr(print_ap)];
    } else {
      body @ [SExpr(c)];
    };

  (tb, b);
};
