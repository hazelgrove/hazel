open Grain_parsing;
// open Grain_typed;
// open Grain_middle_end;
// open Grain_codegen;
// open Grain_linking;
// open Optimize;

exception NotImplemented;

let trans_bool_op = (op: DHExp.BinBoolOp.t): Parsetree.prim2 =>
  switch (op) {
  | And => And
  | Or => Or
  };

let trans_int_op = (op: DHExp.BinIntOp.t): Parsetree.prim2 => {
  switch (op) {
  | Minus =>
    WasmBinaryI64({
      wasm_op: Op_sub_int64,
      arg_types: (Wasm_int64, Wasm_int64),
      ret_type: Wasm_int64,
    })
  | Plus =>
    WasmBinaryI64({
      wasm_op: Op_add_int64,
      arg_types: (Wasm_int64, Wasm_int64),
      ret_type: Wasm_int64,
    })
  | Times =>
    WasmBinaryI64({
      wasm_op: Op_mul_int64,
      arg_types: (Wasm_int64, Wasm_int64),
      ret_type: Wasm_int64,
    })
  | Divide =>
    WasmBinaryI64({
      wasm_op: Op_div_s_int64,
      arg_types: (Wasm_int64, Wasm_int64),
      ret_type: Wasm_int64,
    })
  | LessThan =>
    WasmBinaryI64({
      wasm_op: Op_lt_s_int64,
      arg_types: (Wasm_int64, Wasm_int64),
      ret_type: Grain_bool,
    })
  | GreaterThan =>
    WasmBinaryI64({
      wasm_op: Op_gt_s_int64,
      arg_types: (Wasm_int64, Wasm_int64),
      ret_type: Grain_bool,
    })
  | Equals => Eq
  };
};

let trans_float_op = (op: DHExp.BinFloatOp.t): Parsetree.prim2 => {
  switch (op) {
  | FPlus =>
    WasmBinaryF64({
      wasm_op: Op_add_float64,
      arg_types: (Wasm_float64, Wasm_float64),
      ret_type: Wasm_float64,
    })
  | FMinus =>
    WasmBinaryF64({
      wasm_op: Op_sub_float64,
      arg_types: (Wasm_float64, Wasm_float64),
      ret_type: Wasm_float64,
    })
  | FTimes =>
    WasmBinaryF64({
      wasm_op: Op_mul_float64,
      arg_types: (Wasm_float64, Wasm_float64),
      ret_type: Wasm_float64,
    })
  | FDivide =>
    WasmBinaryF64({
      wasm_op: Op_div_float64,
      arg_types: (Wasm_float64, Wasm_float64),
      ret_type: Wasm_float64,
    })
  | FLessThan =>
    WasmBinaryF64({
      wasm_op: Op_lt_float64,
      arg_types: (Wasm_float64, Wasm_float64),
      ret_type: Grain_bool,
    })
  | FGreaterThan =>
    WasmBinaryF64({
      wasm_op: Op_gt_float64,
      arg_types: (Wasm_float64, Wasm_float64),
      ret_type: Grain_bool,
    })
  | FEquals => Eq
  };
};

open Ast_helper;
let rec trans_expression = (d: DHExp.t): Parsetree.expression =>
  switch (d) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  // TODO rename to ExpandingKeyword
  | Keyword(_)
  | FreeVar(_)
  | InvalidText(_)
  | InconsistentBranches(_)
  | Cast(_)
  | FailedCast(_)
  | InvalidOperation(_) => raise(NotImplemented)
  | BoundVar(v) =>
    let identifier = Identifier.parse(v);
    let id = Location.mknoloc(identifier);
    Ast_helper.Exp.ident(id);
  | Ap(d1, d2) =>
    let e1 = trans_expression(d1);
    let e2 = trans_expression(d2);
    Ast_helper.Exp.apply(e1, [e2]);
  | BoolLit(b) =>
    let c0 = Ast_helper.Const.bool(b);
    Ast_helper.Exp.constant(c0);
  | IntLit(i) =>
    let output = Printf.sprintf("%d", i);
    let c0 = Ast_helper.Const.int64(output);
    Ast_helper.Exp.constant(c0);
  | FloatLit(f) =>
    let output = Printf.sprintf("%f", f);
    let c0 = Ast_helper.Const.float64(output);
    Ast_helper.Exp.constant(c0);
  | BinBoolOp(op, d1, d2) =>
    let e1 = trans_expression(d1);
    let e2 = trans_expression(d2);
    let op0 = trans_bool_op(op);
    Ast_helper.Exp.prim2(op0, e1, e2);
  | BinIntOp(op, d1, d2) =>
    let e1 = trans_expression(d1);
    let e2 = trans_expression(d2);
    let op0 = trans_int_op(op);
    Ast_helper.Exp.prim2(op0, e1, e2);
  | BinFloatOp(op, d1, d2) =>
    let e1 = trans_expression(d1);
    let e2 = trans_expression(d2);
    let op0 = trans_float_op(op);
    Ast_helper.Exp.prim2(op0, e1, e2);
  | _ => raise(NotImplemented)
  // | Let(pat, d1, d2)
  // | FixF(v, ty, d)
  // | Lam(pat, ty, d)
  // | ListNil(ty)
  // | Cons(d1, d2)
  // | Inj(ty, side, d)
  // | Pair(d1, d2)
  // | Triv
  // | ConsistentCase(case)
  };

let trans_parsed = (d: DHExp.t): Parsetree.parsed_program => {
  let exp = trans_expression(d);
  {
    statements: [Ast_helper.Top.expr(exp)],
    comments: [],
    prog_loc: Grain_parsing.Location.dummy_loc,
  };
};

let compile = (d: DHExp.t) => {
  let tree = trans_parsed(d);
  let outputfile = "hazel.wasm";
  let state: Compile.compilation_state = {
    cstate_desc: Parsed(tree),
    cstate_filename: None,
    cstate_outfile: Some(outputfile),
  };
  let result = Compile.compile_resume(state);
  ();
};
