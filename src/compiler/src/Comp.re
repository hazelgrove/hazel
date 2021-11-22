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
let rec trans_pattern = (p: DHPat.t): Parsetree.pattern =>
  switch (p) {
  | EmptyHole(_)
  | NonEmptyHole(_)
  | Keyword(_)
  | InvalidText(_)
  | Ap(_) => raise(NotImplemented)
  | Wild => Ast_helper.Pat.any()
  | Var(v) =>
    let vstr: string = v;
    let id = Location.mknoloc(vstr);
    Ast_helper.Pat.var(id);
  | BoolLit(b) =>
    let c0 = Ast_helper.Const.bool(b);
    Ast_helper.Pat.constant(c0);
  | IntLit(i) =>
    let output = Printf.sprintf("%d", i);
    let c0 = Ast_helper.Const.int64(output);
    Ast_helper.Pat.constant(c0);
  | FloatLit(f) =>
    let output = Printf.sprintf("%f", f);
    let c0 = Ast_helper.Const.float64(output);
    Ast_helper.Pat.constant(c0);
  | _ => raise(NotImplemented)
  // | Inj(InjSide.t, t)
  // | ListNil
  // | Cons(t, t)
  // | Pair(t, t)
  // | Triv
  };

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
  | Let(pat, d1, d2) =>
    let p0 = trans_pattern(pat);
    let e1 = trans_expression(d1);
    let e2 = trans_expression(d2);
    let vb0 = Ast_helper.Vb.mk(p0, e1);
    let elet = Ast_helper.Exp.let_(Nonrecursive, Immutable, [vb0]);
    Ast_helper.Exp.block([elet, e2]);
  | Lam(pat, _, d0) =>
    let p0 = trans_pattern(pat);
    let e0 = trans_expression(d0);
    Ast_helper.Exp.lambda([p0], e0);
  | Pair(d1, d2) =>
    let e1 = trans_expression(d1);
    let e2 = trans_expression(d2);
    Ast_helper.Exp.tuple([e1, e2]);
  | FixF(v, _, d0) =>
    let v0 = trans_pattern(Var(v));
    let e0 = trans_expression(d0);
    let vb0 = Ast_helper.Vb.mk(v0, e0);
    let elet = Ast_helper.Exp.let_(Recursive, Immutable, [vb0]);
    Ast_helper.Exp.block([elet, trans_expression(BoundVar(v))]);
  | _ => raise(NotImplemented)
  // | FixF(v, ty, d)
  // | ListNil(ty)
  // | Cons(d1, d2)
  // | Inj(ty, side, d)
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

open Grain_typed;
open Grain_middle_end;
open Grain_codegen;
open Grain_linking;
open Optimize;

type input_source =
  | InputString(string)
  | InputFile(string);

type compilation_state_desc =
  | Initial(input_source)
  | Parsed(Parsetree.parsed_program)
  | WellFormed(Parsetree.parsed_program)
  | TypeChecked(Typedtree.typed_program)
  | TypedWellFormed(Typedtree.typed_program)
  | Linearized(Anftree.anf_program)
  | Optimized(Anftree.anf_program)
  | Mashed(Mashtree.mash_program)
  | Compiled(Compmod.compiled_program)
  | ObjectFileEmitted(Compmod.compiled_program)
  | Linked(Compmod.compiled_program)
  | Assembled;

type compilation_state = {
  cstate_desc: compilation_state_desc,
  cstate_filename: option(string),
  cstate_outfile: option(string),
};

type compilation_action =
  | Continue(compilation_state)
  | Stop;

type error =
  | Cannot_parse_inline_flags(string)
  | Cannot_use_help_or_version;
exception InlineFlagsError(Location.t, error);

let apply_inline_flags = (prog: Parsetree.parsed_program) => {
  switch (prog.comments) {
  | [Block({cmt_content, cmt_loc}), ..._] =>
    Grain_utils.Config.apply_inline_flags(
      ~on_error=
        err => {
          switch (err) {
          | `Help =>
            raise(InlineFlagsError(cmt_loc, Cannot_use_help_or_version))
          | `Message(msg) =>
            raise(InlineFlagsError(cmt_loc, Cannot_parse_inline_flags(msg)))
          }
        },
      cmt_content,
    )
  | _ => ()
  };
};

let next_state = (~is_root_file=false, {cstate_desc, cstate_filename} as cs) => {
  let cstate_desc =
    switch (cstate_desc) {
      | Initial(input) =>
      let (name, lexbuf, cleanup) =
        switch (input) {
        | InputString(str) => (
            cs.cstate_filename,
            Lexing.from_string(str),
            (() => ()),
          )
        | InputFile(name) =>
          let ic = open_in(name);
          (Some(name), Lexing.from_channel(ic), (() => close_in(ic)));
        };

      let parsed =
        try(Driver.parse(~name?, lexbuf)) {
        | _ as e =>
          cleanup();
          raise(e);
        };

      cleanup();
      Parsed(parsed);
    | Parsed(p) =>
      apply_inline_flags(p);
      if (is_root_file) {
        Grain_utils.Config.set_root_config();
      };
      Well_formedness.check_well_formedness(p);
      WellFormed(p);
    | WellFormed(p) => TypeChecked(Typemod.type_implementation(p))
    | TypeChecked(typed_mod) =>
      Typed_well_formedness.check_well_formedness(typed_mod);
      TypedWellFormed(typed_mod);
    | TypedWellFormed(typed_mod) =>
      Linearized(Linearize.transl_anf_module(typed_mod))
    | Linearized(anfed) =>
      switch (Grain_utils.Config.optimization_level^) {
      | Level_one
      | Level_two
      | Level_three => Optimized(Optimize.optimize_program(anfed))
      | Level_zero => Optimized(anfed)
      }
    | Optimized(optimized) =>
      Mashed(Transl_anf.transl_anf_program(optimized))
    | Mashed(mashed) =>
      Compiled(Compmod.compile_wasm_module(~name=?cstate_filename, mashed))
    | Compiled(compiled) =>
      switch (cs.cstate_outfile) {
      | Some(outfile) => Emitmod.emit_module(compiled, outfile)
      | None => ()
      };
      ObjectFileEmitted(compiled);
    | ObjectFileEmitted(compiled) =>
      Linked(Linkmod.statically_link_wasm_module(compiled))
    | Linked(linked) =>
      switch (cs.cstate_outfile) {
      | Some(outfile) => Emitmod.emit_module(linked, outfile)
      | None => ()
      };
      Assembled;
    | Assembled => Assembled
    };

  let ret = {...cs, cstate_desc};
  //log_state(ret);
  ret;
};

let rec compile_resume = (~is_root_file=false, ~hook=?, s: compilation_state) => {
  let next_state = next_state(~is_root_file, s);
  switch (hook) {
  | Some(func) =>
    switch (func(next_state)) {
    | Continue({cstate_desc: Assembled} as s) => s
    | Continue(s) => compile_resume(~is_root_file, ~hook?, s)
    | Stop => next_state
    }
  | None =>
    switch (next_state.cstate_desc) {
    | Assembled => next_state
    | _ => compile_resume(~is_root_file, ~hook?, next_state)
    }
  };
};

let compile = (d: DHExp.t) => {
  let tree = trans_parsed(d);
  let outputfile = "hazel.wasm";
  let state: compilation_state = {
    cstate_desc: Parsed(tree),
    cstate_filename: None,
    cstate_outfile: Some(outputfile),
  };
  let result = compile_resume(state);
  ();
};
