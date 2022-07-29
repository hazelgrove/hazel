/**
  "hazel/rt/ast_mk.gr", auto-generated.
 */
open Grain;
open Grain.Ident;
open Rt_.Gen({
       let path = "./ast_mk" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Impl = {
  open H;

  module Rt_Ast =
    Rt_Ast.Use({
      let name = v("Ast");
      let from = path;
    });

  module Ast = Rt_Ast.Ast;
  module BinBoolOp = Rt_Ast.BinBoolOp;
  module BinIntOp = Rt_Ast.BinIntOp;
  module BinFloatOp = Rt_Ast.BinFloatOp;

  /* import Ast */
  with_import(Rt_Ast.imp);

  let empty_hole = with_fn3("empty_hole", "u", "i", "sigma", Ast.empty_hole);
  let non_empty_hole =
    with_fn5(
      "non_empty_hole",
      "reason",
      "u",
      "i",
      "sigma",
      "e",
      Ast.non_empty_hole,
    );

  let cast = with_fn3("cast", "e", "t1", "t2", Ast.cast);
  let failed_cast = with_fn3("failed_cast", "e", "t1", "t2", Ast.failed_cast);

  let ap = with_fn2("ap", "e1", "e2", Ast.ap);

  let bool_lit = with_fn1("bool_lit", "b", Ast.bool_lit);
  let int_lit = with_fn1("int_lit", "n", Ast.int_lit);
  let float_lit = with_fn1("float_lit", "f", Ast.float_lit);

  let bin_bool_op =
    with_fn3("bin_bool_op", "op", "e1", "e2", Ast.bin_bool_op);
  let mk_bin_bool_op = (suffix, op) =>
    with_fn2("bin_bool_op_" ++ suffix, "e1", "e2", (e1, e2) =>
      Expr.(ap(var(bin_bool_op), [op, e1, e2]))
    );

  let bin_bool_op_and = mk_bin_bool_op("and", BinBoolOp.and_);
  let bin_bool_op_or = mk_bin_bool_op("or", BinBoolOp.or_);

  let bin_int_op = with_fn3("bin_int_op", "op", "e1", "e2", Ast.bin_int_op);
  let mk_bin_int_op = (suffix, op) =>
    with_fn2("bin_int_op_" ++ suffix, "e1", "e2", (e1, e2) =>
      Expr.(ap(var(bin_int_op), [op, e1, e2]))
    );

  let bin_int_op_minus = mk_bin_int_op("minus", BinIntOp.minus);
  let bin_int_op_plus = mk_bin_int_op("plus", BinIntOp.plus);
  let bin_int_op_times = mk_bin_int_op("times", BinIntOp.times);
  let bin_int_op_divide = mk_bin_int_op("divide", BinIntOp.divide);
  let bin_int_op_less_than = mk_bin_int_op("less_than", BinIntOp.less_than);
  let bin_int_op_greater_than =
    mk_bin_int_op("greater_than", BinIntOp.greater_than);
  let bin_int_op_equals = mk_bin_int_op("equals", BinIntOp.equals);

  let bin_float_op =
    with_fn3("bin_float_op", "op", "e1", "e2", Ast.bin_float_op);
  let mk_bin_float_op = (suffix, op) =>
    with_fn2("bin_float_op_" ++ suffix, "e1", "e2", (e1, e2) =>
      Expr.(ap(var(bin_float_op), [op, e1, e2]))
    );

  let bin_float_op_minus = mk_bin_float_op("minus", BinFloatOp.minus);
  let bin_float_op_plus = mk_bin_float_op("plus", BinFloatOp.plus);
  let bin_float_op_times = mk_bin_float_op("times", BinFloatOp.times);
  let bin_float_op_divide = mk_bin_float_op("divide", BinFloatOp.divide);
  let bin_float_op_less_than =
    mk_bin_float_op("less_than", BinFloatOp.less_than);
  let bin_float_op_greater_than =
    mk_bin_float_op("greater_than", BinFloatOp.greater_than);
  let bin_float_op_equals = mk_bin_float_op("equals", BinFloatOp.equals);
};

module Use = (I: Make.I) => {
  open Use(I);
  open Impl;

  let imp = imp;

  let empty_hole = ap3(empty_hole);
  let non_empty_hole = ap5(non_empty_hole);

  let cast = ap3(cast);
  let failed_cast = ap3(failed_cast);

  let ap = ap2(ap);

  let bool_lit = ap1(bool_lit);
  let int_lit = ap1(int_lit);
  let float_lit = ap1(float_lit);

  let bin_bool_op = ap3(bin_bool_op);
  let bin_bool_op_and = ap2(bin_bool_op_and);
  let bin_bool_op_or = ap2(bin_bool_op_or);

  let bin_int_op = ap3(bin_int_op);
  let bin_int_op_minus = ap2(bin_int_op_minus);
  let bin_int_op_plus = ap2(bin_int_op_plus);
  let bin_int_op_times = ap2(bin_int_op_times);
  let bin_int_op_divide = ap2(bin_int_op_divide);
  let bin_int_op_less_than = ap2(bin_int_op_less_than);
  let bin_int_op_greater_than = ap2(bin_int_op_greater_than);
  let bin_int_op_equals = ap2(bin_int_op_equals);

  let bin_float_op = ap3(bin_float_op);
  let bin_float_op_minus = ap2(bin_float_op_minus);
  let bin_float_op_plus = ap2(bin_float_op_plus);
  let bin_float_op_times = ap2(bin_float_op_times);
  let bin_float_op_divide = ap2(bin_float_op_divide);
  let bin_float_op_less_than = ap2(bin_float_op_less_than);
  let bin_float_op_greater_than = ap2(bin_float_op_greater_than);
  let bin_float_op_equals = ap2(bin_float_op_equals);
};
