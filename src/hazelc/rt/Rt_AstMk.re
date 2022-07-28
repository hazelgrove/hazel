/**
  "hazel/rt/ast_mk.gr", auto-generated.
 */
open Grain;
open Grain.Ident;
open Rt_.Gen({
       let path = "ast_mk" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Impl = {
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
  let bin_bool_op_ = (suffix, op) =>
    with_fn2("bin_bool_op_" ++ suffix, "e1", "e2", bin_bool_op(op));
  let bin_bool_op_and = bin_bool_op_("and", BinBoolOp.and_);
  let bin_bool_op_or = bin_bool_op_("or", BinBoolOp.or_);

  let bin_int_op = with_fn3("bin_int_op", "op", "e1", "e2", Ast.bin_int_op);
  let bin_int_op_ = (suffix, op) =>
    with_fn2("bin_int_op_" ++ suffix, "e1", "e2", bin_int_op(op));
  let bin_int_op_minus = bin_int_op_("minus", BinIntOp.minus);
  let bin_int_op_plus = bin_int_op_("plus", BinIntOp.plus);
  let bin_int_op_times = bin_int_op_("times", BinIntOp.times);
  let bin_int_op_divide = bin_int_op_("divide", BinIntOp.divide);
  let bin_int_op_less_than = bin_int_op_("less_than", BinIntOp.less_than);
  let bin_int_op_greater_than =
    bin_int_op_("greater_than", BinIntOp.greater_than);
  let bin_int_op_equals = bin_int_op_("equals", BinIntOp.equals);

  let bin_float_op =
    with_fn3("bin_float_op", "op", "e1", "e2", Ast.bin_float_op);
  let bin_float_op_ = (suffix, op) =>
    with_fn2("bin_float_op_" ++ suffix, "e1", "e2", bin_float_op(op));
  let bin_float_op_minus = bin_float_op_("minus", BinFloatOp.minus);
  let bin_float_op_plus = bin_float_op_("plus", BinFloatOp.plus);
  let bin_float_op_times = bin_float_op_("times", BinFloatOp.times);
  let bin_float_op_divide = bin_float_op_("divide", BinFloatOp.divide);
  let bin_float_op_less_than =
    bin_float_op_("less_than", BinFloatOp.less_than);
  let bin_float_op_greater_than =
    bin_float_op_("greater_than", BinFloatOp.greater_than);
  let bin_float_op_equals = bin_float_op_("equals", BinFloatOp.equals);
};

module Use = (I: Make.I) => {
  open Use(I);
  let imp = imp;

  include Impl;
};
