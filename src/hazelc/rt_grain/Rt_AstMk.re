/**
  "hazel/rt/ast_mk.gr", auto-generated.
 */
open Rt_Ast;

open Rt_.WithImpl({
       let name = "AstMk";
       let path = "ast_mk";
     });
/**
  Import statement.
 */
let import = import;

/**
  Implementation module.
 */
let impl_md = impl_md;

/* import Ast */
Rt_Ast.import |> add_impl_import;

let empty_hole =
  mk_fn3("empty_hole", "u", "i", "sigma", Ast.empty_hole) |> add_impl_fn;
let non_empty_hole =
  mk_fn5(
    "non_empty_hole",
    "reason",
    "u",
    "i",
    "sigma",
    "e",
    Ast.non_empty_hole,
  )
  |> add_impl_fn;

let cast = mk_fn3("e", "t1", "t2", "sigma", Ast.cast) |> add_impl_fn;
let failed_cast =
  mk_fn3("failed_cast", "e", "t1", "t2", Ast.failed_cast) |> add_impl_fn;

let ap = mk_fn2("ap", "e1", "e2", Ast.ap) |> add_impl_fn;

let bool_lit = mk_fn1("bool_lit", "b", Ast.bool_lit) |> add_impl_fn;
let int_lit = mk_fn1("int_lit", "n", Ast.int_lit) |> add_impl_fn;
let float_lit = mk_fn1("float_lit", "f", Ast.float_lit) |> add_impl_fn;

let bin_bool_op =
  mk_fn3("bin_bool_op", "op", "e1", "e2", Ast.bin_bool_op) |> add_impl_fn;
let bin_bool_op_mk = (name, op) =>
  mk_fn2("bin_bool_op_" ++ name, "e1", "e2", Ast.bin_bool_op(op));
let bin_bool_op_and = bin_bool_op_mk("and", BinBoolOp.and_) |> add_impl_fn;
let bin_bool_op_or = bin_bool_op_mk("or", BinBoolOp.or_) |> add_impl_fn;

let bin_int_op =
  mk_fn3("bin_int_op", "op", "e1", "e2", Ast.bin_int_op) |> add_impl_fn;
let bin_int_op_mk = (name, op) =>
  mk_fn2("bin_int_op_" ++ name, "e1", "e2", Ast.bin_int_op(op));
let bin_int_op_minus = bin_int_op_mk("minus", BinIntOp.minus) |> add_impl_fn;
let bin_int_op_plus = bin_int_op_mk("plus", BinIntOp.plus) |> add_impl_fn;
let bin_int_op_times = bin_int_op_mk("times", BinIntOp.times) |> add_impl_fn;
let bin_int_op_divide =
  bin_int_op_mk("divide", BinIntOp.divide) |> add_impl_fn;

let bin_float_op =
  mk_fn3("bin_float_op", "op", "e1", "e2", Ast.bin_float_op) |> add_impl_fn;
let bin_float_op_mk = (name, op) =>
  mk_fn2("bin_float_op_" ++ name, "e1", "e2", Ast.bin_float_op(op));
let bin_float_op_minus =
  bin_float_op_mk("minus", BinFloatOp.minus) |> add_impl_fn;
let bin_float_op_plus =
  bin_float_op_mk("plus", BinFloatOp.plus) |> add_impl_fn;
let bin_float_op_times =
  bin_float_op_mk("times", BinFloatOp.times) |> add_impl_fn;
let bin_float_op_divide =
  bin_float_op_mk("divide", BinFloatOp.divide) |> add_impl_fn;
