/**
  "hazel/rt/ast.gr", handwritten.
 */
open Rt_.WithoutImpl({
       let name = "Ast";
       let path = "ast";
     });
/**
  Import statement.
 */
let import = import;

/**
  Implementation module.
 */
let impl_md = impl_md;

/**
  [Var] type.
 */
module Var = {
  let ty = ident("Var");
};

/**
  [MetaVar] type.
 */
module MetaVar = {
  let ty = ident("MetaVar");
};

/**
  [MetaVarInst] type.
 */
module MetaVarInst = {
  let ty = ident("MetaVarInst");
};

/**
  [HoleEnvironment] type.
 */
module HoleEnvironment = {
  let ty = ident("HoleEnvironment");
};

/**
  [HoleReason] type.
 */
module HoleReason = {
  let ty = ident("HoleReason");

  let type_inconsistent = ctor0("TypeInconsistent");
  let wrong_length = ctor0("WrongLength");
};

/**
  [HTyp] type.
 */
module HTyp = {
  let ty = ident("HTyp");

  let hole = ctor0("Hole");
  let int = ctor0("Int");
  let float = ctor0("Float");
  let bool = ctor0("Bool");
  let arrow = (t1, t2) => ctor2("Arrow", t1, t2);
  let sum = (t1, t2) => ctor2("Sum", t1, t2);
  let prod = ts => ctor("Prod", ts);
  let list = t => ctor1("List", t);
};

/**
  [BinBoolOp] type.
 */
module BinBoolOp = {
  let ty = ident("BinBoolOp");

  let and_ = ctor0("And");
  let or_ = ctor0("Or");
};

/**
  [BinIntOp] type.
 */
module BinIntOp = {
  let ty = ident("BinIntOp");

  let minus = ctor0("Minus");
  let plus = ctor0("Plus");
  let times = ctor0("Times");
  let divide = ctor0("Divide");
  let less_than = ctor0("LessThan");
  let greater_than = ctor0("GreaterThan");
  let equals = ctor0("Equals");
};

/**
  [BinFloatOp] type.
 */
module BinFloatOp = {
  let ty = ident("BinFloatOp");

  let minus = ctor0("FMinus");
  let plus = ctor0("FPlus");
  let times = ctor0("FTimes");
  let divide = ctor0("FDivide");
  let less_than = ctor0("FLessThan");
  let greater_than = ctor0("FGreaterThan");
  let equals = ctor0("FEquals");
};

/**
  [Ast] type.
 */
module Ast = {
  let ty = ident("Ast");

  let empty_hole = ctor3("EmptyHole");
  let non_empty_hole = ctor5("NonEmptyHole");

  let cast = ctor3("Cast");
  let failed_cast = ctor3("FailedCast");

  let ap = ctor2("Ap");

  let bool_lit = ctor1("BoolLit");
  let int_lit = ctor1("IntLit");
  let float_lit = ctor1("FloatLit");

  let bin_bool_op = ctor3("BinBoolOp");
  let bin_int_op = ctor3("BinIntOp");
  let bin_float_op = ctor3("BinFloatOp");
};
