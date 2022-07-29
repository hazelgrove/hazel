/**
  "hazel/rt/ast.gr", handwritten.
 */
open Grain;
open Grain.Ident;
open Rt_.Stub({
       let path = "./ast" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Use = (I: Make.I) => {
  open Use(I);

  let imp = imp;

  /**
    [Var] type.
   */
  module Var = {
    let ty = ident(v("Var"));
  };

  /**
    [MetaVar] type.
   */
  module MetaVar = {
    let ty = ident(v("MetaVar"));
  };

  /**
    [MetaVarInst] type.
   */
  module MetaVarInst = {
    let ty = ident(v("MetaVarInst"));
  };

  /**
    [HoleEnvironment] type.
   */
  module HoleEnvironment = {
    let ty = ident(v("HoleEnvironment"));
  };

  /**
    [HoleReason] type.
   */
  module HoleReason = {
    let ty = ident(v("HoleReason"));

    let type_inconsistent = ctor0(v("TypeInconsistent"));
    let wrong_length = ctor0(v("WrongLength"));
  };

  /**
    [HTyp] type.
   */
  module HTyp = {
    let ty = ident(v("HTyp"));

    let hole = ctor0(v("Hole"));
    let int = ctor0(v("Int"));
    let float = ctor0(v("Float"));
    let bool = ctor0(v("Bool"));
    let arrow = (t1, t2) => ctor2(v("Arrow"), t1, t2);
    let sum = (t1, t2) => ctor2(v("Sum"), t1, t2);
    let prod = ts => ctor(v("Prod"), ts);
    let list = t => ctor1(v("List"), t);
  };

  /**
    [BinBoolOp] type.
   */
  module BinBoolOp = {
    let ty = ident(v("BinBoolOp"));

    let and_ = ctor0(v("And"));
    let or_ = ctor0(v("Or"));
  };

  /**
    [BinIntOp] type.
   */
  module BinIntOp = {
    let ty = ident(v("BinIntOp"));

    let minus = ctor0(v("Minus"));
    let plus = ctor0(v("Plus"));
    let times = ctor0(v("Times"));
    let divide = ctor0(v("Divide"));
    let less_than = ctor0(v("LessThan"));
    let greater_than = ctor0(v("GreaterThan"));
    let equals = ctor0(v("Equals"));
  };

  /**
    [BinFloatOp] type.
   */
  module BinFloatOp = {
    let ty = ident(v("BinFloatOp"));

    let minus = ctor0(v("FMinus"));
    let plus = ctor0(v("FPlus"));
    let times = ctor0(v("FTimes"));
    let divide = ctor0(v("FDivide"));
    let less_than = ctor0(v("FLessThan"));
    let greater_than = ctor0(v("FGreaterThan"));
    let equals = ctor0(v("FEquals"));
  };

  /**
    [Ast] type.
   */
  module Ast = {
    let ty = ident(v("Ast"));

    let empty_hole = ctor3(v("EmptyHole"));
    let non_empty_hole = ctor5(v("NonEmptyHole"));

    let cast = ctor3(v("Cast"));
    let failed_cast = ctor3(v("FailedCast"));

    let ap = ctor2(v("Ap"));

    let bool_lit = ctor1(v("BoolLit"));
    let int_lit = ctor1(v("IntLit"));
    let float_lit = ctor1(v("FloatLit"));

    let bin_bool_op = ctor3(v("BinBoolOp"));
    let bin_int_op = ctor3(v("BinIntOp"));
    let bin_float_op = ctor3(v("BinFloatOp"));
  };
};
