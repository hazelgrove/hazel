/**
  "hazel/rt/ops.gr", auto-generated.
 */
open Grain;
open Grain.Ident;
open Rt_.Gen({
       let path = "ops" |> Path.v;
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

  module Rt_AstMk =
    Rt_AstMk.Use({
      let name = v("AstMk");
      let from = path;
    });

  module Rt_MaybeIndet =
    Rt_MaybeIndet.Use({
      let name = v("MaybeIndet");
      let from = path;
    });

  module Std_Int32 =
    Std.Int32.Use({
      let name = v("Int32");
      let from = path;
    });

  module Std_Float32 =
    Std.Float32.Use({
      let name = v("Float32");
      let from = path;
    });

  /* import Ast, AstMk, MaybeIndet */
  with_import(Rt_Ast.imp);
  with_import(Rt_AstMk.imp);
  with_import(Rt_MaybeIndet.imp);
  with_import(Std_Int32.imp);
  with_import(Std_Float32.imp);

  let wrap_empty_hole =
    with_fn3("wrap_empty_hole", "u", "i", "sigma", Rt_AstMk.empty_hole);
  let wrap_non_empty_hole =
    with_fn5(
      "wrap_non_empty_hole",
      "reason",
      "u",
      "i",
      "sigma",
      "e",
      Rt_AstMk.non_empty_hole,
    );

  let wrap_cast = with_fn3("wrap_cast", "e", "t1", "t2", Rt_AstMk.cast);
  let wrap_failed_cast =
    with_fn3("wrap_failed_cast", "e", "t1", "t2", Rt_AstMk.failed_cast);

  let wrap_ap = with_fn2("wrap_ap", "e1", "e2", Rt_AstMk.ap);

  let wrap_bool_lit = with_fn1("wrap_bool_lit", "b", Rt_AstMk.bool_lit);
  let wrap_int_lit = with_fn1("wrap_int_lit", "n", Rt_AstMk.int_lit);
  let wrap_float_lit = with_fn1("wrap_float_lit", "f", Rt_AstMk.float_lit);

  let wrap_bin_bool_op =
    with_fn3("wrap_bin_bool_op", "op", "b1", "b2", Rt_AstMk.bin_bool_op);
  let wrap_bin_int_op =
    with_fn3("wrap_bin_int_op", "op", "b1", "b2", Rt_AstMk.bin_int_op);
  let wrap_bin_float_op =
    with_fn3("wrap_bin_float_op", "op", "b1", "b2", Rt_AstMk.bin_float_op);

  type bool_op =
    | And
    | Or;
  type num_op =
    | Minus
    | Plus
    | Times
    | Divide
    | LessThan
    | GreaterThan
    | Equals;
  type ty =
    | Int(num_op)
    | Float(num_op)
    | Bool(bool_op);
  type ness_side =
    | L(bool)
    | R(bool)
    | B(bool, bool);
  type ness =
    | PP
    | IP
    | MP
    | PI
    | PM
    | II
    | MI
    | IM
    | MM;

  let mk_bin_op = (ty, ness, x1, x2) => {
    let fns_of_int_op =
      Rt_AstMk.(
        Std_Int32.(
          fun
          | Minus => (bin_int_op_minus, sub)
          | Plus => (bin_int_op_plus, add)
          | Times => (bin_int_op_times, mul)
          | Divide => (bin_int_op_divide, div)
          | LessThan => (bin_int_op_less_than, lt)
          | GreaterThan => (bin_int_op_greater_than, gt)
          | Equals => (bin_int_op_equals, eq)
        )
      );
    let fns_of_float_op =
      Rt_AstMk.(
        Std_Int32.(
          fun
          | Minus => (bin_int_op_minus, sub)
          | Plus => (bin_int_op_plus, add)
          | Times => (bin_int_op_times, mul)
          | Divide => (bin_int_op_divide, div)
          | LessThan => (bin_int_op_less_than, lt)
          | GreaterThan => (bin_int_op_greater_than, gt)
          | Equals => (bin_int_op_equals, eq)
        )
      );
    let fns_of_bool_op =
      Rt_AstMk.(
        fun
        | And => (bin_bool_op_and, Std.and_)
        | Or => (bin_bool_op_or, Std.or_)
      );

    let str_of_num_op =
      fun
      | Plus => "plus"
      | Minus => "minus"
      | Times => "times"
      | Divide => "divide"
      | LessThan => "less_than"
      | GreaterThan => "greater_than"
      | Equals => "equals";
    let str_of_bool_op =
      fun
      | And => "and"
      | Or => "or";

    let (ty_str, op_str, (wrap, prim), wrap_lit) =
      switch (ty) {
      | Bool(op) => (
          "bool",
          str_of_bool_op(op),
          fns_of_bool_op(op),
          wrap_bool_lit,
        )
      | Int(op) => (
          "int",
          str_of_num_op(op),
          fns_of_int_op(op),
          wrap_int_lit,
        )
      | Float(op) => (
          "float",
          str_of_num_op(op),
          fns_of_float_op(op),
          wrap_float_lit,
        )
      };

    let maybe_match = Rt_MaybeIndet.match;
    let pp = prim;
    let ii = (e1, e2) => wrap(e1, e2);

    let ip = (e1, v2) => wrap(e1, wrap_lit(v2));
    let pi = (v1, e2) => wrap(wrap_lit(v1), e2);

    let mp = (m1, v2) =>
      maybe_match(m1, v1 => pp(v1, v2), e1 => ip(e1, v2));
    let pm = (v1, m2) =>
      maybe_match(m2, v2 => pp(v1, v2), e2 => pi(v1, e2));

    let mi = (m1, e2) => wrap(maybe_match(m1, wrap_lit, e1 => e1), e2);
    let im = (e1, m2) => wrap(e1, maybe_match(m2, wrap_lit, e2 => e2));

    let mm = (m1, m2) =>
      maybe_match(m1, v1 => pm(v1, m2), e1 => im(e1, m2));

    let (ness_str, impl) =
      switch (ness) {
      | PP => ("pp", pp)
      | IP => ("ip", ip)
      | MP => ("mp", mp)
      | PI => ("pi", pi)
      | PM => ("pm", pm)
      | II => ("ii", ii)
      | MI => ("mi", mi)
      | IM => ("im", im)
      | MM => ("mm", mm)
      };

    let name = Printf.sprintf("bin_%s_op_%s_%s", ty_str, op_str, ness_str);
    with_fn2(name, x1, x2, impl);
  };

  let bin_bool_op_and_pp = mk_bin_op(Bool(And), PP, "b1", "b2");
  let bin_bool_op_and_ip = mk_bin_op(Bool(And), IP, "b1_", "b2");
  let bin_bool_op_and_mp = mk_bin_op(Bool(And), MP, "b1__", "b2");
  let bin_bool_op_and_pi = mk_bin_op(Bool(And), PI, "b1", "b2_");
  let bin_bool_op_and_pm = mk_bin_op(Bool(And), PM, "b1", "b2__");
  let bin_bool_op_and_ii = mk_bin_op(Bool(And), II, "b1", "b2");
  let bin_bool_op_and_mi = mk_bin_op(Bool(And), MI, "b1__", "b2_");
  let bin_bool_op_and_im = mk_bin_op(Bool(And), IM, "b1_", "b2__");
  let bin_bool_op_and_mm = mk_bin_op(Bool(And), MM, "b1__", "b2__");

  let bin_bool_op_or_pp = mk_bin_op(Bool(Or), PP, "b1", "b2");
  let bin_bool_op_or_ip = mk_bin_op(Bool(Or), IP, "b1_", "b2");
  let bin_bool_op_or_mp = mk_bin_op(Bool(Or), MP, "b1__", "b2");
  let bin_bool_op_or_pi = mk_bin_op(Bool(Or), PI, "b1", "b2_");
  let bin_bool_op_or_pm = mk_bin_op(Bool(Or), PM, "b1", "b2__");
  let bin_bool_op_or_ii = mk_bin_op(Bool(Or), II, "b1", "b2");
  let bin_bool_op_or_mi = mk_bin_op(Bool(Or), MI, "b1__", "b2_");
  let bin_bool_op_or_im = mk_bin_op(Bool(Or), IM, "b1_", "b2__");
  let bin_bool_op_or_mm = mk_bin_op(Bool(Or), MM, "b1__", "b2__");

  let bin_int_op_minus_pp = mk_bin_op(Int(Minus), PP, "n1", "n2");
  let bin_int_op_minus_ip = mk_bin_op(Int(Minus), IP, "n1_", "n2");
  let bin_int_op_minus_mp = mk_bin_op(Int(Minus), MP, "n1__", "n2");
  let bin_int_op_minus_pi = mk_bin_op(Int(Minus), PI, "n1", "n2_");
  let bin_int_op_minus_pm = mk_bin_op(Int(Minus), PM, "n1", "n2__");
  let bin_int_op_minus_ii = mk_bin_op(Int(Minus), II, "n1", "n2");
  let bin_int_op_minus_mi = mk_bin_op(Int(Minus), MI, "n1__", "n2_");
  let bin_int_op_minus_im = mk_bin_op(Int(Minus), IM, "n1_", "n2__");
  let bin_int_op_minus_mm = mk_bin_op(Int(Minus), MM, "n1__", "n2__");

  let bin_int_op_plus_pp = mk_bin_op(Int(Plus), PP, "n1", "n2");
  let bin_int_op_plus_ip = mk_bin_op(Int(Plus), IP, "n1_", "n2");
  let bin_int_op_plus_mp = mk_bin_op(Int(Plus), MP, "n1__", "n2");
  let bin_int_op_plus_pi = mk_bin_op(Int(Plus), PI, "n1", "n2_");
  let bin_int_op_plus_pm = mk_bin_op(Int(Plus), PM, "n1", "n2__");
  let bin_int_op_plus_ii = mk_bin_op(Int(Plus), II, "n1", "n2");
  let bin_int_op_plus_mi = mk_bin_op(Int(Plus), MI, "n1__", "n2_");
  let bin_int_op_plus_im = mk_bin_op(Int(Plus), IM, "n1_", "n2__");
  let bin_int_op_plus_mm = mk_bin_op(Int(Plus), MM, "n1__", "n2__");

  let bin_int_op_times_pp = mk_bin_op(Int(Times), PP, "n1", "n2");
  let bin_int_op_times_ip = mk_bin_op(Int(Times), IP, "n1_", "n2");
  let bin_int_op_times_mp = mk_bin_op(Int(Times), MP, "n1__", "n2");
  let bin_int_op_times_pi = mk_bin_op(Int(Times), PI, "n1", "n2_");
  let bin_int_op_times_pm = mk_bin_op(Int(Times), PM, "n1", "n2__");
  let bin_int_op_times_ii = mk_bin_op(Int(Times), II, "n1", "n2");
  let bin_int_op_times_mi = mk_bin_op(Int(Times), MI, "n1__", "n2_");
  let bin_int_op_times_im = mk_bin_op(Int(Times), IM, "n1_", "n2__");
  let bin_int_op_times_mm = mk_bin_op(Int(Times), MM, "n1__", "n2__");

  let bin_int_op_divide_pp = mk_bin_op(Int(Divide), PP, "n1", "n2");
  let bin_int_op_divide_ip = mk_bin_op(Int(Divide), IP, "n1_", "n2");
  let bin_int_op_divide_mp = mk_bin_op(Int(Divide), MP, "n1__", "n2");
  let bin_int_op_divide_pi = mk_bin_op(Int(Divide), PI, "n1", "n2_");
  let bin_int_op_divide_pm = mk_bin_op(Int(Divide), PM, "n1", "n2__");
  let bin_int_op_divide_ii = mk_bin_op(Int(Divide), II, "n1", "n2");
  let bin_int_op_divide_mi = mk_bin_op(Int(Divide), MI, "n1__", "n2_");
  let bin_int_op_divide_im = mk_bin_op(Int(Divide), IM, "n1_", "n2__");
  let bin_int_op_divide_mm = mk_bin_op(Int(Divide), MM, "n1__", "n2__");

  let bin_int_op_less_than_pp = mk_bin_op(Int(LessThan), PP, "n1", "n2");
  let bin_int_op_less_than_ip = mk_bin_op(Int(LessThan), IP, "n1_", "n2");
  let bin_int_op_less_than_mp = mk_bin_op(Int(LessThan), MP, "n1__", "n2");
  let bin_int_op_less_than_pi = mk_bin_op(Int(LessThan), PI, "n1", "n2_");
  let bin_int_op_less_than_pm = mk_bin_op(Int(LessThan), PM, "n1", "n2__");
  let bin_int_op_less_than_ii = mk_bin_op(Int(LessThan), II, "n1", "n2");
  let bin_int_op_less_than_mi = mk_bin_op(Int(LessThan), MI, "n1__", "n2_");
  let bin_int_op_less_than_im = mk_bin_op(Int(LessThan), IM, "n1_", "n2__");
  let bin_int_op_less_than_mm = mk_bin_op(Int(LessThan), MM, "n1__", "n2__");

  let bin_int_op_greater_than_pp =
    mk_bin_op(Int(GreaterThan), PP, "n1", "n2");
  let bin_int_op_greater_than_ip =
    mk_bin_op(Int(GreaterThan), IP, "n1_", "n2");
  let bin_int_op_greater_than_mp =
    mk_bin_op(Int(GreaterThan), MP, "n1__", "n2");
  let bin_int_op_greater_than_pi =
    mk_bin_op(Int(GreaterThan), PI, "n1", "n2_");
  let bin_int_op_greater_than_pm =
    mk_bin_op(Int(GreaterThan), PM, "n1", "n2__");
  let bin_int_op_greater_than_ii =
    mk_bin_op(Int(GreaterThan), II, "n1", "n2");
  let bin_int_op_greater_than_mi =
    mk_bin_op(Int(GreaterThan), MI, "n1__", "n2_");
  let bin_int_op_greater_than_im =
    mk_bin_op(Int(GreaterThan), IM, "n1_", "n2__");
  let bin_int_op_greater_than_mm =
    mk_bin_op(Int(GreaterThan), MM, "n1__", "n2__");

  let bin_int_op_equals_pp = mk_bin_op(Int(Equals), PP, "n1", "n2");
  let bin_int_op_equals_ip = mk_bin_op(Int(Equals), IP, "n1_", "n2");
  let bin_int_op_equals_mp = mk_bin_op(Int(Equals), MP, "n1__", "n2");
  let bin_int_op_equals_pi = mk_bin_op(Int(Equals), PI, "n1", "n2_");
  let bin_int_op_equals_pm = mk_bin_op(Int(Equals), PM, "n1", "n2__");
  let bin_int_op_equals_ii = mk_bin_op(Int(Equals), II, "n1", "n2");
  let bin_int_op_equals_mi = mk_bin_op(Int(Equals), MI, "n1__", "n2_");
  let bin_int_op_equals_im = mk_bin_op(Int(Equals), IM, "n1_", "n2__");
  let bin_int_op_equals_mm = mk_bin_op(Int(Equals), MM, "n1__", "n2__");

  let bin_float_op_minus_pp = mk_bin_op(Float(Minus), PP, "f1", "f2");
  let bin_float_op_minus_ip = mk_bin_op(Float(Minus), IP, "f1_", "f2");
  let bin_float_op_minus_mp = mk_bin_op(Float(Minus), MP, "f1__", "f2");
  let bin_float_op_minus_pi = mk_bin_op(Float(Minus), PI, "f1", "f2_");
  let bin_float_op_minus_pm = mk_bin_op(Float(Minus), PM, "f1", "f2__");
  let bin_float_op_minus_ii = mk_bin_op(Float(Minus), II, "f1", "f2");
  let bin_float_op_minus_mi = mk_bin_op(Float(Minus), MI, "f1__", "f2_");
  let bin_float_op_minus_im = mk_bin_op(Float(Minus), IM, "f1_", "f2__");
  let bin_float_op_minus_mm = mk_bin_op(Float(Minus), MM, "f1__", "f2__");

  let bin_float_op_plus_pp = mk_bin_op(Float(Plus), PP, "f1", "f2");
  let bin_float_op_plus_ip = mk_bin_op(Float(Plus), IP, "f1_", "f2");
  let bin_float_op_plus_mp = mk_bin_op(Float(Plus), MP, "f1__", "f2");
  let bin_float_op_plus_pi = mk_bin_op(Float(Plus), PI, "f1", "f2_");
  let bin_float_op_plus_pm = mk_bin_op(Float(Plus), PM, "f1", "f2__");
  let bin_float_op_plus_ii = mk_bin_op(Float(Plus), II, "f1", "f2");
  let bin_float_op_plus_mi = mk_bin_op(Float(Plus), MI, "f1__", "f2_");
  let bin_float_op_plus_im = mk_bin_op(Float(Plus), IM, "f1_", "f2__");
  let bin_float_op_plus_mm = mk_bin_op(Float(Plus), MM, "f1__", "f2__");

  let bin_float_op_times_pp = mk_bin_op(Float(Times), PP, "f1", "f2");
  let bin_float_op_times_ip = mk_bin_op(Float(Times), IP, "f1_", "f2");
  let bin_float_op_times_mp = mk_bin_op(Float(Times), MP, "f1__", "f2");
  let bin_float_op_times_pi = mk_bin_op(Float(Times), PI, "f1", "f2_");
  let bin_float_op_times_pm = mk_bin_op(Float(Times), PM, "f1", "f2__");
  let bin_float_op_times_ii = mk_bin_op(Float(Times), II, "f1", "f2");
  let bin_float_op_times_mi = mk_bin_op(Float(Times), MI, "f1__", "f2_");
  let bin_float_op_times_im = mk_bin_op(Float(Times), IM, "f1_", "f2__");
  let bin_float_op_times_mm = mk_bin_op(Float(Times), MM, "f1__", "f2__");

  let bin_float_op_divide_pp = mk_bin_op(Float(Divide), PP, "f1", "f2");
  let bin_float_op_divide_ip = mk_bin_op(Float(Divide), IP, "f1_", "f2");
  let bin_float_op_divide_mp = mk_bin_op(Float(Divide), MP, "f1__", "f2");
  let bin_float_op_divide_pi = mk_bin_op(Float(Divide), PI, "f1", "f2_");
  let bin_float_op_divide_pm = mk_bin_op(Float(Divide), PM, "f1", "f2__");
  let bin_float_op_divide_ii = mk_bin_op(Float(Divide), II, "f1", "f2");
  let bin_float_op_divide_mi = mk_bin_op(Float(Divide), MI, "f1__", "f2_");
  let bin_float_op_divide_im = mk_bin_op(Float(Divide), IM, "f1_", "f2__");
  let bin_float_op_divide_mm = mk_bin_op(Float(Divide), MM, "f1__", "f2__");

  let bin_float_op_less_than_pp = mk_bin_op(Float(LessThan), PP, "n1", "n2");
  let bin_float_op_less_than_ip =
    mk_bin_op(Float(LessThan), IP, "n1_", "n2");
  let bin_float_op_less_than_mp =
    mk_bin_op(Float(LessThan), MP, "n1__", "n2");
  let bin_float_op_less_than_pi =
    mk_bin_op(Float(LessThan), PI, "n1", "n2_");
  let bin_float_op_less_than_pm =
    mk_bin_op(Float(LessThan), PM, "n1", "n2__");
  let bin_float_op_less_than_ii = mk_bin_op(Float(LessThan), II, "n1", "n2");
  let bin_float_op_less_than_mi =
    mk_bin_op(Float(LessThan), MI, "n1__", "n2_");
  let bin_float_op_less_than_im =
    mk_bin_op(Float(LessThan), IM, "n1_", "n2__");
  let bin_float_op_less_than_mm =
    mk_bin_op(Float(LessThan), MM, "n1__", "n2__");

  let bin_float_op_greater_than_pp =
    mk_bin_op(Float(GreaterThan), PP, "n1", "n2");
  let bin_float_op_greater_than_ip =
    mk_bin_op(Float(GreaterThan), IP, "n1_", "n2");
  let bin_float_op_greater_than_mp =
    mk_bin_op(Float(GreaterThan), MP, "n1__", "n2");
  let bin_float_op_greater_than_pi =
    mk_bin_op(Float(GreaterThan), PI, "n1", "n2_");
  let bin_float_op_greater_than_pm =
    mk_bin_op(Float(GreaterThan), PM, "n1", "n2__");
  let bin_float_op_greater_than_ii =
    mk_bin_op(Float(GreaterThan), II, "n1", "n2");
  let bin_float_op_greater_than_mi =
    mk_bin_op(Float(GreaterThan), MI, "n1__", "n2_");
  let bin_float_op_greater_than_im =
    mk_bin_op(Float(GreaterThan), IM, "n1_", "n2__");
  let bin_float_op_greater_than_mm =
    mk_bin_op(Float(GreaterThan), MM, "n1__", "n2__");

  let bin_float_op_equals_pp = mk_bin_op(Float(Equals), PP, "n1", "n2");
  let bin_float_op_equals_ip = mk_bin_op(Float(Equals), IP, "n1_", "n2");
  let bin_float_op_equals_mp = mk_bin_op(Float(Equals), MP, "n1__", "n2");
  let bin_float_op_equals_pi = mk_bin_op(Float(Equals), PI, "n1", "n2_");
  let bin_float_op_equals_pm = mk_bin_op(Float(Equals), PM, "n1", "n2__");
  let bin_float_op_equals_ii = mk_bin_op(Float(Equals), II, "n1", "n2");
  let bin_float_op_equals_mi = mk_bin_op(Float(Equals), MI, "n1__", "n2_");
  let bin_float_op_equals_im = mk_bin_op(Float(Equals), IM, "n1_", "n2__");
  let bin_float_op_equals_mm = mk_bin_op(Float(Equals), MM, "n1__", "n2__");
};

module Use = (I: Make.I) => {
  include Use(I);
  let imp = imp;

  include Impl;
};
