/**
  "hazel/rt/ops.gr", auto-generated.
 */
open Grain;
open Grain.Ident;
open Rt_.Gen({
       let path = "./ops" |> Path.v;
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
  type indetness =
    | P
    | I
    | M;

  let mk_bin_op = (ty, lness, rness, x1, x2) => {
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
        Std_Float32.(
          fun
          | Minus => (bin_float_op_minus, sub)
          | Plus => (bin_float_op_plus, add)
          | Times => (bin_float_op_times, mul)
          | Divide => (bin_float_op_divide, div)
          | LessThan => (bin_float_op_less_than, lt)
          | GreaterThan => (bin_float_op_greater_than, gt)
          | Equals => (bin_float_op_equals, eq)
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

    let wrap_lit = e => Expr.(ap(var(wrap_lit), [e]));

    let maybe_value = Rt_MaybeIndet.value;
    let maybe_indet = Rt_MaybeIndet.indet;
    let maybe_match = Rt_MaybeIndet.match;

    /* v -> v -> v */
    let pp = prim;
    /* i -> i -> i */
    let ii = (e1, e2) => wrap(e1, e2);

    /* i -> v -> i */
    let ip = (e1, v2) => wrap(e1, wrap_lit(v2));
    /* v -> i -> i */
    let pi = (v1, e2) => wrap(wrap_lit(v1), e2);

    /* m -> p -> m */
    let mp = (m1, v2) =>
      maybe_match(
        m1,
        v1 => maybe_value(pp(v1, v2)),
        e1 => maybe_indet(ip(e1, v2)),
      );
    /* p -> m -> m */
    let pm = (v1, m2) =>
      maybe_match(
        m2,
        v2 => maybe_value(pp(v1, v2)),
        e2 => maybe_indet(pi(v1, e2)),
      );

    /* m -> i -> m */
    let mi = (m1, e2) =>
      maybe_indet(wrap(maybe_match(m1, wrap_lit, e1 => e1), e2));
    /* i -> m -> m */
    let im = (e1, m2) =>
      maybe_indet(wrap(e1, maybe_match(m2, wrap_lit, e2 => e2)));

    /* m -> m -> m */
    let mm = (m1, m2) =>
      maybe_match(m1, v1 => pm(v1, m2), e1 => im(e1, m2));

    let (ness_str, impl) =
      switch (lness, rness) {
      | (P, P) => ("pp", pp)
      | (I, P) => ("ip", ip)
      | (M, P) => ("mp", mp)
      | (P, I) => ("pi", pi)
      | (P, M) => ("pm", pm)
      | (I, I) => ("ii", ii)
      | (M, I) => ("mi", mi)
      | (I, M) => ("im", im)
      | (M, M) => ("mm", mm)
      };

    let name = Printf.sprintf("bin_%s_op_%s_%s", ty_str, op_str, ness_str);
    with_fn2(name, x1, x2, impl);
  };

  let bin_bool_op_and_pp = mk_bin_op(Bool(And), P, P, "b1", "b2");
  let bin_bool_op_and_ip = mk_bin_op(Bool(And), I, P, "b1_", "b2");
  let bin_bool_op_and_mp = mk_bin_op(Bool(And), M, P, "b1__", "b2");
  let bin_bool_op_and_pi = mk_bin_op(Bool(And), P, I, "b1", "b2_");
  let bin_bool_op_and_pm = mk_bin_op(Bool(And), P, M, "b1", "b2__");
  let bin_bool_op_and_ii = mk_bin_op(Bool(And), I, I, "b1", "b2");
  let bin_bool_op_and_mi = mk_bin_op(Bool(And), M, I, "b1__", "b2_");
  let bin_bool_op_and_im = mk_bin_op(Bool(And), I, M, "b1_", "b2__");
  let bin_bool_op_and_mm = mk_bin_op(Bool(And), M, M, "b1__", "b2__");

  let bin_bool_op_or_pp = mk_bin_op(Bool(Or), P, P, "b1", "b2");
  let bin_bool_op_or_ip = mk_bin_op(Bool(Or), I, P, "b1_", "b2");
  let bin_bool_op_or_mp = mk_bin_op(Bool(Or), M, P, "b1__", "b2");
  let bin_bool_op_or_pi = mk_bin_op(Bool(Or), P, I, "b1", "b2_");
  let bin_bool_op_or_pm = mk_bin_op(Bool(Or), P, M, "b1", "b2__");
  let bin_bool_op_or_ii = mk_bin_op(Bool(Or), I, I, "b1", "b2");
  let bin_bool_op_or_mi = mk_bin_op(Bool(Or), M, I, "b1__", "b2_");
  let bin_bool_op_or_im = mk_bin_op(Bool(Or), I, M, "b1_", "b2__");
  let bin_bool_op_or_mm = mk_bin_op(Bool(Or), M, M, "b1__", "b2__");

  let bin_int_op_minus_pp = mk_bin_op(Int(Minus), P, P, "n1", "n2");
  let bin_int_op_minus_ip = mk_bin_op(Int(Minus), I, P, "n1_", "n2");
  let bin_int_op_minus_mp = mk_bin_op(Int(Minus), M, P, "n1__", "n2");
  let bin_int_op_minus_pi = mk_bin_op(Int(Minus), P, I, "n1", "n2_");
  let bin_int_op_minus_pm = mk_bin_op(Int(Minus), P, M, "n1", "n2__");
  let bin_int_op_minus_ii = mk_bin_op(Int(Minus), I, I, "n1", "n2");
  let bin_int_op_minus_mi = mk_bin_op(Int(Minus), M, I, "n1__", "n2_");
  let bin_int_op_minus_im = mk_bin_op(Int(Minus), I, M, "n1_", "n2__");
  let bin_int_op_minus_mm = mk_bin_op(Int(Minus), M, M, "n1__", "n2__");

  let bin_int_op_plus_pp = mk_bin_op(Int(Plus), P, P, "n1", "n2");
  let bin_int_op_plus_ip = mk_bin_op(Int(Plus), I, P, "n1_", "n2");
  let bin_int_op_plus_mp = mk_bin_op(Int(Plus), M, P, "n1__", "n2");
  let bin_int_op_plus_pi = mk_bin_op(Int(Plus), P, I, "n1", "n2_");
  let bin_int_op_plus_pm = mk_bin_op(Int(Plus), P, M, "n1", "n2__");
  let bin_int_op_plus_ii = mk_bin_op(Int(Plus), I, I, "n1", "n2");
  let bin_int_op_plus_mi = mk_bin_op(Int(Plus), M, I, "n1__", "n2_");
  let bin_int_op_plus_im = mk_bin_op(Int(Plus), I, M, "n1_", "n2__");
  let bin_int_op_plus_mm = mk_bin_op(Int(Plus), M, M, "n1__", "n2__");

  let bin_int_op_times_pp = mk_bin_op(Int(Times), P, P, "n1", "n2");
  let bin_int_op_times_ip = mk_bin_op(Int(Times), I, P, "n1_", "n2");
  let bin_int_op_times_mp = mk_bin_op(Int(Times), M, P, "n1__", "n2");
  let bin_int_op_times_pi = mk_bin_op(Int(Times), P, I, "n1", "n2_");
  let bin_int_op_times_pm = mk_bin_op(Int(Times), P, M, "n1", "n2__");
  let bin_int_op_times_ii = mk_bin_op(Int(Times), I, I, "n1", "n2");
  let bin_int_op_times_mi = mk_bin_op(Int(Times), M, I, "n1__", "n2_");
  let bin_int_op_times_im = mk_bin_op(Int(Times), I, M, "n1_", "n2__");
  let bin_int_op_times_mm = mk_bin_op(Int(Times), M, M, "n1__", "n2__");

  let bin_int_op_divide_pp = mk_bin_op(Int(Divide), P, P, "n1", "n2");
  let bin_int_op_divide_ip = mk_bin_op(Int(Divide), I, P, "n1_", "n2");
  let bin_int_op_divide_mp = mk_bin_op(Int(Divide), M, P, "n1__", "n2");
  let bin_int_op_divide_pi = mk_bin_op(Int(Divide), P, I, "n1", "n2_");
  let bin_int_op_divide_pm = mk_bin_op(Int(Divide), P, M, "n1", "n2__");
  let bin_int_op_divide_ii = mk_bin_op(Int(Divide), I, I, "n1", "n2");
  let bin_int_op_divide_mi = mk_bin_op(Int(Divide), M, I, "n1__", "n2_");
  let bin_int_op_divide_im = mk_bin_op(Int(Divide), I, M, "n1_", "n2__");
  let bin_int_op_divide_mm = mk_bin_op(Int(Divide), M, M, "n1__", "n2__");

  let bin_int_op_less_than_pp = mk_bin_op(Int(LessThan), P, P, "n1", "n2");
  let bin_int_op_less_than_ip = mk_bin_op(Int(LessThan), I, P, "n1_", "n2");
  let bin_int_op_less_than_mp = mk_bin_op(Int(LessThan), M, P, "n1__", "n2");
  let bin_int_op_less_than_pi = mk_bin_op(Int(LessThan), P, I, "n1", "n2_");
  let bin_int_op_less_than_pm = mk_bin_op(Int(LessThan), P, M, "n1", "n2__");
  let bin_int_op_less_than_ii = mk_bin_op(Int(LessThan), I, I, "n1", "n2");
  let bin_int_op_less_than_mi =
    mk_bin_op(Int(LessThan), M, I, "n1__", "n2_");
  let bin_int_op_less_than_im =
    mk_bin_op(Int(LessThan), I, M, "n1_", "n2__");
  let bin_int_op_less_than_mm =
    mk_bin_op(Int(LessThan), M, M, "n1__", "n2__");

  let bin_int_op_greater_than_pp =
    mk_bin_op(Int(GreaterThan), P, P, "n1", "n2");
  let bin_int_op_greater_than_ip =
    mk_bin_op(Int(GreaterThan), I, P, "n1_", "n2");
  let bin_int_op_greater_than_mp =
    mk_bin_op(Int(GreaterThan), M, P, "n1__", "n2");
  let bin_int_op_greater_than_pi =
    mk_bin_op(Int(GreaterThan), P, I, "n1", "n2_");
  let bin_int_op_greater_than_pm =
    mk_bin_op(Int(GreaterThan), P, M, "n1", "n2__");
  let bin_int_op_greater_than_ii =
    mk_bin_op(Int(GreaterThan), I, I, "n1", "n2");
  let bin_int_op_greater_than_mi =
    mk_bin_op(Int(GreaterThan), M, I, "n1__", "n2_");
  let bin_int_op_greater_than_im =
    mk_bin_op(Int(GreaterThan), I, M, "n1_", "n2__");
  let bin_int_op_greater_than_mm =
    mk_bin_op(Int(GreaterThan), M, M, "n1__", "n2__");

  let bin_int_op_equals_pp = mk_bin_op(Int(Equals), P, P, "n1", "n2");
  let bin_int_op_equals_ip = mk_bin_op(Int(Equals), I, P, "n1_", "n2");
  let bin_int_op_equals_mp = mk_bin_op(Int(Equals), M, P, "n1__", "n2");
  let bin_int_op_equals_pi = mk_bin_op(Int(Equals), P, I, "n1", "n2_");
  let bin_int_op_equals_pm = mk_bin_op(Int(Equals), P, M, "n1", "n2__");
  let bin_int_op_equals_ii = mk_bin_op(Int(Equals), I, I, "n1", "n2");
  let bin_int_op_equals_mi = mk_bin_op(Int(Equals), M, I, "n1__", "n2_");
  let bin_int_op_equals_im = mk_bin_op(Int(Equals), I, M, "n1_", "n2__");
  let bin_int_op_equals_mm = mk_bin_op(Int(Equals), M, M, "n1__", "n2__");

  let bin_float_op_minus_pp = mk_bin_op(Float(Minus), P, P, "f1", "f2");
  let bin_float_op_minus_ip = mk_bin_op(Float(Minus), I, P, "f1_", "f2");
  let bin_float_op_minus_mp = mk_bin_op(Float(Minus), M, P, "f1__", "f2");
  let bin_float_op_minus_pi = mk_bin_op(Float(Minus), P, I, "f1", "f2_");
  let bin_float_op_minus_pm = mk_bin_op(Float(Minus), P, M, "f1", "f2__");
  let bin_float_op_minus_ii = mk_bin_op(Float(Minus), I, I, "f1", "f2");
  let bin_float_op_minus_mi = mk_bin_op(Float(Minus), M, I, "f1__", "f2_");
  let bin_float_op_minus_im = mk_bin_op(Float(Minus), I, M, "f1_", "f2__");
  let bin_float_op_minus_mm = mk_bin_op(Float(Minus), M, M, "f1__", "f2__");

  let bin_float_op_plus_pp = mk_bin_op(Float(Plus), P, P, "f1", "f2");
  let bin_float_op_plus_ip = mk_bin_op(Float(Plus), I, P, "f1_", "f2");
  let bin_float_op_plus_mp = mk_bin_op(Float(Plus), M, P, "f1__", "f2");
  let bin_float_op_plus_pi = mk_bin_op(Float(Plus), P, I, "f1", "f2_");
  let bin_float_op_plus_pm = mk_bin_op(Float(Plus), P, M, "f1", "f2__");
  let bin_float_op_plus_ii = mk_bin_op(Float(Plus), I, I, "f1", "f2");
  let bin_float_op_plus_mi = mk_bin_op(Float(Plus), M, I, "f1__", "f2_");
  let bin_float_op_plus_im = mk_bin_op(Float(Plus), I, M, "f1_", "f2__");
  let bin_float_op_plus_mm = mk_bin_op(Float(Plus), M, M, "f1__", "f2__");

  let bin_float_op_times_pp = mk_bin_op(Float(Times), P, P, "f1", "f2");
  let bin_float_op_times_ip = mk_bin_op(Float(Times), I, P, "f1_", "f2");
  let bin_float_op_times_mp = mk_bin_op(Float(Times), M, P, "f1__", "f2");
  let bin_float_op_times_pi = mk_bin_op(Float(Times), P, I, "f1", "f2_");
  let bin_float_op_times_pm = mk_bin_op(Float(Times), P, M, "f1", "f2__");
  let bin_float_op_times_ii = mk_bin_op(Float(Times), I, I, "f1", "f2");
  let bin_float_op_times_mi = mk_bin_op(Float(Times), M, I, "f1__", "f2_");
  let bin_float_op_times_im = mk_bin_op(Float(Times), I, M, "f1_", "f2__");
  let bin_float_op_times_mm = mk_bin_op(Float(Times), M, M, "f1__", "f2__");

  let bin_float_op_divide_pp = mk_bin_op(Float(Divide), P, P, "f1", "f2");
  let bin_float_op_divide_ip = mk_bin_op(Float(Divide), I, P, "f1_", "f2");
  let bin_float_op_divide_mp = mk_bin_op(Float(Divide), M, P, "f1__", "f2");
  let bin_float_op_divide_pi = mk_bin_op(Float(Divide), P, I, "f1", "f2_");
  let bin_float_op_divide_pm = mk_bin_op(Float(Divide), P, M, "f1", "f2__");
  let bin_float_op_divide_ii = mk_bin_op(Float(Divide), I, I, "f1", "f2");
  let bin_float_op_divide_mi = mk_bin_op(Float(Divide), M, I, "f1__", "f2_");
  let bin_float_op_divide_im = mk_bin_op(Float(Divide), I, M, "f1_", "f2__");
  let bin_float_op_divide_mm =
    mk_bin_op(Float(Divide), M, M, "f1__", "f2__");

  let bin_float_op_less_than_pp =
    mk_bin_op(Float(LessThan), P, P, "n1", "n2");
  let bin_float_op_less_than_ip =
    mk_bin_op(Float(LessThan), I, P, "n1_", "n2");
  let bin_float_op_less_than_mp =
    mk_bin_op(Float(LessThan), M, P, "n1__", "n2");
  let bin_float_op_less_than_pi =
    mk_bin_op(Float(LessThan), P, I, "n1", "n2_");
  let bin_float_op_less_than_pm =
    mk_bin_op(Float(LessThan), P, M, "n1", "n2__");
  let bin_float_op_less_than_ii =
    mk_bin_op(Float(LessThan), I, I, "n1", "n2");
  let bin_float_op_less_than_mi =
    mk_bin_op(Float(LessThan), M, I, "n1__", "n2_");
  let bin_float_op_less_than_im =
    mk_bin_op(Float(LessThan), I, M, "n1_", "n2__");
  let bin_float_op_less_than_mm =
    mk_bin_op(Float(LessThan), M, M, "n1__", "n2__");

  let bin_float_op_greater_than_pp =
    mk_bin_op(Float(GreaterThan), P, P, "n1", "n2");
  let bin_float_op_greater_than_ip =
    mk_bin_op(Float(GreaterThan), I, P, "n1_", "n2");
  let bin_float_op_greater_than_mp =
    mk_bin_op(Float(GreaterThan), M, P, "n1__", "n2");
  let bin_float_op_greater_than_pi =
    mk_bin_op(Float(GreaterThan), P, I, "n1", "n2_");
  let bin_float_op_greater_than_pm =
    mk_bin_op(Float(GreaterThan), P, M, "n1", "n2__");
  let bin_float_op_greater_than_ii =
    mk_bin_op(Float(GreaterThan), I, I, "n1", "n2");
  let bin_float_op_greater_than_mi =
    mk_bin_op(Float(GreaterThan), M, I, "n1__", "n2_");
  let bin_float_op_greater_than_im =
    mk_bin_op(Float(GreaterThan), I, M, "n1_", "n2__");
  let bin_float_op_greater_than_mm =
    mk_bin_op(Float(GreaterThan), M, M, "n1__", "n2__");

  let bin_float_op_equals_pp = mk_bin_op(Float(Equals), P, P, "n1", "n2");
  let bin_float_op_equals_ip = mk_bin_op(Float(Equals), I, P, "n1_", "n2");
  let bin_float_op_equals_mp = mk_bin_op(Float(Equals), M, P, "n1__", "n2");
  let bin_float_op_equals_pi = mk_bin_op(Float(Equals), P, I, "n1", "n2_");
  let bin_float_op_equals_pm = mk_bin_op(Float(Equals), P, M, "n1", "n2__");
  let bin_float_op_equals_ii = mk_bin_op(Float(Equals), I, I, "n1", "n2");
  let bin_float_op_equals_mi = mk_bin_op(Float(Equals), M, I, "n1__", "n2_");
  let bin_float_op_equals_im = mk_bin_op(Float(Equals), I, M, "n1_", "n2__");
  let bin_float_op_equals_mm =
    mk_bin_op(Float(Equals), M, M, "n1__", "n2__");
};

module Use = (I: Make.I) => {
  open Use(I);
  open Impl;

  let imp = imp;

  let wrap_empty_hole = ap3(wrap_empty_hole);
  let wrap_non_empty_hole = ap3(wrap_non_empty_hole);
  let wrap_cast = ap3(wrap_cast);
  let wrap_failed_cast = ap3(wrap_failed_cast);

  let wrap_ap = ap2(wrap_ap);

  let wrap_bool_lit = ap1(wrap_bool_lit);
  let wrap_int_lit = ap1(wrap_int_lit);
  let wrap_float_lit = ap1(wrap_float_lit);

  let wrap_bin_bool_op = ap1(wrap_bin_bool_op);
  let wrap_bin_int_op = ap1(wrap_bin_int_op);
  let wrap_bin_float_op = ap1(wrap_bin_float_op);

  let bin_bool_op_and_pp = ap2(bin_bool_op_and_pp);
  let bin_bool_op_and_ip = ap2(bin_bool_op_and_ip);
  let bin_bool_op_and_mp = ap2(bin_bool_op_and_mp);
  let bin_bool_op_and_pi = ap2(bin_bool_op_and_pi);
  let bin_bool_op_and_pm = ap2(bin_bool_op_and_pm);
  let bin_bool_op_and_ii = ap2(bin_bool_op_and_ii);
  let bin_bool_op_and_mi = ap2(bin_bool_op_and_mi);
  let bin_bool_op_and_im = ap2(bin_bool_op_and_im);
  let bin_bool_op_and_mm = ap2(bin_bool_op_and_mm);

  let bin_bool_op_or_pp = ap2(bin_bool_op_or_pp);
  let bin_bool_op_or_ip = ap2(bin_bool_op_or_ip);
  let bin_bool_op_or_mp = ap2(bin_bool_op_or_mp);
  let bin_bool_op_or_pi = ap2(bin_bool_op_or_pi);
  let bin_bool_op_or_pm = ap2(bin_bool_op_or_pm);
  let bin_bool_op_or_ii = ap2(bin_bool_op_or_ii);
  let bin_bool_op_or_mi = ap2(bin_bool_op_or_mi);
  let bin_bool_op_or_im = ap2(bin_bool_op_or_im);
  let bin_bool_op_or_mm = ap2(bin_bool_op_or_mm);

  let bin_int_op_minus_pp = ap2(bin_int_op_minus_pp);
  let bin_int_op_minus_ip = ap2(bin_int_op_minus_ip);
  let bin_int_op_minus_mp = ap2(bin_int_op_minus_mp);
  let bin_int_op_minus_pi = ap2(bin_int_op_minus_pi);
  let bin_int_op_minus_pm = ap2(bin_int_op_minus_pm);
  let bin_int_op_minus_ii = ap2(bin_int_op_minus_ii);
  let bin_int_op_minus_mi = ap2(bin_int_op_minus_mi);
  let bin_int_op_minus_im = ap2(bin_int_op_minus_im);
  let bin_int_op_minus_mm = ap2(bin_int_op_minus_mm);

  let bin_int_op_plus_pp = ap2(bin_int_op_plus_pp);
  let bin_int_op_plus_ip = ap2(bin_int_op_plus_ip);
  let bin_int_op_plus_mp = ap2(bin_int_op_plus_mp);
  let bin_int_op_plus_pi = ap2(bin_int_op_plus_pi);
  let bin_int_op_plus_pm = ap2(bin_int_op_plus_pm);
  let bin_int_op_plus_ii = ap2(bin_int_op_plus_ii);
  let bin_int_op_plus_mi = ap2(bin_int_op_plus_mi);
  let bin_int_op_plus_im = ap2(bin_int_op_plus_im);
  let bin_int_op_plus_mm = ap2(bin_int_op_plus_mm);

  let bin_int_op_times_pp = ap2(bin_int_op_times_pp);
  let bin_int_op_times_ip = ap2(bin_int_op_times_ip);
  let bin_int_op_times_mp = ap2(bin_int_op_times_mp);
  let bin_int_op_times_pi = ap2(bin_int_op_times_pi);
  let bin_int_op_times_pm = ap2(bin_int_op_times_pm);
  let bin_int_op_times_ii = ap2(bin_int_op_times_ii);
  let bin_int_op_times_mi = ap2(bin_int_op_times_mi);
  let bin_int_op_times_im = ap2(bin_int_op_times_im);
  let bin_int_op_times_mm = ap2(bin_int_op_times_mm);

  let bin_int_op_divide_pp = ap2(bin_int_op_divide_pp);
  let bin_int_op_divide_ip = ap2(bin_int_op_divide_ip);
  let bin_int_op_divide_mp = ap2(bin_int_op_divide_mp);
  let bin_int_op_divide_pi = ap2(bin_int_op_divide_pi);
  let bin_int_op_divide_pm = ap2(bin_int_op_divide_pm);
  let bin_int_op_divide_ii = ap2(bin_int_op_divide_ii);
  let bin_int_op_divide_mi = ap2(bin_int_op_divide_mi);
  let bin_int_op_divide_im = ap2(bin_int_op_divide_im);
  let bin_int_op_divide_mm = ap2(bin_int_op_divide_mm);

  let bin_int_op_less_than_pp = ap2(bin_int_op_less_than_pp);
  let bin_int_op_less_than_ip = ap2(bin_int_op_less_than_ip);
  let bin_int_op_less_than_mp = ap2(bin_int_op_less_than_mp);
  let bin_int_op_less_than_pi = ap2(bin_int_op_less_than_pi);
  let bin_int_op_less_than_pm = ap2(bin_int_op_less_than_pm);
  let bin_int_op_less_than_ii = ap2(bin_int_op_less_than_ii);
  let bin_int_op_less_than_mi = ap2(bin_int_op_less_than_mi);
  let bin_int_op_less_than_im = ap2(bin_int_op_less_than_im);
  let bin_int_op_less_than_mm = ap2(bin_int_op_less_than_mm);

  let bin_int_op_greater_than_pp = ap2(bin_int_op_greater_than_pp);
  let bin_int_op_greater_than_ip = ap2(bin_int_op_greater_than_ip);
  let bin_int_op_greater_than_mp = ap2(bin_int_op_greater_than_mp);
  let bin_int_op_greater_than_pi = ap2(bin_int_op_greater_than_pi);
  let bin_int_op_greater_than_pm = ap2(bin_int_op_greater_than_pm);
  let bin_int_op_greater_than_ii = ap2(bin_int_op_greater_than_ii);
  let bin_int_op_greater_than_mi = ap2(bin_int_op_greater_than_mi);
  let bin_int_op_greater_than_im = ap2(bin_int_op_greater_than_im);
  let bin_int_op_greater_than_mm = ap2(bin_int_op_greater_than_mm);

  let bin_int_op_equals_pp = ap2(bin_int_op_equals_pp);
  let bin_int_op_equals_ip = ap2(bin_int_op_equals_ip);
  let bin_int_op_equals_mp = ap2(bin_int_op_equals_mp);
  let bin_int_op_equals_pi = ap2(bin_int_op_equals_pi);
  let bin_int_op_equals_pm = ap2(bin_int_op_equals_pm);
  let bin_int_op_equals_ii = ap2(bin_int_op_equals_ii);
  let bin_int_op_equals_mi = ap2(bin_int_op_equals_mi);
  let bin_int_op_equals_im = ap2(bin_int_op_equals_im);
  let bin_int_op_equals_mm = ap2(bin_int_op_equals_mm);

  let bin_float_op_minus_pp = ap2(bin_float_op_minus_pp);
  let bin_float_op_minus_ip = ap2(bin_float_op_minus_ip);
  let bin_float_op_minus_mp = ap2(bin_float_op_minus_mp);
  let bin_float_op_minus_pi = ap2(bin_float_op_minus_pi);
  let bin_float_op_minus_pm = ap2(bin_float_op_minus_pm);
  let bin_float_op_minus_ii = ap2(bin_float_op_minus_ii);
  let bin_float_op_minus_mi = ap2(bin_float_op_minus_mi);
  let bin_float_op_minus_im = ap2(bin_float_op_minus_im);
  let bin_float_op_minus_mm = ap2(bin_float_op_minus_mm);

  let bin_float_op_plus_pp = ap2(bin_float_op_plus_pp);
  let bin_float_op_plus_ip = ap2(bin_float_op_plus_ip);
  let bin_float_op_plus_mp = ap2(bin_float_op_plus_mp);
  let bin_float_op_plus_pi = ap2(bin_float_op_plus_pi);
  let bin_float_op_plus_pm = ap2(bin_float_op_plus_pm);
  let bin_float_op_plus_ii = ap2(bin_float_op_plus_ii);
  let bin_float_op_plus_mi = ap2(bin_float_op_plus_mi);
  let bin_float_op_plus_im = ap2(bin_float_op_plus_im);
  let bin_float_op_plus_mm = ap2(bin_float_op_plus_mm);

  let bin_float_op_times_pp = ap2(bin_float_op_times_pp);
  let bin_float_op_times_ip = ap2(bin_float_op_times_ip);
  let bin_float_op_times_mp = ap2(bin_float_op_times_mp);
  let bin_float_op_times_pi = ap2(bin_float_op_times_pi);
  let bin_float_op_times_pm = ap2(bin_float_op_times_pm);
  let bin_float_op_times_ii = ap2(bin_float_op_times_ii);
  let bin_float_op_times_mi = ap2(bin_float_op_times_mi);
  let bin_float_op_times_im = ap2(bin_float_op_times_im);
  let bin_float_op_times_mm = ap2(bin_float_op_times_mm);

  let bin_float_op_divide_pp = ap2(bin_float_op_divide_pp);
  let bin_float_op_divide_ip = ap2(bin_float_op_divide_ip);
  let bin_float_op_divide_mp = ap2(bin_float_op_divide_mp);
  let bin_float_op_divide_pi = ap2(bin_float_op_divide_pi);
  let bin_float_op_divide_pm = ap2(bin_float_op_divide_pm);
  let bin_float_op_divide_ii = ap2(bin_float_op_divide_ii);
  let bin_float_op_divide_mi = ap2(bin_float_op_divide_mi);
  let bin_float_op_divide_im = ap2(bin_float_op_divide_im);
  let bin_float_op_divide_mm = ap2(bin_float_op_divide_mm);

  let bin_float_op_less_than_pp = ap2(bin_float_op_less_than_pp);
  let bin_float_op_less_than_ip = ap2(bin_float_op_less_than_ip);
  let bin_float_op_less_than_mp = ap2(bin_float_op_less_than_mp);
  let bin_float_op_less_than_pi = ap2(bin_float_op_less_than_pi);
  let bin_float_op_less_than_pm = ap2(bin_float_op_less_than_pm);
  let bin_float_op_less_than_ii = ap2(bin_float_op_less_than_ii);
  let bin_float_op_less_than_mi = ap2(bin_float_op_less_than_mi);
  let bin_float_op_less_than_im = ap2(bin_float_op_less_than_im);
  let bin_float_op_less_than_mm = ap2(bin_float_op_less_than_mm);

  let bin_float_op_greater_than_pp = ap2(bin_float_op_greater_than_pp);
  let bin_float_op_greater_than_ip = ap2(bin_float_op_greater_than_ip);
  let bin_float_op_greater_than_mp = ap2(bin_float_op_greater_than_mp);
  let bin_float_op_greater_than_pi = ap2(bin_float_op_greater_than_pi);
  let bin_float_op_greater_than_pm = ap2(bin_float_op_greater_than_pm);
  let bin_float_op_greater_than_ii = ap2(bin_float_op_greater_than_ii);
  let bin_float_op_greater_than_mi = ap2(bin_float_op_greater_than_mi);
  let bin_float_op_greater_than_im = ap2(bin_float_op_greater_than_im);
  let bin_float_op_greater_than_mm = ap2(bin_float_op_greater_than_mm);

  let bin_float_op_equals_pp = ap2(bin_float_op_equals_pp);
  let bin_float_op_equals_ip = ap2(bin_float_op_equals_ip);
  let bin_float_op_equals_mp = ap2(bin_float_op_equals_mp);
  let bin_float_op_equals_pi = ap2(bin_float_op_equals_pi);
  let bin_float_op_equals_pm = ap2(bin_float_op_equals_pm);
  let bin_float_op_equals_ii = ap2(bin_float_op_equals_ii);
  let bin_float_op_equals_mi = ap2(bin_float_op_equals_mi);
  let bin_float_op_equals_im = ap2(bin_float_op_equals_im);
  let bin_float_op_equals_mm = ap2(bin_float_op_equals_mm);
};
