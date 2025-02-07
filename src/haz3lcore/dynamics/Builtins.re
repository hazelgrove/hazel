open Util;
open OptUtil.Syntax;
open DHExp;

/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `builtins`.

   See the existing ones for reference.
 */

[@deriving (show({with_path: false}), sexp)]
type builtin =
  | Const(Typ.t, DHExp.t)
  | Fn(Typ.t, Typ.t, DHExp.t => option(DHExp.t));

[@deriving (show({with_path: false}), sexp)]
type t = VarMap.t_(builtin);

[@deriving (show({with_path: false}), sexp)]
type forms = VarMap.t_(DHExp.t => option(DHExp.t));

let const = (name: Var.t, typ: Typ.term, v: DHExp.t, builtins: t): t =>
  VarMap.extend(builtins, (name, Const(typ |> Typ.fresh, v)));
let fn =
    (
      name: Var.t,
      t1: Typ.term,
      t2: Typ.term,
      impl: DHExp.t => option(DHExp.t), // None if indet
      builtins: t,
    )
    : t =>
  VarMap.extend(
    builtins,
    (name, Fn(t1 |> Typ.fresh, t2 |> Typ.fresh, impl)),
  );

let (let-unbox) = ((request, v), f) =>
  switch (Unboxing.unbox(request, v)) {
  | IndetMatch
  | DoesNotMatch => None
  | Matches(n) => f(n)
  };

module Pervasives = {
  module Impls = {
    /* constants */
    let infinity = Float(Float.infinity) |> fresh;
    let neg_infinity = Float(Float.neg_infinity) |> fresh;
    let nan = Float(Float.nan) |> fresh;
    let epsilon_float = Float(epsilon_float) |> fresh;
    let pi = Float(Float.pi) |> fresh;
    let max_int = Int(Int.max_int) |> fresh;
    let min_int = Int(Int.min_int) |> fresh;

    [@warning "-8"]
    // let-unbox guarantees that the tuple will have length 2
    let binary = (f: (DHExp.t, DHExp.t) => option(DHExp.t), d: DHExp.t) => {
      let-unbox [d1, d2] = (Tuple(2), d);
      f(d1, d2);
    };

    [@warning "-8"]
    // let-unbox guarantees that the tuple will have length 3
    let ternary =
        (f: (DHExp.t, DHExp.t, DHExp.t) => option(DHExp.t), d: DHExp.t) => {
      let-unbox [d1, d2, d3] = (Tuple(3), d);
      f(d1, d2, d3);
    };

    let is_finite = d => {
      let-unbox f = (Float, d);
      Some(fresh(Bool(Float.is_finite(f))));
    };

    let is_infinite = d => {
      let-unbox f = (Float, d);
      Some(fresh(Bool(Float.is_infinite(f))));
    };

    let is_nan = d => {
      let-unbox f = (Float, d);
      Some(fresh(Bool(Float.is_nan(f))));
    };

    let string_of_int = d => {
      let-unbox n = (Int, d);
      Some(fresh(String(string_of_int(n))));
    };

    let string_of_float = d => {
      let-unbox f = (Float, d);
      Some(fresh(String(string_of_float(f))));
    };

    let string_of_bool = d => {
      let-unbox b = (Bool, d);
      Some(fresh(String(string_of_bool(b))));
    };

    let int_of_float = d => {
      let-unbox f = (Float, d);
      Some(fresh(Int(int_of_float(f))));
    };

    let float_of_int = d => {
      let-unbox n = (Int, d);
      Some(fresh(Float(float_of_int(n))));
    };

    let abs = d => {
      let-unbox n = (Int, d);
      Some(fresh(Int(abs(n))));
    };

    let float_op = (fn, d) => {
      let-unbox f = (Float, d);
      Some(fresh(Float(fn(f))));
    };

    let abs_float = float_op(abs_float);
    let ceil = float_op(ceil);
    let floor = float_op(floor);
    let sqrt = float_op(sqrt);
    let exp = float_op(exp);
    let log = float_op(log);
    let log10 = float_op(log10);
    let sin = float_op(sin);
    let cos = float_op(cos);
    let tan = float_op(tan);
    let asin = float_op(asin);
    let acos = float_op(acos);
    let atan = float_op(atan);

    let of_string =
        (
          convert: string => option('a),
          wrap: 'a => DHExp.t,
          name: string,
          d: DHExp.t,
        ) => {
      let-unbox s = (String, d);
      switch (convert(s)) {
      | Some(n) => Some(wrap(n))
      | None =>
        let d' = BuiltinFun(name) |> DHExp.fresh;
        let d' = Ap(Forward, d', d) |> DHExp.fresh;
        let d' = DynamicErrorHole(d', InvalidOfString) |> DHExp.fresh;
        Some(d');
      };
    };

    let int_of_string =
      of_string(int_of_string_opt, n => Int(n) |> DHExp.fresh);
    let float_of_string =
      of_string(float_of_string_opt, f => Float(f) |> DHExp.fresh);
    let bool_of_string =
      of_string(bool_of_string_opt, b => Bool(b) |> DHExp.fresh);

    let int_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (Int, d1);
        let-unbox n = (Int, d2);
        if (n == 0) {
          Some(
            fresh(
              DynamicErrorHole(
                Ap(Forward, BuiltinFun(name) |> fresh, d1) |> fresh,
                DivideByZero,
              ),
            ),
          );
        } else {
          Some(fresh(Int(m mod n)));
        };
      });

    let string_length = d => {
      let-unbox s = (String, d);
      Some(fresh(Int(String.length(s))));
    };

    let string_compare =
      binary((d1, d2) => {
        let-unbox s1 = (String, d1);
        let-unbox s2 = (String, d2);
        Some(fresh(Int(String.compare(s1, s2))));
      });

    let string_trim = d => {
      let-unbox s = (String, d);
      Some(fresh(String(String.trim(s))));
    };

    let string_of: DHExp.t => option(string) =
      d => {
        let-unbox s = (String, d);
        Some(s);
      };

    let string_concat =
      binary((d1, d2) => {
        let-unbox s1 = (String, d1);
        let-unbox xs = (List, d2);
        let* xs' = List.map(string_of, xs) |> Util.OptUtil.sequence;
        Some(fresh(String(String.concat(s1, xs'))));
      });

    let string_sub = name =>
      ternary((d1, d2, d3) => {
        let-unbox s = (String, d1);
        let-unbox idx = (Int, d2);
        let-unbox len = (Int, d3);
        try(Some(fresh(String(String.sub(s, idx, len))))) {
        | _ =>
          let d' = BuiltinFun(name) |> DHExp.fresh;
          let d' = Ap(Forward, d', d1) |> DHExp.fresh;
          let d' = DynamicErrorHole(d', IndexOutOfBounds) |> DHExp.fresh;
          Some(d');
        };
      });

    let string_split = _ =>
      binary((d1, d2) => {
        let-unbox s = (String, d1);
        let-unbox sep = (String, d2);
        let split_str = Util.StringUtil.plain_split(sep, s);
        let split_str' = List.map(s => String(s) |> DHExp.fresh, split_str);
        Some(fresh(ListLit(split_str')));
      });
  };

  open Impls;

  // Update src/haz3lmenhir/Lexer.mll when any new builtin is added
  let builtins =
    VarMap.empty
    |> const("infinity", Float, infinity)
    |> const("neg_infinity", Float, neg_infinity)
    |> const("nan", Float, nan)
    |> const("epsilon_float", Float, epsilon_float)
    |> const("pi", Float, pi)
    |> const("max_int", Int, max_int)
    |> const("min_int", Int, min_int)
    |> fn("is_finite", Float, Bool, is_finite)
    |> fn("is_infinite", Float, Bool, is_infinite)
    |> fn("is_nan", Float, Bool, is_nan)
    |> fn("int_of_float", Float, Int, int_of_float)
    |> fn("float_of_int", Int, Float, float_of_int)
    |> fn("string_of_int", Int, String, string_of_int)
    |> fn("string_of_float", Float, String, string_of_float)
    |> fn("string_of_bool", Bool, String, string_of_bool)
    |> fn("int_of_string", String, Int, int_of_string("int_of_string"))
    |> fn(
         "float_of_string",
         String,
         Float,
         float_of_string("float_of_string"),
       )
    |> fn("bool_of_string", String, Bool, bool_of_string("bool_of_string"))
    |> fn("abs", Int, Int, abs)
    |> fn("abs_float", Float, Float, abs_float)
    |> fn("ceil", Float, Float, ceil)
    |> fn("floor", Float, Float, floor)
    |> fn("exp", Float, Float, exp)
    |> fn("log", Float, Float, log)
    |> fn("log10", Float, Float, log10)
    |> fn("sqrt", Float, Float, sqrt)
    |> fn("sin", Float, Float, sin)
    |> fn("cos", Float, Float, cos)
    |> fn("tan", Float, Float, tan)
    |> fn("asin", Float, Float, asin)
    |> fn("acos", Float, Float, acos)
    |> fn("atan", Float, Float, atan)
    |> fn(
         "mod",
         Prod([Int |> Typ.fresh, Int |> Typ.fresh]),
         Int,
         int_mod("mod"),
       )
    |> fn("string_length", String, Int, string_length)
    |> fn(
         "string_compare",
         Prod([String |> Typ.fresh, String |> Typ.fresh]),
         Int,
         string_compare,
       )
    |> fn("string_trim", String, String, string_trim)
    |> fn(
         "string_concat",
         Prod([String |> Typ.fresh, List(String |> Typ.fresh) |> Typ.fresh]),
         String,
         string_concat,
       )
    |> fn(
         "string_sub",
         Prod([String |> Typ.fresh, Int |> Typ.fresh, Int |> Typ.fresh]),
         String,
         string_sub("string_sub"),
       )
    |> fn(
         "string_split",
         Prod([String |> Typ.fresh, String |> Typ.fresh]),
         List(String |> Typ.fresh),
         string_split("string_split"),
       );
};

let ctx_init: Ctx.t = {
  let meta_cons_map: ConstructorMap.t(Typ.t) = [
    Variant("$e", [Id.mk()], None),
    Variant("$v", [Id.mk()], None),
  ];
  let meta =
    Ctx.TVarEntry({
      name: "$Meta",
      id: Id.invalid,
      kind: Ctx.Singleton(Sum(meta_cons_map) |> Typ.fresh),
    });
  List.map(
    fun
    | (name, Const(typ, _)) => Ctx.VarEntry({name, typ, id: Id.invalid})
    | (name, Fn(t1, t2, _)) =>
      Ctx.VarEntry({name, typ: Arrow(t1, t2) |> Typ.fresh, id: Id.invalid}),
    Pervasives.builtins,
  )
  |> Ctx.extend(_, meta)
  |> Ctx.add_ctrs(_, "$Meta", Id.invalid, meta_cons_map);
};

let forms_init: forms =
  List.filter_map(
    fun
    | (_, Const(_)) => None
    | (name, Fn(_, _, f)) => Some((name, f)),
    Pervasives.builtins,
  );

let env_init: Environment.t =
  List.fold_left(
    env =>
      fun
      | (name, Const(_, d)) => Environment.extend(env, (name, d))
      | (name, Fn(_)) =>
        Environment.extend(env, (name, BuiltinFun(name) |> fresh)),
    Environment.empty,
    Pervasives.builtins,
  );
