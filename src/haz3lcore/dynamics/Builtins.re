open Util;
open OptUtil.Syntax;

/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `builtins`.

   See the existing ones for reference.
 */
module Fresh = IdTagged.FreshGrammar;

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
    open Fresh.Exp;
    /* constants */
    let infinity = float(Float.infinity);
    let neg_infinity = float(Float.neg_infinity);
    let nan = float(Float.nan);
    let epsilon_float = float(epsilon_float);
    let pi = float(Float.pi);
    let max_int = int(Int.max_int);
    let min_int = int(Int.min_int);

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
      Some(bool(Float.is_finite(f)));
    };

    let is_infinite = d => {
      let-unbox f = (Float, d);
      Some(bool(Float.is_infinite(f)));
    };

    let is_nan = d => {
      let-unbox f = (Float, d);
      Some(bool(Float.is_nan(f)));
    };

    let string_of_int = d => {
      let-unbox n = (Int, d);
      Some(string(string_of_int(n)));
    };

    let string_of_float = d => {
      let-unbox f = (Float, d);
      Some(string(string_of_float(f)));
    };

    let string_of_bool = d => {
      let-unbox b = (Bool, d);
      Some(string(string_of_bool(b)));
    };

    let int_of_float = d => {
      let-unbox f = (Float, d);
      Some(int(int_of_float(f)));
    };

    let float_of_int = d => {
      let-unbox n = (Int, d);
      Some(float(float_of_int(n)));
    };

    let abs = d => {
      let-unbox n = (Int, d);
      Some(int(abs(n)));
    };

    let float_op = (fn, d) => {
      let-unbox f = (Float, d);
      Some(float(fn(f)));
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
        let d' = builtin_fun(name);
        let d' = ap(Forward, d', d);
        let d' = dynamic_error_hole(d', InvalidOfString);
        Some(d');
      };
    };

    let int_of_string = of_string(int_of_string_opt, n => int(n));
    let float_of_string = of_string(float_of_string_opt, f => float(f));
    let bool_of_string = of_string(bool_of_string_opt, b => bool(b));

    let int_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (Int, d1);
        let-unbox n = (Int, d2);
        if (n == 0) {
          Some(
            dynamic_error_hole(
              ap(Forward, builtin_fun(name), d1),
              DivideByZero,
            ),
          );
        } else {
          Some(int(m mod n));
        };
      });

    let string_length = d => {
      let-unbox s = (String, d);
      Some(int(String.length(s)));
    };

    let string_compare =
      binary((d1, d2) => {
        let-unbox s1 = (String, d1);
        let-unbox s2 = (String, d2);
        Some(int(String.compare(s1, s2)));
      });

    let string_trim = d => {
      let-unbox s = (String, d);
      Some(string(String.trim(s)));
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
        Some(string(String.concat(s1, xs')));
      });

    let string_sub = name =>
      ternary((d1, d2, d3) => {
        let-unbox s = (String, d1);
        let-unbox idx = (Int, d2);
        let-unbox len = (Int, d3);
        try(Some(string(String.sub(s, idx, len)))) {
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
        let split_str' = List.map(s => string(s), split_str);
        Some(list_lit(split_str'));
      });
  };

  open Impls;

  // Update src/haz3lmenhir/Lexer.mll when any new builtin is added
  let builtins =
    Fresh.Typ.(
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
      |> fn(
           "bool_of_string",
           String,
           Bool,
           bool_of_string("bool_of_string"),
         )
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
      |> fn("mod", Prod([int(), int()]), Int, int_mod("mod"))
      |> fn("string_length", String, Int, string_length)
      |> fn(
           "string_compare",
           Prod([string(), string()]),
           Int,
           string_compare,
         )
      |> fn("string_trim", String, String, string_trim)
      |> fn(
           "string_concat",
           Prod([string(), list(string())]),
           String,
           string_concat,
         )
      |> fn(
           "string_sub",
           Prod([string(), int(), int()]),
           String,
           string_sub("string_sub"),
         )
      |> fn(
           "string_split",
           Prod([string(), string()]),
           List(string()),
           string_split("string_split"),
         )
    );
};

let livelits_init: Ctx.t =
  Livelit.livelits |> List.map(entry => Ctx.LivelitEntry(entry));

let ctx_init: Ctx.t = {
  let meta_cons_map: ConstructorMap.t(Typ.t) = [
    Variant("$e", [Id.mk()], None),
    Variant("$v", [Id.mk()], None),
  ];
  let meta =
    Ctx.TVarEntry({
      name: "$Meta",
      id: Id.invalid,
      kind: Ctx.Singleton(Fresh.Typ.sum(meta_cons_map)),
    });
  List.map(
    fun
    | (name, Const(typ, _)) =>
      Ctx.VarEntry({
        name,
        typ,
        id: Id.invalid,
      })
    | (name, Fn(t1, t2, _)) =>
      Ctx.VarEntry({
        name,
        typ: Fresh.Typ.arrow(t1, t2),
        id: Id.invalid,
      }),
    Pervasives.builtins,
  )
  |> List.append(livelits_init)
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
        Environment.extend(env, (name, Fresh.Exp.builtin_fun(name))),
    Environment.empty,
    Pervasives.builtins,
  );
