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
      let-unbox f = (CONST_RENAMEME(Float), d);
      Some(bool(Float.is_finite(f)));
    };

    let is_infinite = d => {
      let-unbox f = (CONST_RENAMEME(Float), d);
      Some(bool(Float.is_infinite(f)));
    };

    let is_nan = d => {
      let-unbox f = (CONST_RENAMEME(Float), d);
      Some(bool(Float.is_nan(f)));
    };

    let abs = d => {
      let-unbox n = (CONST_RENAMEME(Int), d);
      Some(int(abs(n)));
    };

    let float_op = (fn, d) => {
      let-unbox f = (CONST_RENAMEME(Float), d);
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

    let int_mod = name =>
      binary((d1, d2) => {
        let-unbox m = (CONST_RENAMEME(Int), d1);
        let-unbox n = (CONST_RENAMEME(Int), d2);
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
      let-unbox s = (CONST_RENAMEME(String), d);
      Some(int(String.length(s)));
    };

    let string_compare =
      binary((d1, d2) => {
        let-unbox s1 = (CONST_RENAMEME(String), d1);
        let-unbox s2 = (CONST_RENAMEME(String), d2);
        Some(int(String.compare(s1, s2)));
      });

    let string_trim = d => {
      let-unbox s = (CONST_RENAMEME(String), d);
      Some(string(String.trim(s)));
    };

    let string_of: DHExp.t => option(string) =
      d => {
        let-unbox s = (CONST_RENAMEME(String), d);
        Some(s);
      };

    let string_concat =
      binary((d1, d2) => {
        let-unbox s1 = (CONST_RENAMEME(String), d1);
        let-unbox xs = (ListLit, d2);
        let* xs' = List.map(string_of, xs) |> Util.OptUtil.sequence;
        Some(string(String.concat(s1, xs')));
      });

    let string_sub = name =>
      ternary((d1, d2, d3) => {
        let-unbox s = (CONST_RENAMEME(String), d1);
        let-unbox idx = (CONST_RENAMEME(Int), d2);
        let-unbox len = (CONST_RENAMEME(Int), d3);
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
        let-unbox s = (CONST_RENAMEME(String), d1);
        let-unbox sep = (CONST_RENAMEME(String), d2);
        let split_str = Util.StringUtil.plain_split(sep, s);
        let split_str' = List.map(s => string(s), split_str);
        Some(list_lit(split_str'));
      });
  };

  open Impls;

  // Update src/haz3lmenhir/Lexer.mll when any new builtin is added

  let of_atom_builtin = (b: CONST_RENAMEMO.builtin): builtin => {
    switch (b) {
    | OneFun(k1, k2, f) =>
      Fn(
        CONST_RENAMET(k1 |> CONST_RENAMEMO.cls_of_kind) |> Typ.fresh,
        CONST_RENAMET(k2 |> CONST_RENAMEMO.cls_of_kind) |> Typ.fresh,
        (d: DHExp.t) => {
          let-unbox x = (CONST_RENAMEME(k1), d);
          switch (f(x)) {
          | L(x) =>
            Some(CONST_RENAMEME(CONST_RENAMEMO.repack(k2, x)) |> Exp.fresh)
          | R(_) => None
          };
        },
      )
    | TwoFun(k1, k2, k3, f) =>
      Fn(
        Prod([
          CONST_RENAMET(k1 |> CONST_RENAMEMO.cls_of_kind) |> Typ.fresh,
          CONST_RENAMET(k2 |> CONST_RENAMEMO.cls_of_kind) |> Typ.fresh,
        ])
        |> Typ.fresh,
        CONST_RENAMET(k3 |> CONST_RENAMEMO.cls_of_kind) |> Typ.fresh,
        [@warning "-8"] (d: DHExp.t) => {
          let-unbox [x, y] = (Tuple(2), d);
          let-unbox x = (CONST_RENAMEME(k1), x);
          let-unbox y = (CONST_RENAMEME(k2), y);
          switch (f(x, y)) {
          | L(x) =>
            Some(CONST_RENAMEME(CONST_RENAMEMO.repack(k3, x)) |> Exp.fresh)
          | R(_) => None
          };
        },
      )
    };
  };

  let builtins =
    Fresh.Typ.(
      VarMap.empty
      |> const("infinity", CONST_RENAMET(Float), infinity)
      |> const("neg_infinity", CONST_RENAMET(Float), neg_infinity)
      |> const("nan", CONST_RENAMET(Float), nan)
      |> const("epsilon_float", CONST_RENAMET(Float), epsilon_float)
      |> const("pi", CONST_RENAMET(Float), pi)
      |> const("max_int", CONST_RENAMET(Int), max_int)
      |> const("min_int", CONST_RENAMET(Int), min_int)
      |> fn(
           "is_finite",
           CONST_RENAMET(Float),
           CONST_RENAMET(Bool),
           is_finite,
         )
      |> fn(
           "is_infinite",
           CONST_RENAMET(Float),
           CONST_RENAMET(Bool),
           is_infinite,
         )
      |> fn("is_nan", CONST_RENAMET(Float), CONST_RENAMET(Bool), is_nan)
      |> fn("abs", CONST_RENAMET(Int), CONST_RENAMET(Int), abs)
      |> fn(
           "abs_float",
           CONST_RENAMET(Float),
           CONST_RENAMET(Float),
           abs_float,
         )
      |> fn("ceil", CONST_RENAMET(Float), CONST_RENAMET(Float), ceil)
      |> fn("floor", CONST_RENAMET(Float), CONST_RENAMET(Float), floor)
      |> fn("exp", CONST_RENAMET(Float), CONST_RENAMET(Float), exp)
      |> fn("log", CONST_RENAMET(Float), CONST_RENAMET(Float), log)
      |> fn("log10", CONST_RENAMET(Float), CONST_RENAMET(Float), log10)
      |> fn("sqrt", CONST_RENAMET(Float), CONST_RENAMET(Float), sqrt)
      |> fn("sin", CONST_RENAMET(Float), CONST_RENAMET(Float), sin)
      |> fn("cos", CONST_RENAMET(Float), CONST_RENAMET(Float), cos)
      |> fn("tan", CONST_RENAMET(Float), CONST_RENAMET(Float), tan)
      |> fn("asin", CONST_RENAMET(Float), CONST_RENAMET(Float), asin)
      |> fn("acos", CONST_RENAMET(Float), CONST_RENAMET(Float), acos)
      |> fn("atan", CONST_RENAMET(Float), CONST_RENAMET(Float), atan)
      |> fn(
           "mod",
           Prod([int(), int()]),
           CONST_RENAMET(Int),
           int_mod("mod"),
         )
      |> fn(
           "string_length",
           CONST_RENAMET(String),
           CONST_RENAMET(Int),
           string_length,
         )
      |> fn(
           "string_compare",
           Prod([string(), string()]),
           CONST_RENAMET(Int),
           string_compare,
         )
      |> fn(
           "string_trim",
           CONST_RENAMET(String),
           CONST_RENAMET(String),
           string_trim,
         )
      |> fn(
           "string_concat",
           Prod([string(), list(string())]),
           CONST_RENAMET(String),
           string_concat,
         )
      |> fn(
           "string_sub",
           Prod([string(), int(), int()]),
           CONST_RENAMET(String),
           string_sub("string_sub"),
         )
      |> fn(
           "string_split",
           Prod([string(), string()]),
           List(string()),
           string_split("string_split"),
         )
    )
    |> VarMap.concat(
         _,
         List.map(
           ((n, b)) => (n, of_atom_builtin(b)),
           CONST_RENAMEMO.converter_builtins,
         ),
       )
    |> VarMap.concat(
         _,
         List.map(
           ((n, b)) => (n, of_atom_builtin(b)),
           Operators.builtins,
         ),
       );
};

let entries =
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
  );

let ctx_init: option(Operators.mode) => Ctx.t =
  use_mode => {
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
    Ctx.{
      use_mode,
      entries,
    }
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
