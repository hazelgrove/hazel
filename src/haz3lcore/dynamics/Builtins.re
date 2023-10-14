/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `builtins`.

   See the existing ones for reference.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Builtin.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_(Builtin.builtin_evaluate);

type pervasive =
  (string, EvaluatorResult.t) => EvaluatorMonad.t(EvaluatorResult.t);

type result = Result.t(EvaluatorResult.t, EvaluatorError.t);

let const = (name: Var.t, typ: Typ.t, v: DHExp.t, builtins: t): t =>
  VarMap.extend(builtins, (name, Builtin.mk_zero(name, typ, v)));
let fn = (name: Var.t, typ: Typ.t, impl: pervasive, builtins: t): t =>
  VarMap.extend(builtins, (name, Builtin.mk_one(name, typ, impl)));

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;

    /* constants */
    let infinity = DHExp.FloatLit(Float.infinity);
    let neg_infinity = DHExp.FloatLit(Float.neg_infinity);
    let nan = DHExp.FloatLit(Float.nan);
    let epsilon_float = DHExp.FloatLit(epsilon_float);
    let pi = DHExp.FloatLit(Float.pi);
    let max_int = DHExp.IntLit(Int.max_int);
    let min_int = DHExp.IntLit(Int.min_int);

    let unary' = (f: DHExp.t => result, name: string, r: EvaluatorResult.t) =>
      switch (r) {
      | BoxedValue(b) =>
        switch (f(b)) {
        | Ok(r') => r' |> return
        | Error(e) => EvaluatorError.Exception(e) |> raise
        }
      | Indet(d) => Indet(ApBuiltin(name, [d])) |> return
      };

    let unary = (f: DHExp.t => Result.t(DHExp.t, EvaluatorError.t), name, r) => {
      let f = (d: DHExp.t): result =>
        switch (f(d)) {
        | Ok(r') => Ok(BoxedValue(r'))
        | Error(e) => Error(e)
        };
      unary'(f, name, r);
    };

    let is_finite =
      unary(
        fun
        | FloatLit(f) => Ok(BoolLit(Float.is_finite(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let is_infinite =
      unary(
        fun
        | FloatLit(f) => Ok(BoolLit(Float.is_infinite(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let is_nan =
      unary(
        fun
        | FloatLit(f) => Ok(BoolLit(Float.is_nan(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let string_of_int =
      unary(
        fun
        | IntLit(n) => Ok(StringLit(string_of_int(n)))
        | d => Error(InvalidBoxedIntLit(d)),
      );

    let string_of_float =
      unary(
        fun
        | FloatLit(f) => Ok(StringLit(string_of_float(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let string_of_bool =
      unary(
        fun
        | BoolLit(b) => Ok(StringLit(string_of_bool(b)))
        | d => Error(InvalidBoxedBoolLit(d)),
      );

    let int_of_float =
      unary(
        fun
        | FloatLit(f) => Ok(IntLit(int_of_float(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let float_of_int =
      unary(
        fun
        | IntLit(n) => Ok(FloatLit(float_of_int(n)))
        | d => Error(InvalidBoxedIntLit(d)),
      );

    let abs =
      unary(
        fun
        | IntLit(n) => Ok(IntLit(abs(n)))
        | d => Error(InvalidBoxedIntLit(d)),
      );

    let float_op = fn =>
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(fn(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

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
        (convert: string => option('a), wrap: 'a => DHExp.t, name: string) =>
      unary'(
        fun
        | StringLit(s) as d =>
          switch (convert(s)) {
          | Some(n) => Ok(BoxedValue(wrap(n)))
          | None =>
            let d' = DHExp.ApBuiltin(name, [d]);
            Ok(Indet(InvalidOperation(d', InvalidOfString)));
          }
        | d => Error(InvalidBoxedStringLit(d)),
        name,
      );

    let int_of_string = of_string(int_of_string_opt, n => IntLit(n));
    let float_of_string = of_string(float_of_string_opt, f => FloatLit(f));
    let bool_of_string = of_string(bool_of_string_opt, b => BoolLit(b));

    let int_mod = (name, r) =>
      switch (r) {
      | BoxedValue(Tuple([IntLit(n), IntLit(m)]) as d1) =>
        switch (m) {
        | 0 =>
          Indet(InvalidOperation(ApBuiltin(name, [d1]), DivideByZero))
          |> return
        | _ => return(BoxedValue(IntLit(n mod m)))
        }
      | BoxedValue(d) =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(d)))
      | Indet(d) => Indet(ApBuiltin(name, [d])) |> return
      };

    let string_length =
      unary(
        fun
        | StringLit(s) => Ok(IntLit(String.length(s)))
        | d => Error(InvalidBoxedStringLit(d)),
      );

    let string_compare =
      unary(
        fun
        | Tuple([StringLit(s1), StringLit(s2)]) =>
          Ok(IntLit(String.compare(s1, s2)))
        | d => Error(InvalidBoxedTuple(d)),
      );

    let string_trim =
      unary(
        fun
        | StringLit(s) => Ok(StringLit(String.trim(s)))
        | d => Error(InvalidBoxedStringLit(d)),
      );

    let string_of: DHExp.t => option(string) =
      fun
      | StringLit(s) => Some(s)
      | _ => None;

    let string_concat =
      unary(
        fun
        | Tuple([StringLit(s1), ListLit(_, _, _, xs)]) =>
          switch (xs |> List.map(string_of) |> Util.OptUtil.sequence) {
          | None => Error(InvalidBoxedStringLit(List.hd(xs)))
          | Some(xs) => Ok(StringLit(String.concat(s1, xs)))
          }
        | d => Error(InvalidBoxedTuple(d)),
      );

    let string_sub = name =>
      unary'(
        fun
        | Tuple([StringLit(s), IntLit(idx), IntLit(len)]) as d =>
          try(Ok(BoxedValue(StringLit(String.sub(s, idx, len))))) {
          | _ =>
            let d' = DHExp.ApBuiltin(name, [d]);
            Ok(Indet(InvalidOperation(d', IndexOutOfBounds)));
          }
        | d => Error(InvalidBoxedTuple(d)),
        name,
      );
  };

  open Impls;
  let builtins =
    VarMap.empty
    |> const("infinity", Float, infinity)
    |> const("neg_infinity", Float, neg_infinity)
    |> const("nan", Float, nan)
    |> const("epsilon_float", Float, epsilon_float)
    |> const("pi", Float, pi)
    |> const("max_int", Int, max_int)
    |> const("min_int", Int, min_int)
    |> fn("is_finite", Arrow(Float, Bool), is_finite)
    |> fn("is_infinite", Arrow(Float, Bool), is_infinite)
    |> fn("is_nan", Arrow(Float, Bool), is_nan)
    |> fn("int_of_float", Arrow(Float, Int), int_of_float)
    |> fn("float_of_int", Arrow(Int, Float), float_of_int)
    |> fn("string_of_int", Arrow(Int, String), string_of_int)
    |> fn("string_of_float", Arrow(Float, String), string_of_float)
    |> fn("string_of_bool", Arrow(Bool, String), string_of_bool)
    |> fn("int_of_string", Arrow(String, Int), int_of_string)
    |> fn("float_of_string", Arrow(String, Float), float_of_string)
    |> fn("bool_of_string", Arrow(String, Bool), bool_of_string)
    |> fn("abs", Arrow(Int, Int), abs)
    |> fn("abs_float", Arrow(Float, Float), abs_float)
    |> fn("ceil", Arrow(Float, Float), ceil)
    |> fn("floor", Arrow(Float, Float), floor)
    |> fn("exp", Arrow(Float, Float), exp)
    |> fn("log", Arrow(Float, Float), log)
    |> fn("log10", Arrow(Float, Float), log10)
    |> fn("sqrt", Arrow(Float, Float), sqrt)
    |> fn("sin", Arrow(Float, Float), sin)
    |> fn("cos", Arrow(Float, Float), cos)
    |> fn("tan", Arrow(Float, Float), tan)
    |> fn("asin", Arrow(Float, Float), asin)
    |> fn("acos", Arrow(Float, Float), acos)
    |> fn("atan", Arrow(Float, Float), atan)
    |> fn("mod", Arrow(Prod([Int, Int]), Int), int_mod)
    |> fn("string_length", Arrow(String, Int), string_length)
    |> fn(
         "string_compare",
         Arrow(Prod([String, String]), Int),
         string_compare,
       )
    |> fn("string_trim", Arrow(String, String), string_trim)
    |> fn(
         "string_concat",
         Arrow(Prod([String, List(String)]), String),
         string_concat,
       )
    |> fn("string_sub", Arrow(Prod([String, Int, Int]), String), string_sub);
};

let ctx_init: Ctx.t = {
  let meta_cons_map =
    ConstructorMap.of_list([("$Expr", None), ("$Value", None)]);
  let meta =
    Ctx.TVarEntry({
      name: "$Meta",
      id: Id.invalid,
      kind: Kind.Singleton(Sum(meta_cons_map)),
    });
  let ctx =
    List.map(
      ((name, Builtin.{typ, _})) =>
        Ctx.VarEntry({name, typ, id: Id.invalid}),
      Pervasives.builtins,
    );
  let ctx = Ctx.extend(ctx, meta);
  let ctx = Ctx.add_ctrs(ctx, "$Meta", Id.invalid, meta_cons_map);
  ctx;
};

let forms_init: forms =
  List.map(
    ((name, Builtin.{eval, _})) => (name, eval),
    Pervasives.builtins,
  );

let env_init: Environment.t =
  List.fold_left(
    (env, (name, Builtin.{elab, _})) =>
      Environment.extend(env, (name, elab)),
    Environment.empty,
    Pervasives.builtins,
  );
