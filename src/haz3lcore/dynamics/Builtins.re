/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Pervasives.Impls` module below and add it to `Pervasives`.

   See the existing ones for reference.
 */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Builtin.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type forms = VarMap.t_((DHExp.t, Builtin.builtin_evaluate));

type pervasive =
  (string, EvaluatorResult.t) => EvaluatorMonad.t(EvaluatorResult.t);

let ctx = (builtins: t): Ctx.t =>
  List.map(
    ((name, Builtin.{typ, _})) =>
      Ctx.VarEntry({name, typ, id: Id.invalid}),
    builtins,
  );

let forms = (builtins: t): forms =>
  List.map(
    ((name, Builtin.{typ: _, eval, elab})) => (name, (elab, eval)),
    builtins,
  );

let using_const = (name: Var.t, typ: Typ.t, v: DHExp.t, builtins: t): t =>
  VarMap.extend(builtins, (name, Builtin.mk_zero(name, typ, v)));
let using_fn = (name: Var.t, typ: Typ.t, impl: pervasive, builtins: t): t =>
  VarMap.extend(builtins, (name, Builtin.mk_one(name, typ, impl)));

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;

    /* constants */
    let pi = DHExp.FloatLit(Float.pi);
    let max_int = DHExp.IntLit(max_int);
    let min_int = DHExp.IntLit(min_int);
    let infinity = DHExp.FloatLit(Float.infinity);
    let neg_infinity = DHExp.FloatLit(Float.neg_infinity);
    let nan = DHExp.FloatLit(Float.nan);
    let epsilon_float = DHExp.FloatLit(epsilon_float);

    let unary' =
        (
          f: DHExp.t => Result.t(EvaluatorResult.t, EvaluatorError.t),
          name: string,
          r: EvaluatorResult.t,
        ) =>
      switch (r) {
      | BoxedValue(b) =>
        switch (f(b)) {
        | Ok(r') => r' |> return
        | Error(e) => EvaluatorError.Exception(e) |> raise
        }
      | Indet(d) => Indet(ApBuiltin(name, [d])) |> return
      };

    let unary = (f: DHExp.t => Result.t(DHExp.t, EvaluatorError.t), name, r) => {
      let f = b =>
        switch (f(b)) {
        | Ok(r') => Ok(BoxedValue(r'))
        | Error(e) => Error(e)
        };
      unary'(f, name, r);
    };

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

    let abs_float =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(abs_float(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let ceil =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(ceil(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let floor =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(floor(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let sqrt =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(sqrt(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let exp =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(exp(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let log =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(log(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let log10 =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(log10(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let sin =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(sin(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let cos =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(cos(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let tan =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(tan(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let asin =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(asin(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let acos =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(acos(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let atan =
      unary(
        fun
        | FloatLit(f) => Ok(FloatLit(atan(f)))
        | d => Error(InvalidBoxedFloatLit(d)),
      );

    let int_of_string = name =>
      unary'(
        fun
        | StringLit(s) as d =>
          switch (int_of_string_opt(Form.strip_quotes(s))) {
          | Some(n) => Ok(BoxedValue(IntLit(n)))
          | None =>
            let d' = DHExp.ApBuiltin(name, [d]);
            Ok(Indet(InvalidOperation(d', ToStringFailed)));
          }
        | d => Error(InvalidBoxedStringLit(d)),
        name,
      );

    let float_of_string = name =>
      unary'(
        fun
        | StringLit(s) as d =>
          switch (float_of_string_opt(Form.strip_quotes(s))) {
          | Some(f) => Ok(BoxedValue(FloatLit(f)))
          | None =>
            let d' = DHExp.ApBuiltin(name, [d]);
            Ok(Indet(InvalidOperation(d', ToStringFailed)));
          }
        | d => Error(InvalidBoxedStringLit(d)),
        name,
      );

    let bool_of_string = name =>
      unary'(
        fun
        | StringLit(s) as d =>
          switch (bool_of_string_opt(Form.strip_quotes(s))) {
          | Some(b) => Ok(BoxedValue(BoolLit(b)))
          | None =>
            let d' = DHExp.ApBuiltin(name, [d]);
            Ok(Indet(InvalidOperation(d', ToStringFailed)));
          }
        | d => Error(InvalidBoxedStringLit(d)),
        name,
      );

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
  };

  open Impls;
  let builtins =
    VarMap.empty
    |> using_const("pi", Float, pi)
    |> using_const("max_int", Int, max_int)
    |> using_const("min_int", Int, min_int)
    |> using_const("epsilon_float", Float, epsilon_float)
    |> using_const("infinity", Float, infinity)
    |> using_const("neg_infinity", Float, neg_infinity)
    |> using_const("nan", Float, nan)
    |> using_fn("int_of_float", Arrow(Float, Int), int_of_float)
    |> using_fn("float_of_int", Arrow(Int, Float), float_of_int)
    |> using_fn("string_of_int", Arrow(Int, String), string_of_int)
    |> using_fn("string_of_float", Arrow(Float, String), string_of_float)
    |> using_fn("string_of_bool", Arrow(Bool, String), string_of_bool)
    |> using_fn("int_of_string", Arrow(String, Int), int_of_string)
    |> using_fn("float_of_string", Arrow(String, Float), float_of_string)
    |> using_fn("bool_of_string", Arrow(String, Bool), bool_of_string)
    |> using_fn("abs", Arrow(Int, Int), abs)
    |> using_fn("abs_float", Arrow(Float, Float), abs_float)
    |> using_fn("ceil", Arrow(Float, Float), ceil)
    |> using_fn("floor", Arrow(Float, Float), floor)
    |> using_fn("exp", Arrow(Float, Float), exp)
    |> using_fn("log", Arrow(Float, Float), log)
    |> using_fn("log10", Arrow(Float, Float), log10)
    |> using_fn("sqrt", Arrow(Float, Float), sqrt)
    |> using_fn("sin", Arrow(Float, Float), sin)
    |> using_fn("cos", Arrow(Float, Float), cos)
    |> using_fn("tan", Arrow(Float, Float), tan)
    |> using_fn("asin", Arrow(Float, Float), asin)
    |> using_fn("acos", Arrow(Float, Float), acos)
    |> using_fn("atan", Arrow(Float, Float), atan)
    |> using_fn("mod", Arrow(Prod([Int, Int]), Int), int_mod);
};
