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

let using = (name: Var.t, impl: Var.t => Builtin.t, builtins: t): t =>
  VarMap.extend(builtins, (name, impl(name)));

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;

    /* is_infinite implementation. */
    let is_infinite = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let b = Float.is_infinite(f);
        BoxedValue(BoolLit(b)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* is_NaN implementation. */
    let is_nan = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let b = Float.is_nan(f);
        BoxedValue(BoolLit(b)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedBoolLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* int_of_float implementation. */
    let int_of_float = (name, r1) =>
      switch (r1) {
      | BoxedValue(FloatLit(f)) =>
        let i = int_of_float(f);
        BoxedValue(IntLit(i)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* float_of_int implementation. */
    let float_of_int = (name, r1) =>
      switch (r1) {
      | BoxedValue(IntLit(i)) =>
        let f = float_of_int(i);
        BoxedValue(FloatLit(f)) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
      | Indet(d1) => Indet(ApBuiltin(name, [d1])) |> return
      };

    /* mod implementation */
    let int_mod = (name, r1) =>
      switch (r1) {
      | BoxedValue(Tuple([IntLit(n), IntLit(m)]) as d1) =>
        switch (m) {
        | 0 =>
          Indet(InvalidOperation(ApBuiltin(name, [d1]), DivideByZero))
          |> return
        | _ => return(BoxedValue(IntLit(n mod m)))
        }
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(d1)))
      | Indet(d1) => return(Indet(ApBuiltin(name, [d1])))
      };

    /* PI implementation. */
    let pi = DHExp.FloatLit(Float.pi);

    /* Infinity float implementation. */
    let infinity = DHExp.FloatLit(Float.infinity);
    let neg_infinity = DHExp.FloatLit(Float.neg_infinity);

    /* NaN float implementation. */
    let nan = DHExp.FloatLit(Float.nan);
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let infinity = name => Builtin.mk_zero(name, Float, Impls.infinity);
  let neg_infinity = name => Builtin.mk_zero(name, Float, Impls.neg_infinity);
  let nan = name => Builtin.mk_zero(name, Float, Impls.nan);
  let is_infinite = name =>
    Builtin.mk_one(name, Arrow(Float, Bool), Impls.is_infinite);
  let is_nan = name =>
    Builtin.mk_one(name, Arrow(Float, Bool), Impls.is_nan);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
  let modulo = name =>
    Builtin.mk_one(name, Arrow(Prod([Int, Int]), Int), Impls.int_mod);

  let builtins =
    VarMap.empty
    |> using("pi", pi)
    |> using("infinity", infinity)
    |> using("neg_infinity", neg_infinity)
    |> using("nan", nan)
    |> using("is_infinite", is_infinite)
    |> using("is_nan", is_nan)
    |> using("int_of_float", int_of_float)
    |> using("float_of_int", float_of_int)
    |> using("mod", modulo);
};
