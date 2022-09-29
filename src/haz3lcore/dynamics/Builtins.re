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
    ((name, Builtin.{typ, _})) => (name, Ctx.{typ, id: Id.invalid}),
    builtins,
  );

let forms = (builtins: t): forms =>
  List.map(
    ((name, Builtin.{typ: _, eval, elab})) => (name, (elab, eval)),
    builtins,
  );

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;

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
    let int_mod = (name, r1, r2) =>
      switch (r1) {
      | BoxedValue(IntLit(n) as d1) =>
        switch (r2) {
        | BoxedValue(IntLit(m) as d2) =>
          switch (n, m) {
          | (_, 0) =>
            Indet(
              InvalidOperation(ApBuiltin(name, [d1, d2]), DivideByZero),
            )
            |> return
          | (n, m) => BoxedValue(IntLit(n mod m)) |> return
          }
        | BoxedValue(d2) =>
          raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2)))
        | Indet(d2) => Indet(ApBuiltin(name, [d1, d2])) |> return
        }
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) =>
        switch (r2) {
        | BoxedValue(d2)
        | Indet(d2) => Indet(ApBuiltin(name, [d1, d2])) |> return
        }
      };

    /* PI implementation. */
    let pi = DHExp.FloatLit(Float.pi);
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
  let modulo = name =>
    Builtin.mk_two(name, Arrow(Int, Arrow(Int, Int)), Impls.int_mod);
};
