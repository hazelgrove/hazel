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
    /* int_of_string implementation. */
    let int_of_string = (name, r1) =>
      switch (r1) {
      | BoxedValue(StringLit(f) as d1) =>
        let f = Re.Str.string_after(f, 1);
        let f = Re.Str.string_before(f, String.length(f) - 1);
        if (String.length(f) == 0) {
          BoxedValue(IntLit(0)) |> return;
        } else if (Form.is_int(f)) {
          let i = int_of_string(f);
          BoxedValue(IntLit(i)) |> return;
        } else {
          Indet(
            InvalidOperation(ApBuiltin(name, [d1]), InvalidIntOfString),
          )
          |> return;
        };
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedStringLit(d1)))
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
    /* string_of_int implementation. */
    let string_of_int = (name, r1) =>
      switch (r1) {
      | BoxedValue(IntLit(i)) =>
        let s = string_of_int(i);
        BoxedValue(StringLit("\"" ++ s ++ "\"")) |> return;
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
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let int_of_string = name =>
    Builtin.mk_one(name, Arrow(String, Int), Impls.int_of_string);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
  let string_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, String), Impls.string_of_int);
  let modulo = name =>
    Builtin.mk_one(name, Arrow(Prod([Int, Int]), Int), Impls.int_mod);

  let builtins =
    VarMap.empty
    |> using("pi", pi)
    |> using("int_of_float", int_of_float)
    |> using("float_of_int", float_of_int)
    |> using("int_of_string", int_of_string)
    |> using("string_of_int", string_of_int)
    |> using("mod", modulo);
};
