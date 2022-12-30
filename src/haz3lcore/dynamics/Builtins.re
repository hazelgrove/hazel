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

let ids_derive = DHExp.ids_derive;

module Pervasives = {
  module Impls = {
    open EvaluatorMonad;
    open EvaluatorResult;

    /* int_of_float implementation. */
    let int_of_float = (name, r1) =>
      switch (r1) {
      | BoxedValue({ids, term: Float(f)}) =>
        let i = int_of_float(f);
        BoxedValue({ids, term: Int(i)}) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
      | Indet(d1) =>
        Indet({ids: ids_derive(d1.ids), term: ApBuiltin(name, [d1])})
        |> return
      };

    /* float_of_int implementation. */
    let float_of_int = (name, r1) =>
      switch (r1) {
      | BoxedValue({ids, term: Int(i)}) =>
        let f = float_of_int(i);
        BoxedValue({ids, term: Float(f)}) |> return;
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
      | Indet(d1) =>
        Indet({ids: ids_derive(d1.ids), term: ApBuiltin(name, [d1])})
        |> return
      };

    /* mod implementation */
    let int_mod = (name, r1) =>
      switch (r1) {
      | BoxedValue(
          {ids, term: Tuple([{term: Int(n), _}, {term: Int(m), _}])} as d1,
        ) =>
        switch (m) {
        | 0 =>
          Indet({
            ids: ids_derive(ids, ~step=2),
            term:
              Error(
                InvalidOperation(
                  DivideByZero,
                  {
                    ids: ids_derive(ids, ~step=1),
                    term: ApBuiltin(name, [d1]),
                  },
                ),
              ),
          })
          |> return
        | _ =>
          return(BoxedValue({ids: ids_derive(ids), term: Int(n mod m)}))
        }
      | BoxedValue(d1) =>
        raise(EvaluatorError.Exception(InvalidBoxedTuple(d1)))
      | Indet(d1) =>
        return(
          Indet({ids: ids_derive(d1.ids), term: ApBuiltin(name, [d1])}),
        )
      };

    /* PI implementation. */
    let pi = DHExp.{ids: [Id.invalid], term: Float(Float.pi)};
  };

  let pi = name => Builtin.mk_zero(name, Float, Impls.pi);
  let int_of_float = name =>
    Builtin.mk_one(name, Arrow(Float, Int), Impls.int_of_float);
  let float_of_int = name =>
    Builtin.mk_one(name, Arrow(Int, Float), Impls.float_of_int);
  let modulo = name =>
    Builtin.mk_one(name, Arrow(Prod([Int, Int]), Int), Impls.int_mod);

  let builtins =
    VarMap.empty
    |> using("pi", pi)
    |> using("int_of_float", int_of_float)
    |> using("float_of_int", float_of_int)
    |> using("mod", modulo);
};
