/*
   Built-in functions for Hazel.

   To add a built-in function or constant, write the implementation in the
   `Impls` module below and add it to the `builtins` list.

   See the existing ones for reference.
 */

module Impls = {
  open EvaluatorMonad;
  open EvaluatorResult;

  /* int_of_float implementation. */
  let int_of_float = (ident, r1) =>
    switch (r1) {
    | BoxedValue(FloatLit(f)) =>
      let i = int_of_float(f);
      BoxedValue(IntLit(i)) |> return;
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1])) |> return
    };

  /* float_of_int implementation. */
  let float_of_int = (ident, r1) =>
    switch (r1) {
    | BoxedValue(IntLit(i)) =>
      let f = float_of_int(i);
      BoxedValue(FloatLit(f)) |> return;
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedFloatLit(d1)))
    | Indet(d1) => Indet(ApBuiltin(ident, [d1])) |> return
    };

  /* mod implementation */
  let int_mod = (ident, r1, r2) =>
    switch (r1) {
    | BoxedValue(IntLit(n) as d1) =>
      switch (r2) {
      | BoxedValue(IntLit(m) as d2) =>
        switch (n, m) {
        | (_, 0) =>
          Indet(InvalidOperation(ApBuiltin(ident, [d1, d2]), DivideByZero))
          |> return
        | (n, m) => BoxedValue(IntLit(n mod m)) |> return
        }
      | BoxedValue(d2) =>
        raise(EvaluatorError.Exception(InvalidBoxedIntLit(d2)))
      | Indet(d2) => Indet(ApBuiltin(ident, [d1, d2])) |> return
      }
    | BoxedValue(d1) =>
      raise(EvaluatorError.Exception(InvalidBoxedIntLit(d1)))
    | Indet(d1) =>
      switch (r2) {
      | BoxedValue(d2)
      | Indet(d2) => Indet(ApBuiltin(ident, [d1, d2])) |> return
      }
    };

  /* PI implementation. */
  let pi = DHExp.FloatLit(Float.pi);
};

let builtins: list(Builtin.t) = [
  Builtin.mk_zero("PI", Float, Impls.pi),
  Builtin.mk_one("int_of_float", Arrow(Float, Int), Impls.int_of_float),
  Builtin.mk_one("float_of_int", Arrow(Int, Float), Impls.float_of_int),
  Builtin.mk_two("mod", Arrow(Int, Arrow(Int, Int)), Impls.int_mod),
];

let ctx: VarCtx.t =
  List.map(({ident, ty, _}: Builtin.t) => (ident, ty), builtins);
let forms =
  List.map(
    ({ident, ty: _ty, eval, elab}: Builtin.t) => (ident, (eval, elab)),
    builtins,
  );

let lookup_type = VarMap.lookup(ctx);
let lookup_form = VarMap.lookup(forms);
