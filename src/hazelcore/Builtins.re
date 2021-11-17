open EvaluatorResult;

module Impl = {
  [@deriving sexp]
  type t = (list(DHExp.t), DHExp.t => EvaluatorResult.t) => EvaluatorResult.t;

  let mk_one_arg = (f, ident, args, evaluate) => {
    let e = DHExp.ApBuiltin(ident, args);
    switch (args) {
    | [] => Indet(e)
    | [d1, ..._] => f(d1, evaluate, e)
    };
  };

  let int_of_float =
    (
      (d1, evaluate, e) =>
        switch (evaluate(d1)) {
        | BoxedValue(FloatLit(f)) =>
          let i = int_of_float(f);
          BoxedValue(IntLit(i));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;

  let float_of_int =
    (
      (d1, evaluate, e) =>
        switch (evaluate(d1)) {
        | BoxedValue(IntLit(i)) =>
          let f = float_of_int(i);
          BoxedValue(FloatLit(f));
        | _ => Indet(e)
        }
    )
    |> mk_one_arg;
};

let builtins: VarMap.t_((HTyp.t, string => Impl.t)) = [
  ("int_of_float", (Arrow(Float, Int), Impl.int_of_float)),
  ("float_of_int", (Arrow(Int, Float), Impl.float_of_int)),
];

let ctx: VarCtx.t = List.map(((x, (ty, _))) => (x, ty), builtins);

let impls: VarMap.t_(Impl.t) =
  List.map(((x, (_, impl))) => (x, impl(x)), builtins);

let lookup_type = x => VarMap.lookup(ctx, x);

let lookup_impl = x =>
  VarMap.lookup(builtins, x) |> Option.map(((_, impl)) => impl(x));
