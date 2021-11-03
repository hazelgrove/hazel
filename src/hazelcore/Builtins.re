module Impls = {
  open EvaluatorResult;

  let int_of_float = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(FloatLit(f)) =>
        let i = int_of_float(f);
        BoxedValue(IntLit(i));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };

  let float_of_int = (ident, args, evaluate) =>
    switch (args) {
    | [] => Indet(ApBuiltin(ident, args))
    | [d1, ..._] =>
      switch (evaluate(d1)) {
      | BoxedValue(IntLit(i)) =>
        let f = float_of_int(i);
        BoxedValue(FloatLit(f));
      | _ => Indet(ApBuiltin(ident, args))
      }
    };
};

[@deriving sexp]
type impl_t =
  (list(DHExp.t), DHExp.t => EvaluatorResult.t) => EvaluatorResult.t;

let ctx: VarCtx.t = [
  ("int_of_float", Arrow(Float, Int)),
  ("float_of_int", Arrow(Int, Float)),
];

let lookup_type = x => VarMap.lookup(ctx, x);

let impls: VarMap.t_(string => impl_t) = [
  ("int_of_float", Impls.int_of_float),
  ("float_of_int", Impls.float_of_int),
];

let lookup_impl = x =>
  VarMap.lookup(impls, x) |> Option.map(impl => impl(x));
