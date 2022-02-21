module Impl: {
  [@deriving sexp]
  type t =
    (
      /* args: */ list(DHExp.t),
      /* evaluate: */ DHExp.t => EvaluatorResult.t
    ) =>
    EvaluatorResult.t;
};

let ctx: VarCtx.t;

let impls: VarMap.t_(Impl.t);

let lookup_type: string => option(HTyp.t);

let lookup_impl: string => option(Impl.t);
