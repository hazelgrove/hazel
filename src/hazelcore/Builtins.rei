let ctx: VarCtx.t;

let lookup_type: string => option(HTyp.t);

[@deriving sexp]
type impl_t =
  (list(DHExp.t), DHExp.t => EvaluatorResult.t) => EvaluatorResult.t;

let lookup_impl: string => option(impl_t);
