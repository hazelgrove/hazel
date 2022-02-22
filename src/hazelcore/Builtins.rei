[@deriving sexp]
type args = list(DHExp.t);

[@deriving sexp]
type eval = (args, DHExp.t => EvaluatorResult.t) => EvaluatorResult.t;

[@deriving sexp]
type elab = DHExp.t;

[@deriving sexp]
type t = {
  ident: Var.t,
  ty: HTyp.t,
  eval,
  elab,
};

/* Context of built-in functions. */
let ctx: VarCtx.t;

/* Map of built-in function names to implementations. */
let forms: VarMap.t_((eval, elab));

/* Lookup the type of a built-in function. */
let lookup_type: Var.t => option(HTyp.t);

/* Lookup the implementation of a built-in function. */
let lookup_form: Var.t => option((eval, elab));
