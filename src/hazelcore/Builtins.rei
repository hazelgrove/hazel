[@deriving sexp]
type form = (
  /* eval: */ (list(DHExp.t), DHExp.t => EvaluatorResult.t) =>
              EvaluatorResult.t,
  /* elab: */ DHExp.t,
);

[@deriving sexp]
type t = (Var.t, HTyp.t, form);

/* Context of builtin functions. */
let ctx: VarCtx.t;

/* Map of builtin function names to implementations. */
let forms: VarMap.t_(form);

/* Lookup the type of a builtin function. */
let lookup_type: Var.t => option(HTyp.t);

/* Lookup the implementation of a builtin function. */
let lookup_form: Var.t => option(form);
