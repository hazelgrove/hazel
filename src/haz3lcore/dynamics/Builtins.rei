/* Context of built-in functions. */
let ctx: VarCtx.t;

/* Map of built-in function names to implementations. */
let forms:
  VarMap.t_((Builtin.builtin_evaluate, Builtin.builtin_elaboration));

/* Lookup the type of a built-in function. */
let lookup_type: Var.t => option(Typ.t);

/* Lookup the implementation of a built-in function. */
let lookup_form:
  Var.t => option((Builtin.builtin_evaluate, Builtin.builtin_elaboration));
