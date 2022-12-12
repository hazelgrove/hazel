/* Context of built-in functions. */
let ctx: VarCtx.t /* Map of built-in function names to implementations. */;

let forms: VarMap.t_((Builtin.eval, Builtin.elab)) /* Lookup the type of a built-in function. */;

let lookup_type: Var.t => option(HTyp.t) /* Lookup the implementation of a built-in function. */;

let lookup_form: Var.t => option((Builtin.eval, Builtin.elab));
