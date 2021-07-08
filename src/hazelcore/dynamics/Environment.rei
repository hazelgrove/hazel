[@deriving sexp]
type t = VarMap.t_(DHExp.t);
include (module type of VarMap);

/* closed substitution [d1/x]d2*/
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;

let subst: (t, DHExp.t) => DHExp.t;
