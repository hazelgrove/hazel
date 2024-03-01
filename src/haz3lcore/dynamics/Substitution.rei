/* closed substitution [d1/x]d2 */
let subst_var: (Statics.Map.t, DExp.t, Var.t, DExp.t) => DExp.t;
let subst: (Statics.Map.t, Environment.t, DExp.t) => DExp.t;
