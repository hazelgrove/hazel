/* closed substitution [d1/x]d2 */
let subst_var: (Statics.Map.t, DHExp.t, Var.t, DHExp.t) => DHExp.t;
let subst: (Statics.Map.t, Environment.t, DHExp.t) => DHExp.t;
