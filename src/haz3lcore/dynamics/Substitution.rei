/* closed substitution [d1/x]d2 */
let subst_var: (DHExp.t, Var.t, DHExp.t) => DHExp.t;
let subst: (Environment.t, DHExp.t) => DHExp.t;
