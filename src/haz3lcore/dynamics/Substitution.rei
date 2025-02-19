/* closed substitution [d1/x]d2 */
let subst_var: (Exp.t, Var.t, Exp.t) => Exp.t;
let subst: (Environment.t, Exp.t) => Exp.t;
