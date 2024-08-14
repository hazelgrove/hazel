/* closed substitution [d1/x]d2 */
let subst_var:
  (Statics.Map.t, DHExp.t(list(Id.t)), Var.t, DHExp.t(list(Id.t))) =>
  DHExp.t(list(Id.t));
let subst:
  (Statics.Map.t, Environment.t, DHExp.t(list(Id.t))) =>
  DHExp.t(list(Id.t));
