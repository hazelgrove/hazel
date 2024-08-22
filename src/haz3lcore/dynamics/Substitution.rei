/* closed substitution [d1/x]d2 */
let subst_var:
  (Statics.Map.t, DHExp.t(IdTag.t), Var.t, DHExp.t(IdTag.t)) =>
  DHExp.t(IdTag.t);
let subst:
  (Statics.Map.t, Environment.t, DHExp.t(IdTag.t)) => DHExp.t(IdTag.t);
