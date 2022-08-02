[@deriving sexp]
type types = ExprLabel.Map.t(Typ.t);

[@deriving sexp]
type syn_ok = {types};

[@deriving sexp]
type syn_error =
  | SynNoRules(ExprLabel.t)
  | SynUnbound(ExprLabel.t)
  | SynHoleUnbound(ExprLabel.t)
  | SynHolePatternHole(ExprLabel.t)
  | SynHoleSigmaUnbound(ExprLabel.t, Ident.t)
  | AnaNotEqual(ExprLabel.t, Typ.t, Typ.t)
  | AnaEqual(ExprLabel.t, Typ.t, Typ.t)
  | AnaInconsistent(ExprLabel.t, Typ.t, Typ.t)
  | AnaPatNotEqual(PatLabel.t, Typ.t, Typ.t);

[@deriving sexp]
type syn_result = result(syn_ok, syn_error);

let syn: (Ident.Map.t(Typ.t), Delta.t, Expr.t) => syn_result;
