[@deriving sexp]
type syn_types = ExprLabel.Map.t(Typ.t);

[@deriving sexp]
type syn_ok = {types: syn_types};

[@deriving sexp]
type syn_error =
  | CaseEmptyRules(ExprLabel.t)
  | UnboundVar(ExprLabel.t)
  | UnboundHole(ExprLabel.t)
  | WrongHoleSort(ExprLabel.t)
  | SigmaUnboundVar(ExprLabel.t, Ident.t)
  | TypesNotEqual(ExprLabel.t, Typ.t, Typ.t)
  | TypesEqual(ExprLabel.t, Typ.t, Typ.t)
  | TypesInconsistent(ExprLabel.t, Typ.t, Typ.t)
  | PatTypesNotEqual(PatLabel.t, Typ.t, Typ.t);

[@deriving sexp]
type syn_result = result(syn_ok, syn_error);

let syn: (TypContext.t, Delta.t, Expr.t) => syn_result;
