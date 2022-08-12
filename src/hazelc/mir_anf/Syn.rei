[@deriving sexp]
type syn_types = ExprLabel.Map.t(Typ.t);
[@deriving sexp]
type syn_idents = Ident.Set.t;
[@deriving sexp]
type syn_labels = {
  expr: ExprLabel.Set.t,
  stmt: StmtLabel.Set.t,
  rule: RuleLabel.Set.t,
  pat: PatLabel.Set.t,
};

[@deriving sexp]
type syn_ok = {
  types: syn_types,
  idents: syn_idents,
  labels: syn_labels,
};

[@deriving sexp]
type syn_error =
  | DuplicateIdent(Ident.t)
  | DuplicateExprLabel(ExprLabel.t)
  | DuplicateStmtLabel(StmtLabel.t)
  | DuplicateRuleLabel(RuleLabel.t)
  | DuplicatePatLabel(PatLabel.t)
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

let syn: (TypContext.t, Delta.t, Anf.block) => syn_result;
