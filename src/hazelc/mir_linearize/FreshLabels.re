[@deriving sexp]
type t = (
  Hir_expr.Expr.Label.t,
  Hir_expr.Expr.RuleLabel.t,
  Hir_expr.Pat.Label.t,
);

let rec fresh_labels_put_expr = (expr', (expr, rule, pat)) => (
  Hir_expr.Expr.Label.max(expr, expr'),
  rule,
  pat,
)
and fresh_labels_put_rule = (rule', (expr, rule, pat)) => (
  expr,
  Hir_expr.Expr.RuleLabel.max(rule, rule'),
  pat,
)
and fresh_labels_put_pat = (pat', (expr, rule, pat)) => (
  expr,
  rule,
  Hir_expr.Pat.Label.max(pat, pat'),
)

and fresh_labels = ({kind, label}: Hir_expr.expr, acc) => {
  let acc = acc |> fresh_labels_put_expr(label);
  switch (kind) {
  | EEmptyHole(_u, _i, sigma) => acc |> fresh_labels_sigma(sigma)
  | ENonEmptyHole(_reason, _u, _i, sigma, e') =>
    acc |> fresh_labels_sigma(sigma) |> fresh_labels(e')
  | EKeyword(_u, _i, sigma, _k) => acc |> fresh_labels_sigma(sigma)
  | EFreeVar(_u, _i, sigma, _x) => acc |> fresh_labels_sigma(sigma)
  | EInvalidText(_u, _i, sigma, _text) => acc |> fresh_labels_sigma(sigma)
  | EInvalidOperation(e, _err) => acc |> fresh_labels(e)
  /* Casts */
  | ECast(e, _ty, _ty') => acc |> fresh_labels(e)
  | EFailedCast(e, _ty, _ty') => acc |> fresh_labels(e)
  /* Case */
  | EConsistentCase(case) => acc |> fresh_labels_case(case)
  | EInconsistentBranches(_u, _i, sigma, case) =>
    acc |> fresh_labels_sigma(sigma) |> fresh_labels_case(case)
  /* Let bindings */
  | ELet(p, _p_ty, e') => acc |> fresh_labels_pat(p) |> fresh_labels(e')
  | ELetRec(_x, p, _p_ty, e1, e2) =>
    acc |> fresh_labels_pat(p) |> fresh_labels(e1) |> fresh_labels(e2)
  /* Function */
  | EFun(p, _p_ty, body) => acc |> fresh_labels_pat(p) |> fresh_labels(body)
  /* Application */
  | EAp(fn, arg) => acc |> fresh_labels(fn) |> fresh_labels(arg)
  | EApBuiltin(_name, args) =>
    args |> List.fold_left((acc, arg) => acc |> fresh_labels(arg), acc)
  /* Binary operations */
  | EBinBoolOp(_op, e1, e2) => acc |> fresh_labels(e1) |> fresh_labels(e2)
  | EBinIntOp(_op, e1, e2) => acc |> fresh_labels(e1) |> fresh_labels(e2)
  | EBinFloatOp(_op, e1, e2) => acc |> fresh_labels(e1) |> fresh_labels(e2)
  /* Pair */
  | EPair(e1, e2) => acc |> fresh_labels(e1) |> fresh_labels(e2)
  /* Cons */
  | ECons(e1, e2) => acc |> fresh_labels(e1) |> fresh_labels(e2)
  /* Sum injection */
  | EInj(_other_ty, _side, e') => acc |> fresh_labels(e')
  /* Immediate expressions */
  | EBoundVar(_ty, _x) => acc
  | EBoolLit(_b) => acc
  | EIntLit(_n) => acc
  | EFloatLit(_f) => acc
  | ENil(_ty) => acc
  | ETriv => acc
  };
}

and fresh_labels_case = ({case_kind}, acc) =>
  switch (case_kind) {
  | ECase(scrut, rules) =>
    acc |> fresh_labels(scrut) |> fresh_labels_rules(rules)
  }

and fresh_labels_rules = (rules, acc) =>
  rules
  |> List.fold_left((acc, rule) => acc |> fresh_labels_rule(rule), acc)

and fresh_labels_rule = ({rule_kind, rule_label}: Hir_expr.rule, acc) => {
  let acc = acc |> fresh_labels_put_rule(rule_label);
  switch (rule_kind) {
  | ERule(p, body) => acc |> fresh_labels_pat(p) |> fresh_labels(body)
  };
}

and fresh_labels_sigma = (sigma, acc) =>
  acc |> Hir_expr.Sigma.fold((_x, e, acc) => acc |> fresh_labels(e), sigma)

and fresh_labels_pat = ({kind, label}, acc) => {
  let acc = acc |> fresh_labels_put_pat(label);
  switch (kind) {
  | PEmptyHole(_u, _i) => acc
  | PNonEmptyHole(_reason, _u, _i, p') => acc |> fresh_labels_pat(p')
  | PKeyword(_u, _i, _k) => acc
  | PInvalidText(_u, _i, _text) => acc
  | PAp(p1, p2)
  | PPair(p1, p2)
  | PCons(p1, p2) => acc |> fresh_labels_pat(p1) |> fresh_labels_pat(p2)
  | PInj(_side, p') => acc |> fresh_labels_pat(p')
  | PWild => acc
  | PVar(_x) => acc
  | PIntLit(_n) => acc
  | PFloatLit(_f) => acc
  | PBoolLit(_b) => acc
  | PNil => acc
  | PTriv => acc
  };
};

let fresh_labels = e =>
  Hir_expr.(Expr.Label.init, Expr.RuleLabel.init, Pat.Label.init)
  |> fresh_labels(e);
