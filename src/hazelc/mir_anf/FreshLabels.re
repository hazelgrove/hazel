open Anf;

[@deriving sexp]
type t = (ExprLabel.t, StmtLabel.t, RuleLabel.t, Pat.Label.t);

let put_expr = (expr', (expr, stmt, rule, pat)) => (
  ExprLabel.max(expr, expr'),
  stmt,
  rule,
  pat,
);

let put_stmt = (stmt', (expr, stmt, rule, pat)) => (
  expr,
  StmtLabel.max(stmt, stmt'),
  rule,
  pat,
);

let put_rule = (rule', (expr, stmt, rule, pat)) => (
  expr,
  stmt,
  RuleLabel.max(rule, rule'),
  pat,
);

let put_pat = (pat', (expr, stmt, rule, pat)) => (
  expr,
  stmt,
  rule,
  Pat.Label.max(pat, pat'),
);

let rec fresh_labels =
        ({block_body: (stmts, im), block_label, _}: block, acc) =>
  acc
  |> put_expr(block_label)
  |> fresh_labels_stmts(stmts)
  |> fresh_labels_imm(im)

and fresh_labels_stmts = (stmts, acc) =>
  stmts |> List.fold_left((acc, stmt) => fresh_labels_stmt(stmt, acc), acc)

and fresh_labels_stmt = ({stmt_kind, stmt_label, _}, acc) => {
  let acc = acc |> put_stmt(stmt_label);
  switch (stmt_kind) {
  | SLet(_x, c) => acc |> fresh_labels_comp(c)
  | SLetRec(_f, _param, _param_ty, _o_ty, body) => acc |> fresh_labels(body)
  };
}

and fresh_labels_comp = ({comp_kind, comp_label, _}, acc) => {
  let acc = acc |> put_expr(comp_label);
  switch (comp_kind) {
  | CImm(im) => acc |> fresh_labels_imm(im)
  | CBinOp(_op, im1, im2) =>
    acc |> fresh_labels_imm(im1) |> fresh_labels_imm(im2)
  | CAp(im1, im2) => acc |> fresh_labels_imm(im1) |> fresh_labels_imm(im2)
  | CFun(_param, _param_ty, body) => acc |> fresh_labels(body)
  | CCons(im1, im2) => acc |> fresh_labels_imm(im1) |> fresh_labels_imm(im2)
  | CPair(im1, im2) => acc |> fresh_labels_imm(im1) |> fresh_labels_imm(im2)
  | CInj(_other_ty, _side, im) => acc |> fresh_labels_imm(im)
  | CCase(scrut, rules) =>
    acc |> fresh_labels_imm(scrut) |> fresh_labels_rules(rules)
  | CEmptyHole(_u, _i, sigma) => acc |> fresh_labels_sigma(sigma)
  | CNonEmptyHole(_reason, _u, _i, sigma, im) =>
    acc |> fresh_labels_sigma(sigma) |> fresh_labels_imm(im)
  | CCast(im, _ty, _ty') => acc |> fresh_labels_imm(im)
  };
}

and fresh_labels_imm = ({imm_kind, imm_label, _}, acc) => {
  let acc = acc |> put_expr(imm_label);
  switch (imm_kind) {
  | IConst(_)
  | IVar(_) => acc
  };
}

and fresh_labels_rules = (rules, acc) =>
  rules
  |> List.fold_left((acc, rule) => acc |> fresh_labels_rule(rule), acc)

and fresh_labels_rule = ({rule_pat, rule_branch, rule_label, _}, acc) =>
  acc
  |> put_rule(rule_label)
  |> fresh_labels_pat(rule_pat)
  |> fresh_labels(rule_branch)

and fresh_labels_sigma = (sigma, acc) =>
  acc |> Sigma.fold((_x, e, acc) => acc |> fresh_labels_imm(e), sigma)

and fresh_labels_pat = ({kind, label, _}, acc) => {
  let acc = acc |> put_pat(label);
  switch (kind) {
  | PPair(p1, p2)
  | PCons(p1, p2) => acc |> fresh_labels_pat(p1) |> fresh_labels_pat(p2)
  | PInj(_side, p') => acc |> fresh_labels_pat(p')
  | PWild => acc
  | PVar(_x) => acc
  | PInt(_n) => acc
  | PFloat(_f) => acc
  | PBool(_b) => acc
  | PNil => acc
  | PTriv => acc
  };
};

let fresh_labels = e =>
  (ExprLabel.init, StmtLabel.init, RuleLabel.init, Pat.Label.init)
  |> fresh_labels(e);
