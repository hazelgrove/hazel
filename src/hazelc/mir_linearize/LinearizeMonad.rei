open Mir_anf;

module State: {
  [@deriving sexp]
  type t;

  let init: FreshLabels.t => t;

  let next_tmp: t => (Ident.t, t);
  let next_tmp_named: (Ident.t, t) => (Ident.t, t);

  let next_expr_label: t => (ExprLabel.t, t);
  let next_rule_label: t => (RuleLabel.t, t);
  let next_stmt_label: t => (StmtLabel.t, t);
  let next_pat_label: t => (Pat.Label.t, t);

  let next_hir_expr_label: t => (Hir_expr.Expr.Label.t, t);
  let next_hir_rule_label: t => (Hir_expr.Expr.RuleLabel.t, t);
  let next_hir_pat_label: t => (Hir_expr.Pat.Label.t, t);
};

include Util.Monads.MONAD with type t('a) = State.t => (State.t, 'a);

let get: t(State.t);
let put: State.t => t(unit);

let sequence: list(t('a)) => t(list('a));

let init: FreshLabels.t => State.t;

let next_tmp: t(Ident.t);
let next_tmp_named: Ident.t => t(Ident.t);

let next_expr_label: t(ExprLabel.t);
let next_rule_label: t(RuleLabel.t);
let next_stmt_label: t(StmtLabel.t);
let next_pat_label: t(Pat.Label.t);

let next_hir_expr_label: t(Hir_expr.Expr.Label.t);
let next_hir_rule_label: t(Hir_expr.Expr.RuleLabel.t);
let next_hir_pat_label: t(Hir_expr.Pat.Label.t);
