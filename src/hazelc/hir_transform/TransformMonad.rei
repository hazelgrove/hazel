open Hir_expr;

module State: {
  [@deriving sexp]
  type t = {
    expr: Expr.Label.Gen.t,
    rule: Expr.RuleLabel.Gen.t,
    pat: Pat.Label.Gen.t,
  };

  let init: t;
};

include Util.StateMonad.S with type state = State.t;

let init: State.t;

let next_expr_label: t(Expr.Label.t);
let next_rule_label: t(Expr.RuleLabel.t);
let next_pat_label: t(Pat.Label.t);
