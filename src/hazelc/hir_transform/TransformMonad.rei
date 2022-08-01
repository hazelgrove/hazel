open Hir_expr;

module ExprLabelGen: Label.Gen.S with type label = Expr.Label.t;
module RuleLabelGen: Label.Gen.S with type label = Expr.RuleLabel.t;
module PatLabelGen: Label.Gen.S with type label = Pat.Label.t;

module State: {
  [@deriving sexp]
  type t = {
    expr: ExprLabelGen.t,
    rule: RuleLabelGen.t,
    pat: PatLabelGen.t,
  };

  let init: t;
};

include Util.StateMonad.S with type state = State.t;

let init: State.t;

let next_expr_label: t(Expr.Label.t);
let next_rule_label: t(Expr.RuleLabel.t);
let next_pat_label: t(Pat.Label.t);
