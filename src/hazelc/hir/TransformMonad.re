module ExprLabelGen = Label_.Gen.Make(Expr.Label);
module RuleLabelGen = Label_.Gen.Make(Expr.RuleLabel);
module PatLabelGen = Label_.Gen.Make(Pat.Label);

module State = {
  [@deriving sexp]
  type t = {
    expr: ExprLabelGen.t,
    rule: RuleLabelGen.t,
    pat: PatLabelGen.t,
  };

  let init = {
    expr: ExprLabelGen.init,
    rule: RuleLabelGen.init,
    pat: PatLabelGen.init,
  };

  let next_expr_label = ({expr, _} as s) => {
    let (l, expr) = ExprLabelGen.next(expr);
    (l, {...s, expr});
  };

  let next_rule_label = ({rule, _} as s) => {
    let (l, rule) = RuleLabelGen.next(rule);
    (l, {...s, rule});
  };

  let next_pat_label = ({pat, _} as s) => {
    let (l, pat) = PatLabelGen.next(pat);
    (l, {...s, pat});
  };
};

include StateMonad.Make(State);

let init = State.init;

let next_expr_label = modify(State.next_expr_label);
let next_rule_label = modify(State.next_rule_label);
let next_pat_label = modify(State.next_pat_label);
