open Hir_expr;

module State = {
  [@deriving sexp]
  type t = {
    expr: Expr.Label.Gen.t,
    rule: Expr.RuleLabel.Gen.t,
    pat: Pat.Label.Gen.t,
  };

  let init = {
    expr: Expr.Label.Gen.init,
    rule: Expr.RuleLabel.Gen.init,
    pat: Pat.Label.Gen.init,
  };

  let next_expr_label = ({expr, _} as s) => {
    let (l, expr) = Expr.Label.Gen.next(expr);
    (l, {...s, expr});
  };

  let next_rule_label = ({rule, _} as s) => {
    let (l, rule) = Expr.RuleLabel.Gen.next(rule);
    (l, {...s, rule});
  };

  let next_pat_label = ({pat, _} as s) => {
    let (l, pat) = Pat.Label.Gen.next(pat);
    (l, {...s, pat});
  };
};

include Util.StateMonad.Make(State);

let init = State.init;

let next_expr_label = modify(State.next_expr_label);
let next_rule_label = modify(State.next_rule_label);
let next_pat_label = modify(State.next_pat_label);
