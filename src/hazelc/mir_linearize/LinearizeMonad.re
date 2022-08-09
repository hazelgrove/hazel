open Mir_anf;

module State = {
  [@deriving sexp]
  type t = {
    t_gen: TmpVarGen.t,
    expr_l_gen: ExprLabel.Gen.t,
    rule_l_gen: RuleLabel.Gen.t,
    stmt_l_gen: StmtLabel.Gen.t,
    pat_l_gen: Pat.Label.Gen.t,
    hir_expr_l_gen: Hir_expr.Expr.Label.Gen.t,
    hir_rule_l_gen: Hir_expr.Expr.RuleLabel.Gen.t,
    hir_pat_l_gen: Hir_expr.Pat.Label.Gen.t,
  };

  let init = ((hir_expr_l, hir_rule_l, hir_pat_l)) => {
    t_gen: TmpVarGen.init,
    expr_l_gen: ExprLabel.Gen.init,
    rule_l_gen: RuleLabel.Gen.init,
    stmt_l_gen: StmtLabel.Gen.init,
    pat_l_gen: Pat.Label.Gen.init,
    hir_expr_l_gen: Hir_expr.Expr.Label.Gen.of_label(hir_expr_l),
    hir_rule_l_gen: Hir_expr.Expr.RuleLabel.Gen.of_label(hir_rule_l),
    hir_pat_l_gen: Hir_expr.Pat.Label.Gen.of_label(hir_pat_l),
  };

  let next_tmp = state => {
    let (x, t_gen) = TmpVarGen.next(state.t_gen);
    (x, {...state, t_gen});
  };

  let next_tmp_named = (x, state) => {
    let (x', t_gen) = TmpVarGen.next_named(x, state.t_gen);
    (x', {...state, t_gen});
  };

  let next_expr_label = ({expr_l_gen, _} as state) => {
    let (l, expr_l_gen) = ExprLabel.Gen.next(expr_l_gen);
    (l, {...state, expr_l_gen});
  };

  let next_rule_label = ({rule_l_gen, _} as state) => {
    let (l, rule_l_gen) = RuleLabel.Gen.next(rule_l_gen);
    (l, {...state, rule_l_gen});
  };

  let next_stmt_label = ({stmt_l_gen, _} as state) => {
    let (l, stmt_l_gen) = StmtLabel.Gen.next(stmt_l_gen);
    (l, {...state, stmt_l_gen});
  };

  let next_pat_label = ({pat_l_gen, _} as state) => {
    let (l, pat_l_gen) = Pat.Label.Gen.next(pat_l_gen);
    (l, {...state, pat_l_gen});
  };

  let next_hir_expr_label = ({hir_expr_l_gen, _} as state) => {
    let (l, hir_expr_l_gen) = Hir_expr.Expr.Label.Gen.next(hir_expr_l_gen);
    (l, {...state, hir_expr_l_gen});
  };

  let next_hir_rule_label = ({hir_rule_l_gen, _} as state) => {
    let (l, hir_rule_l_gen) =
      Hir_expr.Expr.RuleLabel.Gen.next(hir_rule_l_gen);
    (l, {...state, hir_rule_l_gen});
  };

  let next_hir_pat_label = ({hir_pat_l_gen, _} as state) => {
    let (l, hir_pat_l_gen) = Hir_expr.Pat.Label.Gen.next(hir_pat_l_gen);
    (l, {...state, hir_pat_l_gen});
  };
};

include Util.StateMonad.Make(State);
open Syntax;

let sequence = ms => {
  let rec sequence' = (ms, acc) => {
    switch (ms) {
    | [] => acc
    | [m, ...ms] =>
      let* x = m;
      sequence'(ms, acc >>| (acc => [x, ...acc]));
    };
  };

  sequence'(ms, [] |> return) >>| List.rev;
};

let init = State.init;

let next_tmp = modify(State.next_tmp);
let next_tmp_named = x => modify(State.next_tmp_named(x));

let next_expr_label = modify(State.next_expr_label);
let next_rule_label = modify(State.next_rule_label);
let next_stmt_label = modify(State.next_stmt_label);
let next_pat_label = modify(State.next_pat_label);
let next_hir_expr_label = modify(State.next_hir_expr_label);
let next_hir_rule_label = modify(State.next_hir_rule_label);
let next_hir_pat_label = modify(State.next_hir_pat_label);
