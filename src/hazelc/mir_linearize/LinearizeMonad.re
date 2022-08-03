open Mir_anf;

module ExprLabelGen = Label.Gen.Make(ExprLabel);
module RuleLabelGen = Label.Gen.Make(RuleLabel);
module StmtLabelGen = Label.Gen.Make(StmtLabel);
module PatLabelGen = Label.Gen.Make(Pat.Label);

module State = {
  [@deriving sexp]
  type t = {
    t_gen: TmpVarGen.t,
    expr_l_gen: ExprLabelGen.t,
    rule_l_gen: RuleLabelGen.t,
    stmt_l_gen: StmtLabelGen.t,
    pat_l_gen: PatLabelGen.t,
  };

  let init = {
    t_gen: TmpVarGen.init,
    expr_l_gen: ExprLabelGen.init,
    rule_l_gen: RuleLabelGen.init,
    stmt_l_gen: StmtLabelGen.init,
    pat_l_gen: PatLabelGen.init,
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
    let (l, expr_l_gen) = ExprLabelGen.next(expr_l_gen);
    (l, {...state, expr_l_gen});
  };

  let next_rule_label = ({rule_l_gen, _} as state) => {
    let (l, rule_l_gen) = RuleLabelGen.next(rule_l_gen);
    (l, {...state, rule_l_gen});
  };

  let next_stmt_label = ({stmt_l_gen, _} as state) => {
    let (l, stmt_l_gen) = StmtLabelGen.next(stmt_l_gen);
    (l, {...state, stmt_l_gen});
  };

  let next_pat_label = ({pat_l_gen, _} as state) => {
    let (l, pat_l_gen) = PatLabelGen.next(pat_l_gen);
    (l, {...state, pat_l_gen});
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
