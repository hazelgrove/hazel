open Mir_anf;
open Holes;

module TmpIdentGen = Ident.NumberedGen;

module State = {
  [@deriving sexp]
  type t = {
    t_gen: TmpIdentGen.t,
    expr_l_gen: ExprLabel.Gen.t,
    rule_l_gen: RuleLabel.Gen.t,
    stmt_l_gen: StmtLabel.Gen.t,
    pat_l_gen: Pat.Label.Gen.t,
    hir_expr_l_gen: Hir_expr.Expr.Label.Gen.t,
    hir_rule_l_gen: Hir_expr.Expr.RuleLabel.Gen.t,
    hir_pat_l_gen: Hir_expr.Pat.Label.Gen.t,
    hole_renamings: MetaVarMap.t(Renamings.t),
  };

  let init = ((hir_expr_l, hir_rule_l, hir_pat_l)) => {
    t_gen: TmpIdentGen.init(),
    expr_l_gen: ExprLabel.Gen.init,
    rule_l_gen: RuleLabel.Gen.init,
    stmt_l_gen: StmtLabel.Gen.init,
    pat_l_gen: Pat.Label.Gen.init,
    hir_expr_l_gen: Hir_expr.Expr.Label.Gen.of_label(hir_expr_l),
    hir_rule_l_gen: Hir_expr.Expr.RuleLabel.Gen.of_label(hir_rule_l),
    hir_pat_l_gen: Hir_expr.Pat.Label.Gen.of_label(hir_pat_l),
    hole_renamings: MetaVarMap.empty,
  };

  let next_tmp = state => {
    let (x, t_gen) = TmpIdentGen.next(state.t_gen);
    (x, {...state, t_gen});
  };

  let next_tmp_named = (x, state) => {
    let (x', t_gen) = TmpIdentGen.next_named(x, state.t_gen);
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

  let extend_hole_renamings = (u, renamings, {hole_renamings, _} as state) => {
    ...state,
    hole_renamings: hole_renamings |> MetaVarMap.add(u, renamings),
  };

  let get_hole_renamings = ({hole_renamings, _}) => hole_renamings;
};

include Util.StateMonad.Make(State);

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

let extend_hole_renamings = (u, renamings) =>
  update(State.extend_hole_renamings(u, renamings));
