open Sexplib.Std;

[@deriving sexp]
type t = {
  rule_err_holes: list(CursorPath.steps),
  case_err_holes: (list(CursorPath.steps), list(CursorPath.steps)),
  err_holes: list(CursorPath.steps),
  var_err_holes: list(CursorPath.steps),
  var_uses: list(CursorPath.steps),
  tyvar_uses: list(CursorPath.steps),
  current_term: option(CursorPath.t),
};

let is_empty = (dpaths: t): bool =>
  ListUtil.is_empty(dpaths.rule_err_holes)
  && ListUtil.is_empty(fst(dpaths.case_err_holes))
  && ListUtil.is_empty(snd(dpaths.case_err_holes))
  && ListUtil.is_empty(dpaths.err_holes)
  && ListUtil.is_empty(dpaths.var_err_holes)
  && ListUtil.is_empty(dpaths.var_uses)
  && ListUtil.is_empty(dpaths.tyvar_uses)
  && dpaths.current_term == None;

let take_step = (step: int, dpaths: t): t => {
  let {
    rule_err_holes,
    case_err_holes,
    err_holes,
    var_err_holes,
    current_term,
    var_uses,
    tyvar_uses,
  } = dpaths;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let rule_err_holes = rule_err_holes |> List.filter_map(remove_step);
  let case_err_holes = (
    fst(case_err_holes) |> List.filter_map(remove_step),
    snd(case_err_holes) |> List.filter_map(remove_step),
  );
  let err_holes = err_holes |> List.filter_map(remove_step);
  let var_err_holes = var_err_holes |> List.filter_map(remove_step);
  let var_uses = var_uses |> List.filter_map(remove_step);
  let tyvar_uses = tyvar_uses |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  {
    rule_err_holes,
    case_err_holes,
    err_holes,
    var_err_holes,
    var_uses,
    tyvar_uses,
    current_term,
  };
};

let current = (shape: TermShape.t, dpaths: t): list(UHDecorationShape.t) => {
  let is_current = steps =>
    switch (shape) {
    | SubBlock({hd_index, _}) => steps == [hd_index]
    | NTuple({comma_indices, _}) =>
      List.exists(n => steps == [n], comma_indices)
    | BinOp({op_index, _}) => steps == [op_index]
    | Operand
    | Case
    | Rule => steps == []
    };
  let rule_err_holes =
    dpaths.rule_err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.RuleErrHole)
    |> Option.to_list;
  let case_err_holes_notex =
    dpaths.case_err_holes
    |> fst
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.CaseErrHole(NotExhaustive))
    |> Option.to_list;
  let case_err_holes_incon =
    dpaths.case_err_holes
    |> snd
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.CaseErrHole(InconsistentBranches))
    |> Option.to_list;
  let err_holes =
    dpaths.err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.ErrHole)
    |> Option.to_list;
  let var_err_holes =
    dpaths.var_err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.VarErrHole)
    |> Option.to_list;
  let var_uses =
    dpaths.var_uses
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.VarUse)
    |> Option.to_list;
  let tyvar_uses =
    dpaths.tyvar_uses
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.TyVarUse)
    |> Option.to_list;
  let current_term =
    switch (dpaths.current_term) {
    | Some((steps, _)) when is_current(steps) => [
        UHDecorationShape.CurrentTerm,
      ]
    | _ => []
    };
  List.concat([
    rule_err_holes,
    case_err_holes_notex,
    case_err_holes_incon,
    err_holes,
    var_err_holes,
    var_uses,
    tyvar_uses,
    current_term,
  ]);
};
