open Sexplib.Std;

[@deriving sexp]
type scores = {delta_errors: float};

[@deriving sexp]
type operand_report_pat = {
  result_ty: HTyp.t,
  show_text: string,
  scores,
};

let scores_params = (score: scores) => [(score.delta_errors, 1.0)];

let err_holes_operand = (operand: UHPat.operand): list(CursorPath.hole_info) =>
  CursorPath_Pat.holes_operand(operand, [], [])
  |> List.filter(CursorPath.hole_not_empty);

let mk_operand_score =
    (
      ~operand: UHPat.operand,
      ~result_ty as _: HTyp.t,
      {cursor_term, _}: CursorInfo.t,
    )
    : scores => {
  switch (cursor_term) {
  | PatOperand(_, old_op) =>
    let internal_errors_before = old_op |> err_holes_operand |> List.length;
    let internal_errors_after = operand |> err_holes_operand |> List.length;
    let delta_errors = internal_errors_before - internal_errors_after;
    {delta_errors: float_of_int(delta_errors)};
  | _ => {delta_errors: 0.}
  };
};

let mk_pat_operand_report =
    (operand: UHPat.operand, ci: CursorInfo.t): operand_report_pat => {
  let result_ty =
    HTyp.relax(Statics_Pat.syn_operand(ci.ctx, operand) |> Option.map(fst));
  let show_text = UHPat.string_of_operand(operand);
  let scores = mk_operand_score(~operand, ~result_ty, ci);
  {result_ty, show_text, scores};
};
