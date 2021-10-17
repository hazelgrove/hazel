open Sexplib.Std;

[@deriving sexp]
type scores = {
  expression_consistency: float,
  pattern_consistency: float,
};

[@deriving sexp]
type operand_report = {
  show_text: string,
  scores,
};

let scores_params = (score: scores) => [
  (score.expression_consistency, 1.),
  (score.pattern_consistency, 1.),
];

let score_type_consistency = (ty: HTyp.t, ty': HTyp.t): float =>
  switch (HTyp.compare(ty, ty')) {
  | _ when !HTyp.consistent(ty, ty') => (-1.0)
  | _ when ty' == Hole => 0.0
  | LT
  | Incomparable => 0.5
  | Equal => 1.0
  | GT => 2.0
  };

let mk_operand_score = (~result_ty: HTyp.t, {typed, _}: CursorInfo.t): scores =>
  switch (typed) {
  | OnType({analyzed_ty, pattern_ty, ann_ty: _}) =>
    let expression_consistency =
      score_type_consistency(result_ty, analyzed_ty);
    let pattern_consistency = score_type_consistency(result_ty, pattern_ty);
    {expression_consistency, pattern_consistency};
  | _ => {expression_consistency: 0., pattern_consistency: 0.}
  };

let mk_operand_report =
    (operand: UHTyp.operand, ci: CursorInfo.t): operand_report => {
  let show_text = HTyp.to_string(UHTyp.expand_operand(operand));
  let result_ty = UHTyp.expand_operand(operand);
  // TODO(andrew): this isn't quite working.
  // CursorInfo_typ needs to actually process the provided analytic/pattern
  // types as we move down thru the type
  let scores = mk_operand_score(~result_ty, ci);
  {show_text, scores};
};
