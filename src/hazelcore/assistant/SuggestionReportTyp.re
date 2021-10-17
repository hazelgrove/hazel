open Sexplib.Std;

[@deriving sexp]
type scores = {
  analysis_consistency: float,
  pattern_consistency: float,
};

[@deriving sexp]
type operand_report = {
  show_text: string,
  scores,
};

let scores_params = (score: scores) => [
  (score.analysis_consistency, 1.),
  (score.pattern_consistency, 1.),
];

let mk_operand_report =
    (operand: UHTyp.operand, {typed, _}: CursorInfo.t): operand_report => {
  let show_text = HTyp.to_string(UHTyp.expand_operand(operand));
  let result_ty = UHTyp.expand_operand(operand);
  // TODO(andrew): this isn't quite working.
  // CursorInfo_typ needs to actually process the provided analytic/pattern
  // types as we move down thru the type
  let scores: scores =
    switch (typed) {
    | OnType({analyzed_ty, pattern_ty, ann_ty: _}) =>
      let analysis_consistency =
        result_ty != HTyp.Hole
        && analyzed_ty != HTyp.Hole
        && HTyp.consistent(analyzed_ty, result_ty)
          ? analyzed_ty == result_ty ? 1.5 : 1.0 : 0.0;
      let pattern_consistency =
        result_ty != HTyp.Hole
        && pattern_ty != HTyp.Hole
        && HTyp.consistent(pattern_ty, result_ty)
          ? pattern_ty == result_ty ? 1.5 : 1.0 : 0.0;
      {analysis_consistency, pattern_consistency};
    | _ => {analysis_consistency: 0., pattern_consistency: 0.}
    };
  {show_text, scores};
};
