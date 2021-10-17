open Sexplib.Std;

[@deriving sexp]
type scores = unit;

[@deriving sexp]
type operand_report = {
  show_text: string,
  scores,
};

let scores_params = (_: scores) => [];

let mk_operand_report =
    (operand: UHTyp.operand, _ci: CursorInfo.t): operand_report => {
  let show_text = HTyp.to_string(UHTyp.expand_operand(operand));
  let scores = ();
  {show_text, scores};
};
