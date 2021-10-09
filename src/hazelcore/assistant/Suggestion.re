open Sexplib.Std;

[@deriving sexp]
type strategy =
  | Delete
  | InsertLit
  | InsertVar
  | InsertApp
  | InsertCase
  | WrapApp
  | WrapCase
  //| Convert // int-float
  | ReplaceOperator;

[@deriving sexp]
type score = {
  idiomaticity: float,
  type_specificity: float,
  delta_errors: float,
  syntax_conserved: float,
};

[@deriving sexp]
type result_exp_operand = {
  ty: HTyp.t,
  score,
  show_uhexp: UHExp.t,
  show_text: string,
};

[@deriving sexp]
type result =
  | ExpOperand(result_exp_operand);

[@deriving sexp]
type t = {
  strategy,
  action: Action.t,
  result,
};

[@deriving sexp]
type generator = CursorInfo.t => list(t);

let generate = (gs: list(generator), ci: CursorInfo.t): list(t) =>
  List.fold_left((sugs, g) => g(ci) @ sugs, [], gs);

let score_params = (score: score) => [
  (score.delta_errors, 1.),
  (score.idiomaticity, 1.),
  (score.type_specificity, 1.),
  (score.syntax_conserved, 1.5),
];

let score = ({result, _}: t) =>
  switch (result) {
  | ExpOperand({score, _}) =>
    score
    |> score_params
    |> List.map(((score, param)) => param *. score)
    |> List.fold_left((+.), 0.)
  };

let compare = (a1, a2) => Float.compare(score(a2), score(a1));
