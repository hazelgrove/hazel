// Test suite for the step evaluator.
open DHExp;

let option_to_string = (t: option(DHExp.t)): string =>
  switch (t) {
  | Some(d) => d |> sexp_of_t |> Sexplib.Sexp.to_string
  | None => "None"
  };

let gen_ans = (r: DHExp.t): option(DHExp.t) =>
  switch (Evaluator.evaluate(r)) {
  | InvalidInput(_) => None
  | BoxedValue(d) => Some(d)
  | Indet(d) => Some(d)
  };

let testing = (t: DHExp.t): bool =>
  EvaluatorStep.step_evaluate(t) == gen_ans(t);

let%test "bin op test" = {
  let test = [
    BinIntOp(BinIntOp.Divide, IntLit(5), IntLit(0)),
    BinIntOp(BinIntOp.Times, IntLit(5), IntLit(4)),
    BinIntOp(BinIntOp.LessThan, IntLit(5), IntLit(4)),
    BinIntOp(BinIntOp.LessThan, BoolLit(true), IntLit(4)),
    BinBoolOp(BinBoolOp.And, BoolLit(true), IntLit(4)),
    BinFloatOp(BinFloatOp.FPlus, FloatLit(5.5), FloatLit(4.0)),
  ];

  List.map(testing, test) |> List.fold_left((&&), true);
};
