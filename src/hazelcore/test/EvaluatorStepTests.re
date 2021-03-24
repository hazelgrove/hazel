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
  EvaluatorStep.step_evaluate(t, EvaluatorStep.default_option) == gen_ans(t);

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

let%test "fibo" = {
  let t = Sys.time();
  let program: DHExp.t =
    "(Let(Var fibo)(Cast(FixF fibo(Arrow Hole Int)(Lam(Var x)Hole(ConsistentCase(Case(BinIntOp LessThan(Cast(BoundVar x)Hole Int)(IntLit 3))((Rule(BoolLit true)(IntLit 1))(Rule(BoolLit false)(BinIntOp Plus(Ap(Cast(BoundVar fibo)(Arrow Hole Int)(Arrow Int Int))(BinIntOp Minus(Cast(BoundVar x)Hole Int)(IntLit 1)))(Ap(Cast(BoundVar fibo)(Arrow Hole Int)(Arrow Int Int))(BinIntOp Minus(Cast(BoundVar x)Hole Int)(IntLit 2))))))0))))(Arrow Hole Int)(Arrow Int Int))(Ap(BoundVar fibo)(IntLit 4)))"
    |> Sexplib.Sexp.of_string
    |> DHExp.t_of_sexp;
  Format.printf("Fibo:\n");
  let _ = Evaluator.evaluate(program);
  Format.printf("evaluate\tExecution time: %fs\n", Sys.time() -. t);
  let t = Sys.time();
  let _ =
    EvaluatorStep.quick_step_evaluate(program, EvaluatorStep.default_option);
  Format.printf("quick_step\tExecution time: %fs\n", Sys.time() -. t);
  let t = Sys.time();
  let _ = EvaluatorStep.step_evaluate(program, EvaluatorStep.default_option);
  Format.printf("step_evaluate\tExecution time: %fs\n", Sys.time() -. t);
  // switch (EvaluatorStep.quick_step_evaluate(program)) {
  // | BoxedValue(d)
  // | Indet(d) => print_string(d |> sexp_of_t |> Sexplib.Sexp.to_string)
  // | _ => print_string("Error")
  // };

  true;
};
