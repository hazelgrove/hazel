open Sexplib;

let mvar = MetaVarGen.init;

let%test "sample addition test" = {
  // 1
  let addition =
    DHExp.BinIntOp(DHExp.BinIntOp.Plus, DHExp.IntLit(1), DHExp.IntLit(1));

  Evaluator.evaluate(addition) == Evaluator.BoxedValue(DHExp.IntLit(2));
};

let%test "sample user op" = {
  let func_sym = "++";

  let lam =
    DHExp.Lam(
      DHPat.Var("x"),
      HTyp.Int,
      DHExp.Lam(
        DHPat.Var("y"),
        HTyp.Int,
        DHExp.BinIntOp(
          DHExp.BinIntOp.Plus,
          DHExp.IntLit(100),
          DHExp.BoundVar("x"),
        ),
      ),
    );

  let body =
    DHExp.BinUserOp(
      DHExp.BinUserOp.UserOp(func_sym),
      DHExp.IntLit(1),
      DHExp.IntLit(1),
    );

  let let_expr = DHExp.Let(DHPat.Var(func_sym), lam, body);

  let result = Evaluator.evaluate(let_expr);
  print_endline(Sexp.to_string(DHExp.sexp_of_t(let_expr)));
  print_endline(Sexp.to_string(Evaluator.sexp_of_result(result)));

  result == Evaluator.BoxedValue(DHExp.IntLit(101));
};
