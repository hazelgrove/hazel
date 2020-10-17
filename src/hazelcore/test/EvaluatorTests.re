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

  let let_expr = DHExp.Let(DHPat.Var("_++_"), lam, body);

  Evaluator.evaluate(let_expr) == Evaluator.BoxedValue(DHExp.IntLit(101));
};
