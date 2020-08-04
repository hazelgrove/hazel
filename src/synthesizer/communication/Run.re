let run = (e: UHExp.t, ex: Types.example) => {
  let e' = Shim.uHExpToExp(e) |> Shim.collapseBlock;
  let r = Eval.eval([], e');
  let k = Unevaluator.unevaluate([], [], r, ex);
  let (f, _) = Solver.solve(k, e');
  f;
};
