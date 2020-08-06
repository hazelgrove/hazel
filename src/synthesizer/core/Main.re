open Types;
module IntMap = Map.Make(Int);
let main = ((exp, example): input): output => {
  let r = Eval.eval([], exp);
  let k = Unevaluator.unevaluate([], [], r, example);
  let (f, _) = Solver.solve(k, exp);
  f;
};
/* IntMap.(empty |> add(0, Types.Unit) |> add(17, Types.Unit)); */
