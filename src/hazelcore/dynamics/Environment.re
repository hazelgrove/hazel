include VarMap;

[@deriving sexp]
type nonrec t = t(DHExp.t);

let id_env = (ctx: Contexts.t): t =>
  Contexts.vars(ctx)
  |> List.map(((_, x, _)) => (x, DHExp.BoundVar(x)))
  |> VarMap.of_list;
