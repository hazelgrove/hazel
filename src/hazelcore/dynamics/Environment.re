include VarMap;

[@deriving sexp]
type nonrec t = t(DHExp.t);

let id_env = (vars: VarMap.t(HTyp.t)): t =>
  VarMap.map(
    xt => {
      let (x, _) = xt;
      DHExp.BoundVar(x);
    },
    vars,
  );
