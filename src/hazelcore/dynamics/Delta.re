module Hole = {
  type t =
    | Expression(HTyp.t, VarCtx.t)
    | Pattern(HTyp.t, VarCtx.t)
    | Type;
};

include MetaVarMap;
type t = MetaVarMap.t(Hole.t);

let empty: t = (MetaVarMap.empty: t);

let union = (d1, d2) => MetaVarMap.union((_, a, _) => Some(a), d1, d2);
