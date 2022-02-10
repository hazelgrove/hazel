module Hole: {
  type t =
    | Expression(HTyp.t, VarCtx.t)
    | Pattern(HTyp.t, VarCtx.t)
    | Type;
};

type t = MetaVarMap.t(Hole.t);

let empty: t;

let union: (t, t) => t;

let add: (int, Hole.t, t) => t;
