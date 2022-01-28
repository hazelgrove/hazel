module Hole: {
  type t =
    | Expression(HTyp.t, VarCtx.t)
    | Type(Kind.t, TyCtx.t)
    | Pattern(HTyp.t, VarCtx.t);
};

type t = MetaVarMap.t(Hole.t);

let empty: t;

let union: (t, t) => t;

let add: (int, Hole.t, t) => t;
