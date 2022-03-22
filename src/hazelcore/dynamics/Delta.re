module Hole = {
  [@deriving sexp]
  type t =
    | Expression(HTyp.t, VarCtx.t)
    | Pattern(HTyp.t, VarCtx.t)
    | Type;
};

include MetaVarMap;
type t = MetaVarMap.t(Hole.t);

let empty: t = (MetaVarMap.empty: t);

let union = (d1, d2) => MetaVarMap.union((_, a, _) => Some(a), d1, d2);

let subst_tyvar = (delta: t, i: Index.t, ty: HTyp.t): t =>
  IntMap.bindings(delta)
  |> List.map(((u, hole)) =>
       switch (hole) {
       | Hole.Expression(ty_u, gamma) => (
           u,
           Hole.Expression(
             HTyp.subst(ty_u, i, ty),
             VarCtx.subst_tyvar(gamma, i, ty),
           ),
         )
       | Pattern(ty_u, gamma) => (u, Pattern(ty_u, gamma))
       | Type => (u, Type)
       }
     )
  |> List.to_seq
  |> IntMap.of_seq;
