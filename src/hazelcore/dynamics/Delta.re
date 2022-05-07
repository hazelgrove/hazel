module Hole = {
  [@deriving sexp]
  type t =
    | Expression(HTyp.t, Contexts.t)
    | Pattern(HTyp.t, Contexts.t)
    | Type;
};

include MetaVarMap;
type t = MetaVarMap.t(Hole.t);

let empty: t = (MetaVarMap.empty: t);

let union = (d1, d2) => MetaVarMap.union((_, a, _) => Some(a), d1, d2);

let subst_tyvar = (delta: t, i: Index.Abs.t, ty: HTyp.t): t =>
  IntMap.bindings(delta)
  |> List.map(((u, hole)) =>
       switch (hole) {
       | Hole.Expression(ty_u, ctx) => (
           u,
           Hole.Expression(
             HTyp.subst(ty_u, i, ty),
             Contexts.subst_tyvar(ctx, i, ty),
           ),
         )
       | Pattern(ty_u, ctx) => (u, Pattern(ty_u, ctx))
       | Type => (u, Type)
       }
     )
  |> List.to_seq
  |> IntMap.of_seq;

let eliminate_tyvars = (delta: t, tyvars: list((TyVar.t, Kind.t))): t =>
  MetaVarMap.map(
    fun
    | Hole.Expression(ty, ctx) => {
        let ty = HTyp.eliminate_tyvars(ty, tyvars);
        let ctx = Contexts.eliminate_tyvars(ctx, tyvars);
        Hole.Expression(ty, ctx);
      }
    | Pattern(ty, ctx) => {
        let ty = HTyp.eliminate_tyvars(ty, tyvars);
        let ctx = Contexts.eliminate_tyvars(ctx, tyvars);
        Pattern(ty, ctx);
      }
    | Type as t => t,
    delta,
  );

let sexp_of_t = (delta: t): Sexplib.Sexp.t =>
  IntMap.sexp_of_t(Hole.sexp_of_t, delta);

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  IntMap.t_of_sexp(Hole.t_of_sexp, sexp);
