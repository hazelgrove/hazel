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

let subst_tyvars = (delta: t, tyvars: list((Index.Abs.t, HTyp.t))): t =>
  MetaVarMap.map(
    fun
    | Hole.Expression(ty, ctx) => {
        let ty = HTyp.subst_tyvars(ty, tyvars);
        Hole.Expression(ty, ctx);
      }
    | Pattern(ty, ctx) => {
        let ty = HTyp.subst_tyvars(ty, tyvars);
        Pattern(ty, ctx);
      }
    | Type as t => t,
    delta,
  );

let sexp_of_t = (delta: t): Sexplib.Sexp.t =>
  IntMap.sexp_of_t(Hole.sexp_of_t, delta);

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  IntMap.t_of_sexp(Hole.t_of_sexp, sexp);
