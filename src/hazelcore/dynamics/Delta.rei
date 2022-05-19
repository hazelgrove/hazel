module Hole: {
  type t =
    | Expression(HTyp.t, Context.t)
    | Pattern(HTyp.t, Context.t)
    | Type;
};

type t = MetaVarMap.t(Hole.t);

let empty: t;

let union: (t, t) => t;

let add: (int, Hole.t, t) => t;

let subst_tyvars: (t, list((Index.Abs.t, HTyp.t))) => t;

let sexp_of_t: t => Sexplib.Sexp.t;
let t_of_sexp: Sexplib.Sexp.t => t;
