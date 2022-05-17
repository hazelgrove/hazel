[@deriving sexp]
type context = VarMap.t_((HTyp.t, Hir.has_indet));

let transform: (context, DHExp.t) => Hir.expr;
