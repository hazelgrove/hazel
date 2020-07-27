[@deriving sexp]
type exp =
  | Assert
  | Case
  | Other(expl)
and expl =
  | Nil
  | Cons(exp, expl);
