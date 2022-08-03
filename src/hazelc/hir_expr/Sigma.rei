include (module type of Ident.Map);

[@deriving sexp]
type t = Expr.sigma;
