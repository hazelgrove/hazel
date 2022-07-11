open Pretty;

[@deriving sexp]
type t = Layout.t(HTypAnnot.t);
