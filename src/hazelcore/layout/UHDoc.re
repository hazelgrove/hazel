open Pretty;

[@deriving sexp]
type t = Doc.t(UHAnnot.t);
