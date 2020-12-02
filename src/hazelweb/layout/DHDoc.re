open Pretty;

[@deriving sexp]
type t = Doc.t(DHAnnot.t);
