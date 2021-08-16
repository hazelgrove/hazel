open Pretty;

[@deriving (sexp, show)]
type t = Doc.t(DHAnnot.t);
