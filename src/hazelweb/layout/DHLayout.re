open Pretty;

[@deriving (sexp, show)]
type t = Layout.t(DHAnnot.t);
