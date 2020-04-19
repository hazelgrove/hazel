include NodeInstanceInfo;

[@deriving sexp]
type t = NodeInstanceInfo.t(SpliceInfo.t(DHExp.t));
