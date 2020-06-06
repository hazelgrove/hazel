include NodeInstanceInfo;
open Sexplib.Std;

[@deriving sexp]
type t = NodeInstanceInfo.t(SpliceInfo.t(option(DHExp.t)));
