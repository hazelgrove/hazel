[@deriving sexp]
type t = Box.t(UHAnnot.t);

type with_splices = (t, SpliceMap.t(t));
