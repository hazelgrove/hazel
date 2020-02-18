[@deriving sexp]
type t = MetaVarMap.t(MetaVarInst.t);

let init = MetaVarMap.empty;

let update = (inst: HoleInstance.t, usi: t): t =>
  MetaVarMap.insert_or_update(usi, inst);
