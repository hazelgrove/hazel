[@deriving sexp]
type t = MetaVarMap.t(MetaVarInst.t);

let init = MetaVarMap.empty;

let lookup = (u, si) => MetaVarMap.lookup(si, u);

let insert_or_update = (inst: HoleInstance.t, usi: t): t =>
  MetaVarMap.insert_or_update(usi, inst);
