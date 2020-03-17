[@deriving sexp]
type t = MetaVarMap.t(MetaVarInst.t);

let init = MetaVarMap.empty;

let lookup = (u, si) => MetaVarMap.lookup(si, u);

let insert_or_update = (inst: NodeInstance.t, usi: t): t =>
  MetaVarMap.insert_or_update(usi, inst);
