let matches: (Contexts.t, TPat.t, HTyp.t, Kind.t) => Contexts.t;

let fix_holes:
  (Contexts.t, TPat.t, Kind.t, MetaVarGen.t) =>
  (Contexts.t, TPat.t, MetaVarGen.t);
let fix_holes_z:
  (Contexts.t, ZTPat.t, Kind.t, MetaVarGen.t) =>
  (Contexts.t, ZTPat.t, MetaVarGen.t);
