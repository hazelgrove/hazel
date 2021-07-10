let matches: (Contexts.t, TPat.t, HTyp.t, Kind.t) => Contexts.t;

let fix_holes: (Contexts.t, TPat.t, Kind.t) => (Contexts.t, TPat.t);
let fix_holes_z: (Contexts.t, ZTPat.t, Kind.t) => (Contexts.t, ZTPat.t);
