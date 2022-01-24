let matches: (Contexts.t, TPat.t, HTyp.t, Kind.t('a)) => Contexts.t;

let fix_holes: (Contexts.t, TPat.t, Kind.t('a)) => (Contexts.t, TPat.t);
let fix_holes_z: (Contexts.t, ZTPat.t, Kind.t('a)) => (Contexts.t, ZTPat.t);
