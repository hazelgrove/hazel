[@deriving sexp]
type t = (Contexts.t, HTyp.t);

let wrap: HTyp.t => t;

let many: (Contexts.t, list(HTyp.t)) => list(t);

let normalize: t => HTyp.normalized;
let head_normalize: t => HTyp.head_normalized;

let equivalent: (t, t) => bool;
let consistent: (t, t) => bool;

let ground_cases_of: t => HTyp.ground_cases;

let is_hole: t => bool;

let list: t => t;
