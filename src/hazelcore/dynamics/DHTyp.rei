[@deriving sexp]
type t = (Context.t, HTyp.t);

let many: (Context.t, list(HTyp.t)) => list(t);

let normalize: t => HTyp.normalized;
let head_normalize: t => HTyp.head_normalized;

let equivalent: (t, t) => bool;
let consistent: (t, t) => bool;

let ground_cases_of: t => HTyp.ground_cases;

let is_hole: t => bool;
let is_tyvar_hole: t => bool;

let list: t => t;
