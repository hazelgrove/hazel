[@deriving sexp]
type t = int;

let paren_l = 0;
let paren_body = 1;
let paren_r = 2;

let lam_lam = 0;
let lam_pat = 1;
let lam_dot = 2;

let let_let = 0;
let let_pat = 1;
let let_eq = 2;
let let_def = 3;
let let_in = 4;

let cond_then = 0;
