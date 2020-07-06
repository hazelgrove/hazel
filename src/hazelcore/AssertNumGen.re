[@deriving sexp]
type t = AssertNum.t;
let init = 0;
let next = x => {
  let n = x + 1;
  (x, n);
};
