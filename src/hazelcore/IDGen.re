/* A simple IDVariable generator */
[@deriving sexp]
type t = (MetaVar.t, AssertNumber.t);
let init = (0, 0);
let init_hole = x => {
  switch (x) {
  | (_, b) => (0, b)
  };
};
let next_hole = x => {
  switch (x) {
  | (a, b) =>
    let n = (a + 1, b);
    (a + 1, n);
  };
};

let next_assert = x => {
  switch (x) {
  | (a, b) =>
    let n = (a, b + 1);
    (b + 1, n);
  };
};
