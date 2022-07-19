[@deriving sexp]
type t = Label.t;

let init = Label.init;

let next = l_gen => {
  let l_gen' = Label.next(l_gen);
  (l_gen, l_gen');
};
