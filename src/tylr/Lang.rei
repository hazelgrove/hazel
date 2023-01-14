module Sort: {
  type t;
  let compare: (t, t) => int;
  let root: t;
};

let t: list((Sort.t, list((Gram.t(Sort.t), Assoc.t))));
