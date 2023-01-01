module type S = {
  type t;
  let compare: (t, t) => int;
  let root: t;
};
