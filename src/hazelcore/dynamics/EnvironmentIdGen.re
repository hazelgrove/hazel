[@deriving sexp]
type t = EnvironmentId.t;

let init = EnvironmentId.init;

let next = id => {
  let id = id + 1;
  (id, id);
};
