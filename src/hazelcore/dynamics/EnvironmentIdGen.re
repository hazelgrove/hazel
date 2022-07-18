[@deriving sexp]
type t = EnvironmentId.t;

let init = EnvironmentId.init;

let next = id => {
  let id = EnvironmentId.next(id);
  (id, id);
};
